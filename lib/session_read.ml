module U = Yojson.Safe.Util

let ( let* ) = Result.bind

let err_malformed_message msg =
  Fmt.kstr (fun s -> Error s) "malformed message entry: %s" msg

let err_invalid_json msg =
  Fmt.kstr (fun s -> Error s) "invalid JSON in session: %s" msg

let err_leaf_not_found id =
  Fmt.kstr (fun s -> Error s) "leaf id %s not found in session" id

let err_parent_not_found pid =
  Fmt.kstr (fun s -> Error s) "parentId %s not found in session" pid

let err_session_not_found ~session_id ~dir =
  Fmt.kstr (fun s -> Error s) "session %s not found in %s" session_id dir

let err_parent_cycle () =
  Error
    "parentId chain forms a cycle (corrupt session file); no path to the root \
     exists"

let parse_entries path =
  let* content = Util.read_file path in
  String.split_on_char '\n' content
  |> List.filter (fun l -> String.trim l <> "")
  |> List.fold_left
       (fun acc line ->
         let* acc = acc in
         match Yojson.Safe.from_string line with
         | json -> Ok (json :: acc)
         | exception Yojson.Json_error msg -> err_invalid_json msg)
       (Ok [])
  |> Result.map List.rev

let is_header e = U.member "type" e = `String "session"

(* Keep header separate: its id is the session UUID, not a parentId target. *)
type tree = {
  header : Yojson.Safe.t option;
  entries : Yojson.Safe.t list;
  by_id : (string, Yojson.Safe.t) Hashtbl.t;
}

let load path =
  let* all = parse_entries path in
  let entries = List.filter (fun e -> not (is_header e)) all in
  let by_id = Hashtbl.create 64 in
  List.iter
    (fun e ->
      match U.member "id" e with
      | `String id -> Hashtbl.replace by_id id e
      | _ -> ())
    entries;
  Ok { header = List.find_opt is_header all; entries; by_id }

let header t = t.header
let entries t = t.entries
let entry t id = Hashtbl.find_opt t.by_id id

let text_of_content = function
  | `String s -> s
  | content ->
      U.to_list content
      |> List.filter_map (fun block ->
          match U.member "type" block with
          | `String "text" -> Some (U.to_string (U.member "text" block))
          | _ -> None)
      |> String.concat ""

let content_blocks_of = function
  | `String s -> [ Llm.Text s ]
  | content ->
      U.to_list content
      |> List.filter_map (fun block ->
          match U.member "type" block with
          | `String "text" ->
              Some (Llm.Text (U.to_string (U.member "text" block)))
          | `String "image" ->
              let media_type =
                match U.member "mimeType" block with
                | `String s -> s
                | _ -> "application/octet-stream"
              in
              let data =
                match U.member "data" block with `String s -> s | _ -> ""
              in
              Some (Llm.Image { media_type; data })
          | _ -> None)

(* Leading text blocks become [User.text]; remaining blocks preserve order. *)
let is_text = function Llm.Text _ -> true | _ -> false

let user_of_content content =
  let blocks = content_blocks_of content in
  let rec split acc = function
    | (Llm.Text _ as b) :: rest when List.for_all is_text acc ->
        split (b :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let text_blocks, attachments = split [] blocks in
  let text =
    text_blocks
    |> List.filter_map (function Llm.Text s -> Some s | _ -> None)
    |> String.concat ""
  in
  Llm.User { text; blocks = attachments }

(* Treat an all-zero usage object (emitted when provider reported none) as
   [None] so the context gauge does not show a spurious 0 on resume. *)
let usage_of_json json =
  let field name = match U.member name json with `Int n -> n | _ -> 0 in
  let input = field "input"
  and output = field "output"
  and cache_read = field "cacheRead"
  and cache_creation = field "cacheWrite" in
  if input = 0 && output = 0 && cache_read = 0 && cache_creation = 0 then None
  else Some { Llm.input; output; cache_read; cache_creation }

let thinking_of_content content =
  U.to_list content
  |> List.find_map (fun block ->
      match U.member "type" block with
      | `String "thinking" -> (
          match U.member "redacted" block with
          | `Bool true ->
              Some (Llm.Redacted (U.to_string (U.member "data" block)))
          | _ ->
              let text = U.to_string (U.member "thinking" block) in
              let signature =
                match U.member "thinkingSignature" block with
                | `String s -> Some s
                | _ -> None
              in
              Some (Llm.Reasoning { text; signature }))
      | _ -> None)

let message_of_json msg =
  match U.member "role" msg with
  | `String "user" -> Ok (Some (user_of_content (U.member "content" msg)))
  | `String "assistant" ->
      let content = U.member "content" msg in
      let text = text_of_content content in
      let thinking = thinking_of_content content in
      let tool_calls =
        U.to_list content
        |> List.filter_map (fun block ->
            match U.member "type" block with
            | `String "toolCall" ->
                Some
                  Llm.
                    {
                      id = U.to_string (U.member "id" block);
                      name = U.to_string (U.member "name" block);
                      arguments = U.member "arguments" block;
                    }
            | _ -> None)
      in
      Ok
        (Some
           (Llm.Assistant
              {
                text;
                thinking;
                tool_calls;
                usage = usage_of_json (U.member "usage" msg);
              }))
  | `String "toolResult" ->
      Ok
        (Some
           (Llm.Tool
              {
                id = U.to_string (U.member "toolCallId" msg);
                name = U.to_string (U.member "toolName" msg);
                content = text_of_content (U.member "content" msg);
                is_error = U.to_bool (U.member "isError" msg);
                details =
                  (match U.member "details" msg with
                  | `Null -> None
                  | d -> Some d);
              }))
  | _ -> Ok None

let entry_message entry =
  match U.member "type" entry with
  | `String "message" -> (
      try message_of_json (U.member "message" entry)
      with U.Type_error (msg, _) -> err_malformed_message msg)
  | _ -> Ok None

(* Walk [parentId] links to the root; [fuel] breaks cycles in corrupt files. *)
let rec walk_parents by_id ~fuel entry acc =
  if fuel <= 0 then err_parent_cycle ()
  else
    let* msg = entry_message entry in
    let acc = match msg with Some m -> m :: acc | None -> acc in
    match U.member "parentId" entry with
    | `Null -> Ok acc
    | `String pid -> (
        match Hashtbl.find_opt by_id pid with
        | Some parent -> walk_parents by_id ~fuel:(fuel - 1) parent acc
        | None -> err_parent_not_found pid)
    | _ -> Ok acc

let messages t ?leaf () =
  let* start =
    match leaf with
    | Some id -> (
        match entry t id with Some e -> Ok e | None -> err_leaf_not_found id)
    | None -> (
        match List.rev t.entries with
        | last :: _ -> Ok last
        | [] -> Error "session has no entries")
  in
  walk_parents t.by_id ~fuel:(List.length t.entries) start []

let is_incomplete_branch_point entry =
  match U.member "type" entry with
  | `String "message" -> (
      let msg = U.member "message" entry in
      match U.member "role" msg with
      | `String "assistant" -> (
          match U.member "content" msg with
          | `List blocks ->
              List.exists
                (fun block -> U.member "type" block = `String "toolCall")
                blocks
          | _ -> false)
      | _ -> false)
  | _ -> false

let resolve_branch_point t ~leaf =
  let rec walk ~fuel id =
    if fuel <= 0 then err_parent_cycle ()
    else
      match entry t id with
      | None -> err_leaf_not_found id
      | Some entry when not (is_incomplete_branch_point entry) ->
          Ok (id, id <> leaf)
      | Some entry -> (
          match U.member "parentId" entry with
          | `String pid when Hashtbl.mem t.by_id pid ->
              walk ~fuel:(fuel - 1) pid
          | `String pid -> err_parent_not_found pid
          | _ -> Ok (id, id <> leaf))
  in
  walk ~fuel:(List.length t.entries) leaf

let entry_ids t =
  List.filter_map
    (fun e -> match U.member "id" e with `String id -> Some id | _ -> None)
    t.entries

let file_for_session ~dir ~session_id =
  let suffix = "_" ^ session_id ^ ".jsonl" in
  let files =
    match Sys.readdir dir with
    | files ->
        Array.to_list files
        |> List.filter (fun f -> Filename.check_suffix f suffix)
    | exception Sys_error _ -> []
  in
  match files with
  | f :: _ -> Ok (Filename.concat dir f)
  | [] -> err_session_not_found ~session_id ~dir

let leaf_ids t =
  let parents = Hashtbl.create 64 in
  List.iter
    (fun e ->
      match U.member "parentId" e with
      | `String pid -> Hashtbl.replace parents pid ()
      | _ -> ())
    t.entries;
  List.filter (fun id -> not (Hashtbl.mem parents id)) (entry_ids t)
