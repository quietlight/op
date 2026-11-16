(* Reading and summarising pi-compatible session logs. The writer half lives in
   session.ml; this module only parses existing files back into messages,
   branch-point resolutions, and listing summaries.

   Every reader starts from one [parse_entries] pass (read + line-split +
   per-line JSON parse), so resolving a resume no longer re-reads the same file
   several times. Derived views (tree entries, ids, leaves, summaries) are pure
   functions over that parsed list. *)

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

(* The single read+parse pass every other reader is built on. Parses each
   non-empty line as JSON in file order, including the session header as the
   first entry; views that want only tree entries drop it by type. Errors on an
   unreadable file or a malformed JSON line. *)
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

(* A parsed session file. [entries] are the tree entries in file order: the
   header is held apart because it carries an id (the session id, not a valid
   parentId target) and no parent link. [by_id] follows parentId links in O(1). *)
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

(* Concatenate the "text" fields of content blocks of type "text". pi also
   allows user content to be a plain string. Image blocks contribute no text. *)
let text_of_content = function
  | `String s -> s
  | content ->
      U.to_list content
      |> List.filter_map (fun block ->
          match U.member "type" block with
          | `String "text" -> Some (U.to_string (U.member "text" block))
          | _ -> None)
      |> String.concat ""

(* Parse a pi content list into neutral [content_block]s, preserving order.
   A plain-string content becomes a single [Text]. Image blocks carry the pi
   [mimeType]/[data] pair (docs/pi-session-format.md); round-tripping them is a
   correctness requirement so a resumed session replays the same history. *)
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

(* Split parsed blocks into (text-body, attachment-blocks): a leading run of
   text blocks is joined into the [User.text] body, the rest (images, and any
   text after an image) stay as [blocks]. Mirrors how the adapters emit a text
   body followed by attachments. *)
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

(* Read an assistant message's [usage] back. op's writer always emits the
   object, zeroed when the provider reported nothing, so an all-zero reading is
   "no usage" rather than "a free turn" — reporting it as [None] keeps the
   context gauge from showing a spurious 0 for a resumed branch. *)
let usage_of_json json =
  let field name = match U.member name json with `Int n -> n | _ -> 0 in
  let input = field "input"
  and output = field "output"
  and cache_read = field "cacheRead"
  and cache_creation = field "cacheWrite" in
  if input = 0 && output = 0 && cache_read = 0 && cache_creation = 0 then None
  else Some { Llm.input; output; cache_read; cache_creation }

(* The first thinking block of an assistant turn's content, if any. Anthropic
   replays these verbatim, so the signature must survive the round trip. *)
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

(* Turn a pi message object into an Llm.message; None for roles op ignores. *)
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
                (* Read tool-specific metadata back verbatim for pi-format
                   parity and compatibility with foreign and historical logs. *)
                details =
                  (match U.member "details" msg with
                  | `Null -> None
                  | d -> Some d);
              }))
  | _ -> Ok None

(* An entry produces a message only when its type is "message"; every other
   entry type (model_change, thinking_level_change, ...) is skipped. *)
let entry_message entry =
  match U.member "type" entry with
  | `String "message" -> (
      try message_of_json (U.member "message" entry)
      with U.Type_error (msg, _) -> err_malformed_message msg)
  | _ -> Ok None

(* Walk parentId links from the start entry to the root. [fuel] bounds the walk
   so a corrupt parent cycle (a.parentId = b, b.parentId = a — hand-edited or
   truncated jsonl) cannot spin forever accumulating messages; the caller seeds
   it with the entry count, which no acyclic chain can exceed. Same guard
   {!describe_tip} already carries. *)
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
  (* Same cycle bound as {!walk_parents}: this walk also follows parentId and
     would spin on a corrupt chain. *)
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

(* --- resuming ------------------------------------------------------------ *)

(* Every tree-entry id, so the writer can check a requested leaf exists and
   seed its id-uniqueness set on resume. *)
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

(* The leaf entries of the tree: entries that are no other entry's parent. A
   linear session has exactly one leaf (its tail); a forked session has one per
   branch tip, so continuing without a --parentId is ambiguous. Returned in
   file (write) order. *)
let leaf_ids t =
  let parents = Hashtbl.create 64 in
  List.iter
    (fun e ->
      match U.member "parentId" e with
      | `String pid -> Hashtbl.replace parents pid ()
      | _ -> ())
    t.entries;
  List.filter (fun id -> not (Hashtbl.mem parents id)) (entry_ids t)
