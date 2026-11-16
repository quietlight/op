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

(* Tree entries only: the header carries an id (the session id, not a valid
   parentId target) but no parent link, so it is dropped here. *)
let tree_entries path =
  let* entries = parse_entries path in
  Ok (List.filter (fun e -> not (is_header e)) entries)

(* Index entries by their id so parentId links can be followed in O(1). Shared
   by [load_messages] and [resolve_branch_point], which both walk the tree. *)
let index_by_id entries =
  let by_id = Hashtbl.create 64 in
  List.iter
    (fun e ->
      match U.member "id" e with
      | `String id -> Hashtbl.replace by_id id e
      | _ -> ())
    entries;
  by_id

(* Concatenate the "text" fields of content blocks of type "text". pi also
   allows user content to be a plain string. Image blocks contribute no text. *)
let concat_text = function
  | `String s -> s
  | content ->
      U.to_list content
      |> List.filter_map (fun block ->
          match U.member "type" block with
          | `String "text" -> Some (U.to_string (U.member "text" block))
          | _ -> None)
      |> String.concat ""

(* Human-readable size for an image placeholder, e.g. "312 KB". [n] is the
   base64 byte count; the decoded size is ~3/4 of that. *)
let human_bytes n =
  if n < 1024 then Fmt.str "%d B" n
  else if n < 1024 * 1024 then Fmt.str "%d KB" (n / 1024)
  else Fmt.str "%d MB" (n / (1024 * 1024))

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

(* Preview text for [op --list messages]: like [concat_text] but images are
   shown as an elided placeholder ([image image/png 312 KB]) rather than
   dropped or dumped as base64. *)
let preview_text = function
  | `String s -> s
  | content ->
      U.to_list content
      |> List.filter_map (fun block ->
          match U.member "type" block with
          | `String "text" -> Some (U.to_string (U.member "text" block))
          | `String "image" ->
              let media_type =
                match U.member "mimeType" block with
                | `String s -> s
                | _ -> "image"
              in
              let size =
                match U.member "data" block with
                | `String s -> human_bytes (String.length s)
                | _ -> "?"
              in
              Fmt.kstr (fun s -> Some s) "[image %s %s]" media_type size
          | _ -> None)
      |> String.concat " "

(* [--context FILE] prepends a `<context file="…">…</context>` block per file to
   the user's first message (see context.ml). That is op's boilerplate, not what
   the user typed, and in a project that always passes the same CLAUDE.md it
   makes every session's summary read [<context file=CLAUDE…] — identical rows
   where the listing exists to tell sessions apart. So skip the leading blocks
   and describe the prompt underneath. Unterminated or non-leading blocks are
   left alone: this only trims what op is known to have prepended. *)
let strip_context_blocks text =
  let close = "</context>" in
  let n = String.length close in
  let rec strip s =
    let s = String.trim s in
    if not (String.starts_with ~prefix:"<context file=" s) then s
    else
      match Util.substring ~start:0 ~needle:close s with
      | None -> s
      | Some i -> strip (String.sub s (i + n) (String.length s - i - n))
  in
  (* A message that is *only* context still has to say something. *)
  match strip text with
  | "" -> text
  | stripped -> stripped

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
      let text = concat_text content in
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
                content = concat_text (U.member "content" msg);
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

let load_messages ~path ?leaf () =
  let* entries = tree_entries path in
  let by_id = index_by_id entries in
  let* start =
    match leaf with
    | Some id -> (
        match Hashtbl.find_opt by_id id with
        | Some e -> Ok e
        | None -> err_leaf_not_found id)
    | None -> (
        match List.rev entries with
        | last :: _ -> Ok last
        | [] -> Error "session has no entries")
  in
  walk_parents by_id ~fuel:(List.length entries) start []

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

let resolve_branch_point ~path ~leaf =
  let* entries = tree_entries path in
  let by_id = index_by_id entries in
  (* Same cycle bound as {!walk_parents}: this walk also follows parentId and
     would spin on a corrupt chain. *)
  let rec walk ~fuel id =
    if fuel <= 0 then err_parent_cycle ()
    else
      match Hashtbl.find_opt by_id id with
      | None -> err_leaf_not_found id
      | Some entry ->
          if is_incomplete_branch_point entry then
            match U.member "parentId" entry with
            | `String pid -> (
                match Hashtbl.find_opt by_id pid with
                | Some _ -> walk ~fuel:(fuel - 1) pid
                | None -> err_parent_not_found pid)
            | `Null -> Ok (id, id <> leaf)
            | _ -> Ok (id, id <> leaf)
          else Ok (id, id <> leaf)
  in
  walk ~fuel:(List.length entries) leaf

(* --- resuming ------------------------------------------------------------ *)

(* Every tree-entry id in the file (header excluded by type), so the writer can
   check a requested leaf exists and seed its id-uniqueness set on resume. *)
let entry_ids path =
  let* entries = parse_entries path in
  Ok
    (List.filter_map
       (fun e ->
         match (U.member "type" e, U.member "id" e) with
         | `String "session", _ -> None
         | _, `String id -> Some id
         | _ -> None)
       entries)

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

(* The leaf entries of the tree: tree entries that are no other entry's parent.
   A linear session has exactly one leaf (its tail); a forked session has one
   per branch tip, so continuing without a --parentId is ambiguous. Returned in
   file (write) order; the session header is excluded by type. A malformed or
   unreadable file yields the empty list (the caller then reports "no entries to
   resume"). *)
let leaf_ids ~path =
  match parse_entries path with
  | Error _ -> []
  | Ok entries ->
      let parents = Hashtbl.create 64 in
      List.iter
        (fun e ->
          match U.member "parentId" e with
          | `String pid -> Hashtbl.replace parents pid ()
          | _ -> ())
        entries;
      List.filter_map
        (fun e ->
          match (U.member "type" e, U.member "id" e) with
          | `String "session", _ -> None
          | _, `String id when not (Hashtbl.mem parents id) -> Some id
          | _ -> None)
        entries

(* --- listing ------------------------------------------------------------- *)

type summary = {
  id : string;
  timestamp : string;
  summary : string;
  count : int;
}

(* Whichever comes first: a session row is one line of a table, so a newline has
   to end it even when a sentence carries on past one (a URL's dot used to win,
   printing the line break into the listing). *)
let first_sentence s =
  let cut_at =
    match (String.index_opt s '.', String.index_opt s '\n') with
    | Some i, Some j -> Some (min i j)
    | Some i, None -> Some i
    | None, j -> j
  in
  match cut_at with Some i -> String.sub s 0 i | None -> s

let jsonl_files dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".jsonl")
    |> List.sort compare
  with Sys_error _ -> []

let is_user_message e =
  U.member "type" e = `String "message"
  && U.member "role" (U.member "message" e) = `String "user"

let first_user_summary entries =
  match List.find_opt is_user_message entries with
  | Some e ->
      let text =
        strip_context_blocks
          (concat_text (U.member "content" (U.member "message" e)))
      in
      Util.truncate ~max_bytes:80 (first_sentence text)
  | None -> ""

(* Number of conversational message entries (user/assistant/toolResult) — a
   rough size signal so similar-looking sessions can be told apart. *)
let message_count entries =
  List.fold_left
    (fun n e -> if U.member "type" e = `String "message" then n + 1 else n)
    0 entries

let summary_of_file path =
  let* entries = parse_entries path in
  match entries with
  | header :: _ ->
      let id = U.to_string (U.member "id" header) in
      let timestamp = U.to_string (U.member "timestamp" header) in
      Ok
        {
          id;
          timestamp;
          summary = first_user_summary entries;
          count = message_count entries;
        }
  | [] -> Error "empty file"

let list_sessions ~dir () =
  let oks =
    jsonl_files dir
    |> List.filter_map (fun f ->
        match summary_of_file (Filename.concat dir f) with
        | Ok s -> Some s
        | Error _ -> None)
  in
  Ok oks

type entry_summary = {
  id : string;
  parent_id : string option;
  role : string;
  preview : string;
  is_fork : bool;
}

(* Summarise an assistant turn's tool calls, used as the preview when the turn
   carried no text (a tool-only turn). e.g. "→ edit lib/session.ml, nu ls". *)
let tool_calls_hint content =
  match content with
  | `List blocks -> (
      let hint block =
        match U.member "type" block with
        | `String "toolCall" ->
            let name =
              match U.member "name" block with `String s -> s | _ -> "?"
            in
            let detail =
              List.find_map
                (fun field ->
                  match U.member field (U.member "arguments" block) with
                  | `String s -> Some s
                  | _ -> None)
                [ "path"; "file_path"; "command"; "pattern" ]
            in
            Some
              (match detail with
              | Some d -> Fmt.str "%s %s" name d
              | None -> name)
        | _ -> None
      in
      match List.filter_map hint blocks with
      | [] -> ""
      | hints -> Util.truncate ~max_bytes:80 ("→ " ^ String.concat ", " hints))
  | _ -> ""

(* Count children per entry id (across all entry types) so an entry with more
   than one child can be flagged as a fork point. *)
let child_counts entries =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun e ->
      match U.member "parentId" e with
      | `String pid ->
          Hashtbl.replace tbl pid
            (1 + Option.value ~default:0 (Hashtbl.find_opt tbl pid))
      | _ -> ())
    entries;
  tbl

(* The [(role, preview)] pair a listing shows for a conversational entry;
   [None] for tool results and metadata entries, which have nothing to show. *)
let role_and_preview entry =
  match U.member "type" entry with
  | `String "message" -> (
      let msg = U.member "message" entry in
      match U.member "role" msg with
      | `String (("user" | "assistant") as role) ->
          let content = U.member "content" msg in
          let text = strip_context_blocks (preview_text content) in
          let preview =
            if String.trim text <> "" then Util.truncate ~max_bytes:80 text
            else tool_calls_hint content
          in
          Some (role, preview)
      | _ -> None)
  | _ -> None

(* A user/assistant entry as an [entry_summary]; [None] for other entries. *)
let entry_summary_of children entry =
  match role_and_preview entry with
  | None -> None
  | Some (role, preview) ->
      let id = match U.member "id" entry with `String s -> s | _ -> "" in
      let parent_id =
        match U.member "parentId" entry with `String s -> Some s | _ -> None
      in
      let is_fork =
        Option.value ~default:0 (Hashtbl.find_opt children id) > 1
      in
      Some { id; parent_id; role; preview; is_fork }

let list_entries ~path () =
  let* entries = parse_entries path in
  let children = child_counts entries in
  Ok (List.filter_map (entry_summary_of children) entries)

type branch_tip = {
  tip : string;
  role : string;
  preview : string;
  timestamp : string;
}

(* A tip is whatever entry the branch ends on — routinely a tool result or a
   metadata entry, which carry no text of their own. Walk [parentId]-ward to
   the nearest conversational entry so the tip is described by something a
   human recognises; [index_by_id] makes each hop O(1), and the entry count
   bounds the walk so a corrupt parent cycle cannot spin. *)
let describe_tip by_id ~fuel id =
  let rec walk fuel id =
    if fuel <= 0 then None
    else
      match Hashtbl.find_opt by_id id with
      | None -> None
      | Some entry -> (
          match role_and_preview entry with
          | Some _ as found -> found
          | None -> (
              match U.member "parentId" entry with
              | `String pid -> walk (fuel - 1) pid
              | _ -> None))
  in
  walk fuel id

let list_branches ~path () =
  let* entries = parse_entries path in
  let by_id = index_by_id entries in
  let fuel = List.length entries in
  let children = child_counts entries in
  let is_leaf id = not (Hashtbl.mem children id) in
  Ok
    (List.filter_map
       (fun entry ->
         match (is_header entry, U.member "id" entry) with
         | false, `String id when is_leaf id ->
             let role, preview =
               Option.value (describe_tip by_id ~fuel id) ~default:("", "")
             in
             let timestamp =
               match U.member "timestamp" entry with `String s -> s | _ -> ""
             in
             Some { tip = id; role; preview; timestamp }
         | _ -> None)
       entries)
