(* The [op --list] views of session files: one row per session, per message,
   or per branch tip. See session_list.mli. *)

module U = Yojson.Safe.Util

let ( let* ) = Result.bind

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
                | `String s -> Util.human_bytes (String.length s)
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
          (Session_read.text_of_content
             (U.member "content" (U.member "message" e)))
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
  let* tree = Session_read.load path in
  let entries = Session_read.entries tree in
  match Session_read.header tree with
  | Some header ->
      let id = U.to_string (U.member "id" header) in
      let timestamp = U.to_string (U.member "timestamp" header) in
      Ok
        {
          id;
          timestamp;
          summary = first_user_summary entries;
          count = message_count entries;
        }
  | None -> Error "no session header"

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
  let* tree = Session_read.load path in
  let entries = Session_read.entries tree in
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
   human recognises; the tree's id index makes each hop O(1), and the entry count
   bounds the walk so a corrupt parent cycle cannot spin. *)
let describe_tip tree ~fuel id =
  let rec walk fuel id =
    if fuel <= 0 then None
    else
      match Session_read.entry tree id with
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
  let* tree = Session_read.load path in
  let fuel = List.length (Session_read.entries tree) in
  Ok
    (List.map
       (fun id ->
         let role, preview =
           Option.value (describe_tip tree ~fuel id) ~default:("", "")
         in
         let timestamp =
           match
             Option.map (U.member "timestamp") (Session_read.entry tree id)
           with
           | Some (`String s) -> s
           | _ -> ""
         in
         { tip = id; role; preview; timestamp })
       (Session_read.leaf_ids tree))
