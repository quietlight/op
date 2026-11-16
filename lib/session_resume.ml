let ( let* ) = Result.bind
let err fmt = Fmt.kstr (fun s -> Error s) fmt

(* The dead end this message has to open up: the caller wants to continue, the
   session has several tips, and only they can say which. So spell the branches
   out as commands to paste rather than naming a listing to go read — doubly so
   because a tip is often a tool result, which --list messages never shows. *)
let err_session_branches ~session_id ~path n =
  let tips =
    match Session_list.list_branches ~path () with Ok ts -> ts | Error _ -> []
  in
  let line (t : Session_list.branch_tip) =
    let described =
      if t.preview = "" then "" else Fmt.str "   # %s %s" t.role t.preview
    in
    Fmt.str "\n  op --sessionId %s --parentId %s%s" session_id t.tip described
  in
  err
    "session %s has %d branches; continue one of them with --parentId:%s\n\n\
     (op --list branches --sessionId %s lists these; op --list messages shows \
     every entry, '*' marking where the branches split)"
    session_id n
    (String.concat "" (List.map line tips))
    session_id

let requested_leaf ~session_id ~path tree parent_id =
  match parent_id with
  | Some p -> Ok p
  | None -> (
      match Session_read.leaf_ids tree with
      | [ id ] -> Ok id
      | [] -> Error "session has no entries to resume"
      | ids -> err_session_branches ~session_id ~path (List.length ids))

let complete_leaf tree leaf =
  match Session_read.resolve_branch_point tree ~leaf with
  | Ok (id, false) -> Ok (id, None)
  | Ok (id, true) -> Ok (id, Some (leaf, id))
  | Error e -> Error e

let path_of ~cwd ~session_id =
  let* root = Session.default_root () in
  let dir = Session.dir_for ~root ~cwd in
  Session_read.file_for_session ~dir ~session_id

(* Everything a resume decides before a byte is written, from one parse of the
   file: which entry to continue from (the requested one, or the session's
   single leaf), corrected to the nearest complete turn, and the history that
   leads to it. *)
let resolve ~cwd ~session_id ~parent_id =
  let* path = path_of ~cwd ~session_id in
  let* tree = Session_read.load path in
  let* leaf = requested_leaf ~session_id ~path tree parent_id in
  let* leaf, branch_correction = complete_leaf tree leaf in
  let* history = Session_read.messages tree ~leaf () in
  Ok (path, leaf, branch_correction, history)

(* The read-only half of [resume]: the same leaf selection and branch-point
   correction, stopping short of reopening the file for appending. A leaf
   correction is silent here because nothing is written to the branch. *)
let load ~cwd ~session_id ~parent_id () =
  let* path, _, _, history = resolve ~cwd ~session_id ~parent_id in
  Ok (path, history)

let resume ?sink ~cwd ~session_id ~parent_id ~(provider : Config.provider)
    ~model () =
  let* path, leaf, branch_correction, history =
    resolve ~cwd ~session_id ~parent_id
  in
  let* session = Session.resume ?sink ~path ~leaf () in
  Session.append_model_change session ~provider:provider.name ~model;
  Ok (session, history, branch_correction)
