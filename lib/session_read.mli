(** Reading pi-compatible session logs (writer: {!Session}; [--list] views:
    {!Session_list}). *)

type tree
(** A parsed session file: header and tree entries indexed by id. *)

val load : string -> (tree, string) result
(** [load path] parses the session JSONL file at [path]. *)

val header : tree -> Yojson.Safe.t option
(** [header t] is the session header entry, if present. *)

val entries : tree -> Yojson.Safe.t list
(** [entries t] returns tree entries (excluding the header) in file order. *)

val entry : tree -> string -> Yojson.Safe.t option
(** [entry t id] looks up a tree entry by id. *)

val entry_ids : tree -> string list
(** [entry_ids t] returns tree-entry ids in file order (excluding the header).
*)

val messages : tree -> ?leaf:string -> unit -> (Llm.message list, string) result
(** [messages t ?leaf ()] reconstructs root-to-leaf {!Llm.message} history by
    walking [parentId] links from [leaf] (default: last entry). Skips
    non-message entries. *)

val resolve_branch_point : tree -> leaf:string -> (string * bool, string) result
(** [resolve_branch_point t ~leaf] returns [(chosen_id, adjusted)]. If [leaf] is
    an assistant turn with unanswered tool calls, walks [parentId]-ward to the
    nearest complete branch point and sets [adjusted = true]. *)

val leaf_ids : tree -> string list
(** [leaf_ids t] returns ids of entries with no children, in file order. *)

val text_of_content : Yojson.Safe.t -> string
(** [text_of_content content] concatenates ["text"] blocks (or returns a plain
    string); ignores images. *)

val file_for_session :
  dir:string -> session_id:string -> (string, string) result
(** [file_for_session ~dir ~session_id] locates [*_<session_id>.jsonl] in [dir].
*)
