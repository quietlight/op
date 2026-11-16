(** Reading pi-compatible session logs.

    The writer half is {!module:Session}; this module parses existing session
    jsonl files back into {!Llm.message} histories and branch-point resolutions.
    The [op --list] views are {!Session_list}. *)

type tree
(** A session file parsed once: its header and its tree entries, indexed by id.
    Every reader below works on a [tree], so one operation — resolving a resume,
    say — reads and parses the file a single time and reports a malformed file
    from one place. *)

val load : string -> (tree, string) result
(** [load path] reads and parses the session file at [path]. Errors on an
    unreadable file or a malformed JSON line. *)

val header : tree -> Yojson.Safe.t option
(** [header t] is the session header line, when the file has one. *)

val entries : tree -> Yojson.Safe.t list
(** [entries t] is every tree entry (the header excluded), in file order. *)

val entry : tree -> string -> Yojson.Safe.t option
(** [entry t id] is the tree entry whose id is [id]. *)

val entry_ids : tree -> string list
(** [entry_ids t] is every tree-entry id (the header's session id is excluded),
    in file order. Used by {!Session.resume} to check a requested leaf exists
    and to seed its id-uniqueness set. *)

val messages : tree -> ?leaf:string -> unit -> (Llm.message list, string) result
(** [messages t ?leaf ()] is the session as an [Llm.message] list in
    root-to-leaf order. The starting entry is the one whose id equals [leaf], or
    the last entry in the file when [leaf] is omitted; the chain is rebuilt by
    walking [parentId] links to the root. Only [message] entries yield messages
    (other entry types are skipped); the system prompt is never stored. Errors
    on an unknown [leaf], or a [parentId] referencing a missing entry. *)

val resolve_branch_point : tree -> leaf:string -> (string * bool, string) result
(** [resolve_branch_point t ~leaf] returns [(chosen_id, adjusted)], where
    [chosen_id] is [leaf] unless it is an assistant turn with unanswered tool
    calls. In that case, it walks [parentId]-ward to the nearest complete branch
    point and sets [adjusted] to [true]. Errors on an unknown [leaf] or a
    missing parent. *)

val leaf_ids : tree -> string list
(** [leaf_ids t] Ids of the tree's leaf entries — those that are no other
    entry's parent — in file order. A linear session yields exactly one (its
    tail, the natural place to continue from); a forked session yields one per
    branch tip, in which case continuing is ambiguous and a [--parentId] must
    pick between them. A session with no tree entries yields the empty list. *)

val text_of_content : Yojson.Safe.t -> string
(** [text_of_content content] concatenates the text blocks of a pi message
    [content] (a block list, or a plain string). Image blocks contribute
    nothing. *)

val file_for_session :
  dir:string -> session_id:string -> (string, string) result
(** [file_for_session ~dir ~session_id] Path of the session file in [dir] whose
    name ends in [_<session_id>.jsonl], or an error when none matches. *)
