(** Reading pi-compatible session logs.

    The writer half is {!module:Session}; this module parses existing session
    jsonl files back into {!Llm.message} histories, branch-point resolutions and
    listing summaries. Every reader is built on one read+parse pass, so
    resolving a resume does not re-read the same file repeatedly. *)

val entry_ids : string -> (string list, string) result
(** [entry_ids path] is every tree-entry id in the session file (the header's
    session id is excluded), in file order. Used by {!Session.resume} to check a
    requested leaf exists and to seed its id-uniqueness set. Errors on an
    unreadable file or a malformed JSON line. *)

val load_messages :
  path:string -> ?leaf:string -> unit -> (Llm.message list, string) result
(** [load_messages ~path ?leaf ()] reads a pi-format session jsonl file as an
    [Llm.message] list in root-to-leaf order. The starting entry is the one
    whose id equals [leaf], or the last entry in the file when [leaf] is
    omitted; the chain is rebuilt by walking [parentId] links to the root. Only
    [message] entries yield messages (other entry types are skipped); the system
    prompt is never stored. Errors on an unreadable file, a non-JSON line, an
    unknown [leaf], or a [parentId] referencing a missing entry. *)

val resolve_branch_point :
  path:string -> leaf:string -> (string * bool, string) result
(** [resolve_branch_point ~path ~leaf] returns [(chosen_id, adjusted)], where
    [chosen_id] is [leaf] unless it is an assistant turn with unanswered tool
    calls. In that case, it walks [parentId]-ward to the nearest complete branch
    point and sets [adjusted] to [true]. Errors on an unreadable file, non-JSON
    line, unknown [leaf], or missing parent. *)

val file_for_session :
  dir:string -> session_id:string -> (string, string) result
(** [file_for_session ~dir ~session_id] Path of the session file in [dir] whose
    name ends in [_<session_id>.jsonl], or an error when none matches. *)

val leaf_ids : path:string -> string list
(** [leaf_ids ~path] Ids of the tree's leaf entries — those that are no other
    entry's parent — in file order. A linear session yields exactly one (its
    tail, the natural place to continue from); a forked session yields one per
    branch tip, in which case continuing is ambiguous and a [--parentId] must
    pick between them. A session with no tree entries (or an unreadable file)
    yields the empty list. *)

type summary = {
  id : string;
  timestamp : string;
  summary : string;
  count : int;  (** number of conversational message entries *)
}

val list_sessions : dir:string -> unit -> (summary list, string) result
(** [list_sessions ~dir ()] Summaries of every session in [dir], oldest first.
    {!field-summary} is the first sentence of the session's first user message
    and {!field-count} its message volume. A missing directory yields the empty
    list. The caller applies [--limit] and reports any tail it drops. *)

type entry_summary = {
  id : string;
  parent_id : string option;
  role : string;  (** "user" | "assistant" *)
  preview : string;
  is_fork : bool;  (** the entry has more than one child (a branch point) *)
}

val list_entries : path:string -> unit -> (entry_summary list, string) result
(** [list_entries ~path ()] User and assistant messages in the session, oldest
    first; tool results and metadata entries are skipped. {!field-preview} is a
    truncated text snippet, or a summary of the turn's tool calls when it
    carried no text. The caller applies [--limit] and reports any tail it drops.
*)

type branch_tip = {
  tip : string;  (** leaf entry id — what [--parentId] takes to continue here *)
  role : string;  (** "user" | "assistant", or "" if the branch has neither *)
  preview : string;
  timestamp : string;  (** of the tip entry, not of the described message *)
}

val list_branches : path:string -> unit -> (branch_tip list, string) result
(** [list_branches ~path ()] One row per branch tip — the same ids as
    {!leaf_ids}, in file order, each described by the nearest conversational
    entry at or above it. A tip is often a tool result or a metadata entry (a
    run killed mid-turn ends on one), so {!field-role} and {!field-preview} come
    from that ancestor rather than the tip itself; both are [""] when the branch
    holds no user or assistant message. A linear session yields one row, which
    is why {!field-tip} is exactly what a caller passes to [--parentId] to
    continue that branch. Errors on an unreadable or malformed file. *)
