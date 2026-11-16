(** The [op --list] views of pi-compatible session logs: one row per session,
    per message, or per branch tip. Built on {!Session_read}'s parsed tree. *)

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
    {!Session_read.leaf_ids}, in file order, each described by the nearest
    conversational entry at or above it. A tip is often a tool result or a
    metadata entry (a run killed mid-turn ends on one), so {!field-role} and
    {!field-preview} come from that ancestor rather than the tip itself; both
    are [""] when the branch holds no user or assistant message. A linear
    session yields one row, which is why {!field-tip} is exactly what a caller
    passes to [--parentId] to continue that branch. Errors on an unreadable or
    malformed file. *)
