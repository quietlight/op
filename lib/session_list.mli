(** [op --list] views over parsed session trees ({!Session_read}). *)

type summary = {
  id : string;
  timestamp : string;
  summary : string;
  count : int;  (** Number of message entries. *)
}

val list_sessions : dir:string -> unit -> (summary list, string) result
(** [list_sessions ~dir ()] summarizes all sessions in [dir], oldest first.
    Returns [Ok []] if [dir] does not exist. *)

type entry_summary = {
  id : string;
  parent_id : string option;
  role : string;  (** ["user"] or ["assistant"]. *)
  preview : string;
  is_fork : bool;  (** Entry has more than one child. *)
}

val list_entries : path:string -> unit -> (entry_summary list, string) result
(** [list_entries ~path ()] lists user and assistant messages oldest first,
    falling back to tool-call hints when an assistant turn has no text. *)

type branch_tip = {
  tip : string;  (** Leaf entry id for [--parentId]. *)
  role : string;  (** ["user"], ["assistant"], or [""]. *)
  preview : string;
  timestamp : string;  (** Timestamp of the tip entry. *)
}

val list_branches : path:string -> unit -> (branch_tip list, string) result
(** [list_branches ~path ()] returns one row per leaf ({!Session_read.leaf_ids})
    in file order, described by the nearest user/assistant ancestor. *)
