(** Compact single-line JSON envelopes for [op --list --json] and
    [op --set --json]. Field names are camelCase to match {!Run_report}. *)

val error : ?hint:string -> string -> string
(** [error ?hint msg] renders [{"error":msg,"isError":true}] with optional
    [hint]. *)

val sessions : Session_list.summary list -> string
(** [sessions xs] renders the [sessions] envelope. *)

val messages : session_id:string -> Session_list.entry_summary list -> string
(** [messages ~session_id xs] renders the [messages] envelope. *)

val branches : session_id:string -> Session_list.branch_tip list -> string
(** [branches ~session_id xs] renders the [branches] envelope. *)

val models : (string * string) list -> string
(** [models pairs] renders the [models] envelope from [(provider, model)] pairs.
*)

val default : provider:string -> model:string -> string
(** [default ~provider ~model] renders the [default] envelope. *)
