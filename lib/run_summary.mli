(** Pure helpers for assembling prompt-run JSON metadata. *)

val last_assistant_text : Llm.message list -> string option
(** [last_assistant_text history] returns the last non-empty assistant text in
    [history], if any. *)

val run_metadata : base:int -> Llm.message list -> (string * Yojson.Safe.t) list
(** [run_metadata ~base history] summarizes turns and tool results added after
    [base] messages. *)

val session_fields : Session.t option -> (string * Yojson.Safe.t) list
(** [session_fields session] returns [sessionId] and [parentId] fields for the
    prompt-run JSON envelope. *)

val branch_correction_metadata :
  (string * string) option -> (string * Yojson.Safe.t) list
(** [branch_correction_metadata c] returns JSON fields for a redirected branch
    point. *)
