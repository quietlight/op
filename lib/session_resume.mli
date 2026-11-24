(** Session resume and branch-point resolution. *)

val path_of : cwd:string -> session_id:string -> (string, string) result
(** [path_of ~cwd ~session_id] resolves [session_id]'s file path under [cwd]'s
    session directory. *)

val load :
  cwd:string ->
  session_id:string ->
  parent_id:string option ->
  unit ->
  (string * Llm.message list, string) result
(** [load ~cwd ~session_id ~parent_id ()] is the read-only half of {!resume}:
    selects and corrects the leaf, returning the path and history without
    opening the file for appending (used by [--handoff]). *)

val resume :
  ?sink:(Yojson.Safe.t -> unit) ->
  cwd:string ->
  session_id:string ->
  parent_id:string option ->
  provider:Config.provider ->
  model:string ->
  unit ->
  (Session.t * Llm.message list * (string * string) option, string) result
(** [resume ?sink ~cwd ~session_id ~parent_id ~provider ~model ()] opens a prior
    session, resolves the branch point, appends a [model_change] entry, and
    returns [(session, history, branch_correction)]. *)
