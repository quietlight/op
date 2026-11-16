(** Session resume and branch-point resolution for prompt runs. *)

val path_of : cwd:string -> session_id:string -> (string, string) result
(** [path_of ~cwd ~session_id] Path of [session_id]'s file under [cwd]'s session
    directory. This is what a fresh session records as its [parentSession] when
    [--fromSession] forks off a finished thread. *)

val load :
  cwd:string ->
  session_id:string ->
  parent_id:string option ->
  unit ->
  (string * Llm.message list, string) result
(** [load ~cwd ~session_id ~parent_id ()] is the read-only half of {!resume}:
    the same leaf selection and branch-point correction, returning the session's
    path and history without opening the file for appending. Used by
    [--handoff], which must not write to the session it summarises. The leaf
    correction is not reported, since nothing is appended to the branch. *)

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
    session (located under [cwd]'s session directory), resolves the effective
    branch point, appends the model-change entry, and returns the resumed
    session, prior history, and optional [(requested, effective)] branch
    correction. [sink] is handed to {!Session.resume}, so a resumed run streams
    its new entries (there is no header line to re-emit). *)
