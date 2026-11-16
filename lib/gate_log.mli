(** Pi-format audit log for gate judge calls.

    Each op run gets at most one gate-log file under
    [~/.op_global/gate/--<cwd>--/] (same directory encoding and jsonl format as
    {!Session}), created lazily on the first judge call — most runs make none
    and leave nothing behind. The header's [parentSession] points at the run's
    main session file, tying the two together.

    Per judge call the log records the exact payload the judge model received (a
    [user] message), the model's raw response with real token usage (an
    [assistant] message, absent when the transport failed), and a [custom] entry
    ([customType] "gate_verdict") with the final verdict, its reason, the
    routing reason from the deterministic rules, and the command — the parsed
    verdict can differ from the raw response (unparseable output, transport
    errors and unknown verdicts all coerce to "fault"), so it is recorded
    explicitly.

    This is an audit trail wearing session clothes: consecutive judge calls are
    independent conversations, not multi-turn history, so "resuming" a gate log
    is meaningless. Logging is best-effort and must never break a run:
    {!log_call} swallows every failure, warning once and disabling itself. *)

type t
(** A lazy handle: no file exists until the first {!log_call}. *)

val default_root : unit -> (string, string) result
(** [default_root ()] Return [$HOME/.op_global/gate], or an error when HOME is
    unset. *)

val v :
  ?root:string ->
  cwd:string ->
  parent_session:string option ->
  warn:(string -> unit) ->
  unit ->
  t
(** [v ?root ~cwd ~parent_session ~warn ()] Build a lazy handle rooted at [root]
    (default {!default_root}; a root error is deferred — [warn] fires only if a
    call actually needs logging). [parent_session] is the main session file's
    path, recorded in the header. [warn] reports the first logging failure. *)

val log_call :
  t ->
  api:string ->
  provider:string ->
  model:string ->
  payload:string ->
  command:string ->
  reason:string ->
  turn:Llm.assistant_turn option ->
  verdict:Gate.verdict ->
  unit
(** [log_call t ~api ~provider ~model ~payload ~command ~reason ~turn ~verdict]
    Append one judge call: [payload] is the exact user-message text sent to the
    judge model, [turn] its raw response ([None] when the transport failed),
    [reason] why the rules routed the command to the judge, [verdict] the final
    parsed verdict. Never raises; on failure warns once and disables the log. *)

val path : t -> string option
(** [path t] Return the log file path once it has been materialized, [None]
    before the first successful {!log_call}. *)

val close : t -> unit
(** [close t] Close the log file if one was opened. Never raises. *)
