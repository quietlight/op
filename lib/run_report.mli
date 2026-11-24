(** Prompt-run JSON result envelopes and exit-code classification.

    Field names are camelCase ([sessionId], [parentId], [stopReason], [isError])
    to match {!List_report}. *)

val sum_usage : ?base:int -> Llm.message list -> Llm.usage option
(** [sum_usage ?base history] totals reported usage across assistant turns after
    skipping [base] leading messages (default 0, used to exclude replayed turns
    on resume). *)

val context_tokens : Llm.usage -> int
(** [context_tokens u] is [input + output + cache_read + cache_creation]. *)

val context_usage : Llm.message list -> Llm.usage option
(** [context_usage history] returns the usage of the last assistant turn that
    reported any (current window occupancy). *)

val context_fields :
  window:int option -> Llm.message list -> (string * Yojson.Safe.t) list
(** [context_fields ~window history] returns
    [("context", {tokens, window, pct})] with [pct] rounded to two decimals, or
    [[]] if window or usage is unknown. *)

val tokens_line : Llm.usage -> string
(** [tokens_line u] formats ["tokens: N in, N out"], appending cache counts only
    when non-zero. *)

type classification = {
  is_error : bool;
  exit_code : int;
  stop_reason : string option;
      (** ["finished"], ["max_steps"], ["gate_killed"], ["context_limit"], or
          [None] for hard failures. *)
}
(** Single source of truth for [isError] and exit code ([0] iff [not is_error]).
*)

val classify : ('a * Agent.outcome, 'b) result -> classification
(** [classify result] maps an agent-loop result to its exit code and
    [stopReason]. Non-[Finished] outcomes are errors. *)

val result_json :
  ?typed:bool ->
  answer:string ->
  session_fields:(string * Yojson.Safe.t) list ->
  model:string ->
  input:int ->
  output:int ->
  cache_read:int ->
  cache_write:int ->
  metadata:(string * Yojson.Safe.t) list ->
  outcome:Agent.outcome ->
  unit ->
  Yojson.Safe.t
(** [result_json ?typed ~answer ~session_fields ~model ~input ~output
     ~cache_read ~cache_write ~metadata ~outcome ()] builds the result envelope
    JSON (shared with {!Render.footer}). [typed] prepends [("type", "result")]
    for [--stream]. *)

val result :
  ?typed:bool ->
  answer:string ->
  session_fields:(string * Yojson.Safe.t) list ->
  model:string ->
  input:int ->
  output:int ->
  cache_read:int ->
  cache_write:int ->
  metadata:(string * Yojson.Safe.t) list ->
  outcome:Agent.outcome ->
  unit ->
  string
(** [result ?typed ~answer ~session_fields ~model ~input ~output ~cache_read
     ~cache_write ~metadata ~outcome ()] serializes {!result_json} as one
    compact JSON line. *)

val error_json :
  ?typed:bool ->
  msg:string ->
  ?hint:string ->
  ?session_fields:(string * Yojson.Safe.t) list ->
  ?model:string ->
  ?metadata:(string * Yojson.Safe.t) list ->
  unit ->
  Yojson.Safe.t
(** [error_json ?typed ~msg ?hint ?session_fields ?model ?metadata ()] builds
    the hard-failure envelope JSON. *)

val error :
  ?typed:bool ->
  msg:string ->
  ?hint:string ->
  ?session_fields:(string * Yojson.Safe.t) list ->
  ?model:string ->
  ?metadata:(string * Yojson.Safe.t) list ->
  unit ->
  string
(** [error ?typed ~msg ?hint ?session_fields ?model ?metadata ()] serializes
    {!error_json} as one compact JSON line. *)

(** {1 Envelope fields derived from the run} *)

val last_assistant_text : Llm.message list -> string option
(** [last_assistant_text history] returns the last non-empty assistant text. *)

val run_metadata : base:int -> Llm.message list -> (string * Yojson.Safe.t) list
(** [run_metadata ~base history] summarizes turns, tool calls, tool errors, and
    unique tool names added after [base] messages. *)

val session_fields : Session.t option -> (string * Yojson.Safe.t) list
(** [session_fields session] returns [sessionId] and optional [parentId] fields.
*)

val branch_correction_metadata :
  (string * string) option -> (string * Yojson.Safe.t) list
(** [branch_correction_metadata c] returns [branchCorrectedFrom]/[To] fields. *)
