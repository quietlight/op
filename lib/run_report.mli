(** Pure JSON renderers for the prompt-run [op --json] result object, and the
    exit-code classification shared by the [--json] and human paths.

    Mirrors {!List_report}: each renderer returns one compact JSON line for
    stdout — or, in the [_json] variants, the envelope value the line is made
    of, which {!Render} formats for the default human output. [bin/main.ml] owns
    data fetching, stderr diagnostics, and exit codes. Side-effect-free so they
    are unit/expect-testable.

    Field names are camelCase to match the [--list] envelopes: [sessionId],
    [parentId], [stopReason], [isError], etc. *)

val sum_usage : ?base:int -> Llm.message list -> Llm.usage option
(** [sum_usage ?base history] totals the reported usage across the assistant
    turns of [history], or [None] when no turn carried usage. All four fields —
    including [cache_read] and [cache_creation] — are summed, so callers can
    report the true prompt volume rather than just the uncached [input]. [base]
    (default [0]) skips that many leading messages: a resumed run passes the
    length of the replayed history so the report covers only this run's turns,
    which {!Session_read.messages} would otherwise double-count now that it
    restores the recorded usage. *)

val context_tokens : Llm.usage -> int
(** [context_tokens u] is the window occupancy of one turn:
    [input + output + cache_read + cache_creation]. The cache counts are part of
    the prompt, so a heavily cached thread is not mistaken for an empty one. *)

val context_usage : Llm.message list -> Llm.usage option
(** [context_usage history] is the usage of the {e last} assistant turn that
    reported any — the current occupancy of the context window, as opposed to
    {!sum_usage}'s lifetime total. [None] when no turn reported usage. *)

val context_fields :
  window:int option -> Llm.message list -> (string * Yojson.Safe.t) list
(** [context_fields ~window history] is the envelope's [context] object —
    [{tokens, window, pct}], [pct] rounded to two decimals — or [[]] when either
    the window (no [contextWindow] in models.json) or the occupancy (no turn
    reported usage) is unknown. Callers fold it into {!val-result}'s [metadata].
*)

val tokens_line : Llm.usage -> string
(** [tokens_line u] renders the human stderr token summary, e.g.
    [{|tokens: 3 in, 50 out (1259 cache read, 0 cache write)|}]. The cache
    counts are appended only when nonzero, so a non-caching run stays the terse
    [{|tokens: N in, N out|}]. *)

type classification = {
  is_error : bool;
  exit_code : int;
  stop_reason : string option;
      (** [["finished"]] | [["max_steps"]] | [["gate_killed"]] |
          [["context_limit"]] for a completed/truncated/gate-terminated/
          context-capped run; [None] for a hard failure (the {!val-error}
          envelope carries no [stopReason]). *)
}
(** A run's machine classification — the single source of truth for [isError]
    and the exit code. {!field-exit_code} is [0] iff {!field-is_error} is
    [false]. *)

val classify : ('a * Agent.outcome, 'b) result -> classification
(** [classify result] derives the classification from an agent-loop result. A
    truncated run ([Max_steps_reached]) is an error: a parent agent budgeting on
    [isError] (or [$?]) must see a non-complete run as non-success. *)

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
     ~cache_read ~cache_write ~metadata ~outcome ()] is {!val-result}'s envelope
    before serialisation — the same object, as JSON. The human renderer
    ({!Render.footer}) formats this value rather than reconstructing the run's
    numbers itself, so the two output modes can never disagree about what a run
    reported. *)

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
     ~cache_write ~metadata ~outcome ()] renders the Ok envelope for a completed
    or truncated run. The [tokens] object carries [input], [output], [cacheRead]
    and [cacheWrite] (the last two mirroring the session-log usage object, so a
    parent can budget on the real prompt volume). [isError] and [stopReason] are
    derived from [outcome]; [answer] is retained even on truncation (a partial
    answer is useful for salvage/resume). *)

val error_json :
  ?typed:bool ->
  msg:string ->
  ?hint:string ->
  ?session_fields:(string * Yojson.Safe.t) list ->
  ?model:string ->
  ?metadata:(string * Yojson.Safe.t) list ->
  unit ->
  Yojson.Safe.t
(** [error_json ?typed ~msg ?hint ?session_fields ?model ?metadata ()] is
    {!val-error}'s envelope before serialisation. See {!result_json}. *)

val error :
  ?typed:bool ->
  msg:string ->
  ?hint:string ->
  ?session_fields:(string * Yojson.Safe.t) list ->
  ?model:string ->
  ?metadata:(string * Yojson.Safe.t) list ->
  unit ->
  string
(** [error ?typed ~msg ?hint ?session_fields ?model ?metadata ()] renders the
    hard-failure envelope [{"error":…,"isError":true}]. [session_fields] and
    [model] are optional so early failures (prompt resolution, config load, TLS)
    — where the model and session are not yet known — render as the bare
    [{"error","isError"}] shape. *)

(** {1 Envelope fields derived from the run} *)

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
