(** Human-readable rendering of session entries — op's default stdout.

    Per-tool detail: the file a [read] took and the slice it took, and the
    actual nu script. Every function returns lines rather than printing them, so
    the renderer is side-effect-free and expect-testable; [bin/main.ml] decides
    which channel they land on and when to flush.

    [nu_modules/pi-session-view.nu] is this module's twin: same layout, same
    line budgets, same per-tool lines, but for a finished session file and for
    hop's live [--stream]. The two are expected to print the {e same} transcript
    for the same entries — change one, change the other, and check it by
    rendering a session file both ways. The one deliberate divergence is [todo]:
    the twin keeps a checklist renderer for legacy op sessions and pi sessions
    (whose [todo] toolResult [details] carry the list state), while this module
    renders those entries through the generic unknown-tool path.

    The machine views are unaffected: [--stream] still emits the session's
    ndjson byte-identically (that is what [hop] reads) and [--json] still emits
    the bare result envelope. *)

type t
(** A render session. Stateful only because a [toolResult] entry names its call
    by id and nothing else — saying which file a [read] result came from means
    remembering the call — and because the [session] header carries the [cwd]
    that later paths are shown relative to. *)

val v : color:bool -> ?cwd:string -> ?echo_user:bool -> unit -> t
(** [v ~color ?cwd ?echo_user ()] is a fresh renderer. [color] enables ANSI
    styling; the caller owns the tty/[NO_COLOR] decision. [cwd] seeds the path
    root for a resumed run, whose stream carries no [session] header (a fresh
    run's header overwrites it). [echo_user] (default [false]) prints user
    messages: noise for a live run, where the prompt was just typed, but wanted
    when replaying a finished session. *)

val entry : t -> Yojson.Safe.t -> string list
(** [entry t json] renders one session-log entry — [session], [model_change],
    [message] (user / assistant / toolResult) or the gate log's [custom]
    [gate_verdict]. Unknown entry types and unknown tools degrade rather than
    fail: an unrecognised type renders nothing, an unrecognised tool falls back
    to a [k=v] argument summary, and missing or wrong-typed fields are treated
    as absent, so a partial or foreign log still displays. Entries must be fed
    in order — a [toolResult] rendered before its call cannot name its file. *)

val footer : t -> Yojson.Safe.t -> string list
(** [footer t envelope] renders the run's tail from the result envelope
    ({!Run_report.result_json} or {!Run_report.error_json}): outcome, turn and
    tool-call counts, tokens, the context gauge and the resume command — or, for
    an error envelope, the message and its hint. Taking the same value the
    machine modes serialise is what keeps the two from disagreeing about what a
    run reported. *)
