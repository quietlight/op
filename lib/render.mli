(** Human-readable rendering of session entries — op's default stdout.

    Per-tool detail: the file a [read] took and the slice it took, and the
    actual nu script. Every function returns lines rather than printing them, so
    the renderer is side-effect-free and expect-testable; [bin/main.ml] decides
    which channel they land on and when to flush.

    This is the only renderer op maintains. [op --render] replays a finished
    session file, or a [--stream] piped in, through it, which is what [hop] does
    for its live view. An earlier Nushell renderer of the same layout,
    [pi-session-view.nu], lives outside this repository as an example of
    well-written nu and is not kept in step with this module.

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
(** [entry t json] renders one session-log entry — [session], [model_change], or
    [message] (user / assistant / toolResult). Unknown entry types and unknown
    tools degrade rather than fail: an unrecognised type renders nothing, an
    unrecognised tool falls back to a [k=v] argument summary, and missing or
    wrong-typed fields are treated as absent, so a partial or foreign log still
    displays. Terminal control characters in any string are scrubbed (ESC and
    friends shown as [?], CR dropped), so text from a file the agent read cannot
    drive the reader's terminal. Entries must be fed in order — a [toolResult]
    rendered before its call cannot name its file. *)

val footer : t -> Yojson.Safe.t -> string list
(** [footer t envelope] renders the run's tail from the result envelope
    ({!Run_report.result_json} or {!Run_report.error_json}): outcome, turn and
    tool-call counts, tokens, the context gauge and the resume command — or, for
    an error envelope, the message and its hint. Taking the same value the
    machine modes serialise is what keeps the two from disagreeing about what a
    run reported. *)

val line : t -> string -> string list
(** [line t text] renders one line of a session file, or of [op --stream]: a
    session entry through {!entry}, the stream's closing result envelope (the
    line tagged ["type":"result"]) through {!footer}. A blank or non-JSON line
    renders nothing. This is what [op --render] folds over its input. *)
