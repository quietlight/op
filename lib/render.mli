(** Human-readable transcript renderer for session entries (default stdout and
    [op --render]).

    All functions return output lines without printing. *)

type t
(** Per-run renderer state: remembers tool-call arguments by id (so [toolResult]
    entries can name their target file) and the session [cwd] for relative
    paths. *)

val v : color:bool -> ?cwd:string -> ?echo_user:bool -> unit -> t
(** [v ~color ?cwd ?echo_user ()] creates a renderer. [cwd] seeds relative path
    display when no [session] header precedes entries; [echo_user] (default
    [false]) prints user messages (used for replay). *)

val entry : t -> Yojson.Safe.t -> string list
(** [entry t json] renders one session entry ([session], [model_change], or
    [message]), scrubbing terminal control characters first. Unknown types and
    malformed fields degrade gracefully. Feed entries in order so [toolResult]
    entries resolve their call arguments. *)

val scrub_string : string -> string
(** [scrub_string s] drops CR and replaces other C0 controls and DEL with ['?']
    (preserving ['\n'] and ['\t']) so untrusted text cannot inject terminal
    escapes. *)

val footer : t -> Yojson.Safe.t -> string list
(** [footer t envelope] renders the run summary or error from a
    {!Run_report.result_json} / {!Run_report.error_json} envelope. *)

val line : t -> string -> string list
(** [line t text] renders one JSONL line from a session file or [--stream],
    routing ["type":"result"] to {!footer} and other entries to {!entry}. Blank
    and non-JSON lines return [[]]. *)
