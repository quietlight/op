(** Pure prompt-source resolution. [bin/main.ml] owns the I/O (reading the file,
    reading stdin, checking the tty) and the fatal-error plumbing; this module
    owns the decision — which source wins, when input is ambiguous, and what
    counts as an empty prompt — so the rules are unit-tested directly.

    Precedence: [--prompt FILE] beats the [PROMPT] argument, which beats stdin.
    A [PROMPT] argument given together with a non-empty stdin is an
    {!constructor-Ambiguous} error rather than a silent drop. *)

(** Which source a prompt came from. *)
type source = File | Arg | Stdin

type error =
  | No_source
      (** No source at all: no [--prompt] file, no argument, stdin a tty. *)
  | Empty of source  (** A source was provided but trims to empty whitespace. *)
  | Ambiguous  (** A [PROMPT] argument and a non-empty stdin were both given. *)

val resolve :
  file:string option ->
  arg:string option ->
  stdin:string option ->
  (string, error) result
(** [resolve ~file ~arg ~stdin] picks the prompt by precedence ([file] > [arg] >
    [stdin]). Each source is [None] when absent (file not given, no positional
    argument, or stdin is a tty) and [Some s] when provided — [Some ""] is an
    {!constructor-Empty} error, distinct from [None]. The returned prompt is
    trimmed. [file], when [Some], always wins and the other sources are not
    consulted (matching [--prompt]'s "overrides" wording); otherwise a non-empty
    [arg] together with a non-empty [stdin] is {!constructor-Ambiguous}. *)

val message : file_path:string option -> ?resuming:bool -> error -> string
(** [message ~file_path ?resuming e] renders a one-line human/JSON error string
    for [e] — the same text flows to stderr in human mode and into the
    [{"error":…}] JSON envelope in [--json] mode. [file_path] names the file in
    an [Empty File] message; it is ignored otherwise. [resuming] (default
    [false]) only changes the {!constructor-No_source} message: when a session
    resume was requested but no prompt was given, it points at the [--sessionId]
    form instead of the bare usage line. *)
