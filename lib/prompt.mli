(** Prompt-source resolution.

    Precedence: [--prompt FILE] > [PROMPT] argument > stdin. Giving both a
    [PROMPT] argument and non-empty stdin is {!constructor-Ambiguous}. *)

(** Prompt input source. *)
type source = File | Arg | Stdin

type error =
  | No_source  (** No file, no argument, and stdin is a tty. *)
  | Empty of source  (** Provided source trims to empty. *)
  | Ambiguous  (** Both [PROMPT] argument and non-empty stdin were given. *)

val resolve :
  file:string option ->
  arg:string option ->
  stdin:string option ->
  (string, error) result
(** [resolve ~file ~arg ~stdin] resolves and trims the prompt. [file] wins
    unconditionally; otherwise a non-empty [arg] and non-empty [stdin] yield
    {!constructor-Ambiguous}. [Some ""] yields {!constructor-Empty}. *)

val message : file_path:string option -> ?resuming:bool -> error -> string
(** [message ~file_path ?resuming e] formats [e] for stderr or JSON error
    envelopes. [resuming] (default [false]) tailors {!constructor-No_source} to
    [--sessionId]. *)
