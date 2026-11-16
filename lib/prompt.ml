(** Pure prompt-source resolution.

    The agent loop needs a single prompt string. It can come from
    [--prompt FILE], the positional [PROMPT] argument, or stdin (only when not a
    tty). [bin/main.ml] owns the I/O — reading the file, reading stdin, checking
    the tty — and the fatal-error plumbing; this module owns the decision: which
    source wins, when input is ambiguous, and what counts as an empty prompt. It
    is side-effect-free so the resolution rules are unit-tested directly.

    Precedence: [--prompt FILE] beats the [PROMPT] argument, which beats stdin
    (matching the [--prompt] flag's "overrides PROMPT and stdin" wording). A
    [PROMPT] argument given together with a non-empty stdin is an [Ambiguous]
    error rather than a silent drop, so a stray pipe can't be ignored into a
    long agentic run. *)

type source = File | Arg | Stdin

type error =
  | No_source (* nothing provided: no file, no argument, stdin a tty *)
  | Empty of source (* a source was provided but trims to empty *)
  | Ambiguous (* a PROMPT argument and a non-empty stdin both given *)

(* [resolve] applies the precedence and ambiguity rules. Each source is
   [string option]: [None] means "not provided" (file absent, no positional
   argument, or stdin is a tty); [Some s] means "provided, here are its bytes"
   — possibly empty or whitespace-only, which is an [Empty] error, distinct
   from the [None] "not provided" case. The returned prompt is trimmed. *)
let resolve ~file ~arg ~stdin =
  let trim = String.trim in
  match file with
  | Some content ->
      let t = trim content in
      if t = "" then Error (Empty File) else Ok t
  | None -> (
      (* No file: the argument and/or stdin. The argument wins over stdin, but
         if both carry a real (non-empty) prompt that is ambiguous. *)
      match (arg, stdin) with
      | Some a, Some s ->
          let ta = trim a and ts = trim s in
          if ta <> "" && ts <> "" then Error Ambiguous
          else if ta <> "" then Ok ta
          else if ts <> "" then Ok ts
          else Error (Empty Arg)
      | Some a, None ->
          let ta = trim a in
          if ta = "" then Error (Empty Arg) else Ok ta
      | None, Some s ->
          let ts = trim s in
          if ts = "" then Error (Empty Stdin) else Ok ts
      | None, None -> Error No_source)

let message ~file_path ?(resuming = false) = function
  | No_source when resuming ->
      (* [--sessionId]/[--parentId] was given but no prompt: op is one-shot, so
         resuming still needs a prompt to continue from. Name the session form so
         the message points at the actual mistake rather than the bare usage. *)
      "usage: op --sessionId ID \"prompt\" — resuming a session still needs a \
       prompt to continue\n\
       op --help prints man page"
  | No_source ->
      "usage: op \"prompt\" (or pipe a prompt on stdin)\n\
       op --help prints man page"
  | Empty File -> (
      match file_path with
      | Some p ->
          Fmt.str "prompt file %s is empty (after trimming whitespace)" p
      | None -> "prompt file is empty (after trimming whitespace)")
  | Empty Arg -> "prompt argument is empty (after trimming whitespace)"
  | Empty Stdin -> "prompt from stdin is empty (after trimming whitespace)"
  | Ambiguous ->
      "ambiguous input: a PROMPT argument and stdin were both provided; pass \
       only one (or redirect stdin from /dev/null)"
