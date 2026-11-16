(** Pure rendering for [--context FILE] injection. [bin/main.ml] owns the I/O
    (reading each file, failing on a missing one); this module owns the shape of
    the injected text, so it is unit-tested directly.

    Each file becomes a delimited block carrying its path — the tag marks the
    text as background rather than part of the ask, and names where the model
    can re-read a fresh copy:

    {v
    <context file="CLAUDE.md">
    ...file contents...
    </context>
    v}

    Blocks are prepended to the prompt in the order the flags were given. The
    result is the run's user message, so injected context persists in the
    session transcript and survives [--sessionId] resumes without re-passing the
    flag (unlike the system prompt, which is never logged). *)

val render : files:(string * string) list -> prompt:string -> string
(** [render ~files ~prompt] is [prompt] with one [<context>] block per
    [(path, contents)] pair prepended, blocks and prompt separated by blank
    lines. Contents are injected whole, trimmed of outer whitespace only. With
    [files = []], [prompt] is returned unchanged. *)
