(** Rendering for [--context FILE] injection.

    Each file is wrapped in [<context file="...">...</context>] and prepended to
    the user prompt in flag order, so injected context is logged to the session
    and survives resumes. *)

val render : files:(string * string) list -> prompt:string -> string
(** [render ~files ~prompt] prepends one [<context>] block per
    [(path, contents)] pair (trimmed, with closing tags escaped). Returns
    [prompt] unchanged when [files = []]. *)
