(** The [--handoff] document: a prompt that starts a {e fresh} thread carrying
    the work of a finished one.

    op does not compact. Compaction is lossy and rewards ever-longer threads; a
    handoff is extractive — the user names the next goal, a model writes the
    prompt for a new session, and the user reads it before it is sent. The
    section skeleton follows pi's compaction summary so the two remain legible
    to each other, but nothing is written back into the source session: the
    provenance record is the new session's [parentSession] header.

    {b [--handoff] is an unscoped session reader, and profiles that grant [op]
       inherit it.} [--handoff --sessionId ID] reads {e any} session under
    [~/.op_global/sessions] and summarises it through [handoffModel]; a plain
    [--sessionId] resume goes further and appends a branch to it. A nu profile
    that lists [op] as an external — the nested-subagent pattern — therefore
    hands the run read access to every session ever logged, through a door the
    gate cannot see: the script names no path (Layer 2 has nothing to match) and
    the sessions dir's dot-path protection only covers direct [open]. Ruled on
    2026-08-12: this is an {e accepted} trust grant, in the same ledger as
    [make]/[opam] in [pathMode: host] profiles — granting [op] is granting the
    session store, so a profile that shouldn't read history must simply not list
    [op]. Revisit only if a lineage-scoped restricted mode (child sessions
    stamped with [parentSession] via the [OP_*] handshake) is ever built.

    Everything here is pure. The single model call and the session I/O live in
    [bin/main.ml], so the document's shape stays unit-testable. *)

type files = { read : string list; modified : string list }
(** The paths a session touched {e through the native file tools}, deduplicated,
    in first-seen order. Files a [nu] script, a build tool or a sub-agent read
    or wrote are not seen and so are not listed. *)

val files_of_history : Llm.message list -> files
(** [files_of_history history] extracts the [path] argument of every read tool
    call (into {!field-read}) and every write or edit tool call (into
    {!field-modified}) in [history]. The glob tool is excluded: its [path] is a
    search root and its [pattern] names no file, so counting it would point the
    next thread at directories nobody opened.

    Only calls whose tool result came back without an error count. A gate
    denial, a missing file or an edit whose [old_string] did not match leaves
    the file untouched, and listing it would be exactly the invention this
    document promises not to make. A call with no result at all — the run was
    cut off mid-turn — is treated the same way. *)

val system_prompt : string
(** [system_prompt] The instruction that makes the model write the next thread's
    prompt rather than answer the conversation it is shown. *)

val user_prompt : goal:string -> string
(** [user_prompt ~goal] The final user turn appended to the replayed history.
    [goal] is the [PROMPT] argument of [--handoff]; [""] tells the model to
    infer the goal and say that it did. The goal is the only thing that varies
    per run — that {!assemble} appends the file lists is a constant, and lives
    in {!system_prompt}. *)

val assemble : summary:string -> files:files -> string
(** [assemble ~summary ~files] is the handoff document: the model's [summary],
    then [<read-files>]/[<modified-files>] blocks rendered {e by op}. The paths
    come from {!files_of_history} rather than the model, so none are
    hallucinated or dropped. They are plain text, so a reviewer can edit them in
    the draft before sending. Empty blocks are omitted entirely. *)
