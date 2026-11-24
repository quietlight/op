(** [--handoff] prompt assembly: carries work from a finished session into a
    fresh one without compacting in place.

    {b Trust boundary:} [--handoff --sessionId ID] reads any session under
    [~/.op_global/sessions] (and [--sessionId] can append to it). A [nu] profile
    listing [op] in [externals] therefore grants access to the entire session
    store — accepted as an explicit trust grant; profiles that should not read
    session history must omit [op]. *)

type files = { read : string list; modified : string list }
(** Deduplicated, first-seen paths touched via native file tools ([read],
    [write], [edit]). Files accessed through [nu] or subprocesses are not
    tracked. *)

val files_of_history : Llm.message list -> files
(** [files_of_history history] extracts paths from successful ["read"],
    ["write"], and ["edit"] calls into {!field-read} and {!field-modified}.
    Calls with error or missing results, and legacy ["glob"] calls, are
    excluded. *)

val system_prompt : string
(** System prompt instructing the model to write the opening prompt for the next
    thread. *)

val user_prompt : goal:string -> string
(** [user_prompt ~goal] builds the final user prompt appended to replayed
    history ([""] asks the model to infer the goal). *)

val assemble : summary:string -> files:files -> string
(** [assemble ~summary ~files] appends [<read-files>] and [<modified-files>]
    blocks from {!files_of_history} to [summary], omitting empty blocks. *)
