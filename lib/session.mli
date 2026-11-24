(** Pi-compatible session logging.

    Writes append-only JSONL files under [~/.op_global/sessions/--<cwd>--/] (see
    [docs/pi-session-format.md]): a session header followed by tree entries
    linked by [id]/[parentId]. System prompts are not logged; entries are
    flushed on each write. *)

type t

val default_root : unit -> (string, string) result
(** [default_root ()] returns [$HOME/.op_global/sessions], or an error if [HOME]
    is unset. *)

val dir_for : root:string -> cwd:string -> string
(** [dir_for ~root ~cwd] encodes [cwd] using pi's directory naming: strip
    leading separator, map ['/'], ['\\'], and [':'] to ['-'], wrapped in [--].
*)

val v :
  ?sink:(Yojson.Safe.t -> unit) ->
  ?parent_session:string ->
  dir:string ->
  cwd:string ->
  provider:string ->
  model:string ->
  unit ->
  (t, string) result
(** [v ?sink ?parent_session ~dir ~cwd ~provider ~model ()] creates [dir] if
    needed and starts [<timestamp>_<uuidv7>.jsonl] with a session header,
    [model_change], and [thinking_level_change] entries. [parent_session] sets
    the header's [parentSession] field. [sink] (default [ignore]) receives each
    line after it is flushed to disk and must not raise. *)

val unsaved :
  ?sink:(Yojson.Safe.t -> unit) ->
  ?parent_session:string ->
  cwd:string ->
  provider:string ->
  model:string ->
  unit ->
  t
(** [unsaved ?sink ?parent_session ~cwd ~provider ~model ()] creates an
    in-memory session that feeds [sink] without writing a file. {!val-path} and
    {!val-id} are [""]. *)

val path : t -> string
(** [path t] is the session file path. *)

val id : t -> string
(** [id t] is the session UUID extracted from the filename. *)

val leaf : t -> string option
(** [leaf t] is the id of the last written entry (parent of the next entry),
    always [Some] after {!val-v}. *)

val append_model_change : t -> provider:string -> model:string -> unit
(** [append_model_change t ~provider ~model] appends a [model_change] entry. *)

val append_user :
  t -> ?blocks:Llm.content_block list -> text:string -> unit -> unit
(** [append_user t ?blocks ~text ()] appends a user message with [text] and
    optional content [blocks] (default [[]]). *)

val append_tool_result : t -> Llm.tool_result -> unit
(** [append_tool_result t r] appends a [toolResult] message entry. *)

val append_assistant :
  t ->
  api:string ->
  provider:string ->
  model:string ->
  Llm.assistant_turn ->
  unit
(** [append_assistant t ~api ~provider ~model turn] appends an assistant message
    entry with wire metadata and token usage (zeroed when absent). *)

val close : t -> unit
(** [close t] closes the session file. *)

val resume :
  ?sink:(Yojson.Safe.t -> unit) ->
  path:string ->
  leaf:string ->
  unit ->
  (t, string) result
(** [resume ?sink ~path ~leaf ()] reopens [path] for appending at [leaf]
    (validating [leaf] and seeding the id set via {!Session_read.entry_ids}).
    Appending to a [leaf] with existing children creates a branch. [sink]
    receives only newly appended entries. *)
