(** Pi-compatible session logging.

    Each run of op appends a session file under ~/.op_global/sessions/--<cwd>--/
    in pi's session jsonl format (see docs/pi-session-format.md and
    docs/pi-session.schema.json): a header line followed by tree entries linked
    by id/parentId. op writes a strictly linear chain today; the tree shape is
    what will later let a run fork from any prior entry via --parentId.

    The system prompt is not logged (pi does not store it either); user prompts,
    assistant turns, and tool results are. Entries are flushed as they are
    appended, so partial runs still leave a valid log. *)

type t

val default_root : unit -> (string, string) result
(** [default_root ()] Return [$HOME/.op_global/sessions], or an error when HOME
    is unset. *)

val dir_for : root:string -> cwd:string -> string
(** [dir_for ~root ~cwd] Return the per-project session directory, e.g.
    [dir_for ~root ~cwd:"/Users/x/proj"] is [<root>/--Users-x-proj--] (pi's
    encoding: leading separator stripped, [/], [\ ] and [:] mapped to [-]). *)

val v :
  ?sink:(Yojson.Safe.t -> unit) ->
  ?parent_session:string ->
  dir:string ->
  cwd:string ->
  provider:string ->
  model:string ->
  unit ->
  (t, string) result
(** [v ?sink ?parent_session ~dir ~cwd ~provider ~model ()] Create [dir] if
    needed and start a new session file [<timestamp>_<uuidv7>.jsonl] inside it,
    writing the session header plus initial [model_change] and
    [thinking_level_change] entries. [parent_session] is the header's
    [parentSession] field — the path of a related session file (pi uses it for
    forks; op for [--fromSession]).

    [sink] (default [ignore]) is called with every line written to the file —
    the header first, then each entry — after it has been flushed to disk. This
    is the tee behind [op --stream]: the stream is byte-for-byte the session
    file's lines. A sink that raises would abort the run, so callers pass a
    total function. *)

val path : t -> string
(** [path t] Return the session file path. *)

val id : t -> string
(** [id t] Return the session id (the uuid in the file name), as passed to
    [op --sessionId] to resume the session. *)

val leaf : t -> string option
(** [leaf t] Return the id of the last entry written (the parent of whatever
    comes next), or [None] before any tree entry has been appended. After
    [create] it is always [Some] (the header is followed by a [model_change]
    entry). *)

val append_model_change : t -> provider:string -> model:string -> unit
(** [append_model_change t ~provider ~model] Append a [model_change] entry
    recording the provider and model in use. Used by [create] and when resuming,
    where a fork may run under a different model than the original path. *)

val append_user :
  t -> ?blocks:Llm.content_block list -> text:string -> unit -> unit
(** [append_user t ?blocks ~text ()] Append a user message entry: the [text]
    body followed by any content [blocks] (image attachments), written as a pi
    [text]/[image] content list. [blocks] defaults to [[]] — a text-only turn,
    the single text-block list op has always written. *)

val append_tool_result : t -> Llm.tool_result -> unit
(** [append_tool_result t r] Append a tool result entry [r]. *)

val append_assistant :
  t ->
  api:string ->
  provider:string ->
  model:string ->
  Llm.assistant_turn ->
  unit
(** [append_assistant t ~api ~provider ~model turn] Append an assistant turn.
    [api] is the wire API name (e.g. "anthropic-messages"), [provider] the
    provider name, [model] the model id — recorded on each assistant message as
    pi does. Token usage is copied from the provider when available and zeroed
    otherwise. *)

val close : t -> unit
(** [close t] Close the session file [t]. *)

val resume :
  ?sink:(Yojson.Safe.t -> unit) ->
  path:string ->
  leaf:string ->
  unit ->
  (t, string) result
(** [resume ?sink ~path ~leaf ()] Reopen an existing session file for appending:
    collect every entry id (via {!Session_read.entry_ids}, so fresh ids stay
    unique), check that [leaf] is one of them (Error otherwise), open the file
    in append mode, and set the current leaf to [leaf]. The next append becomes
    a child of [leaf]; if [leaf] already has children this forks the tree.
    Reading the file back into messages is {!Session_read.load_messages}. [sink]
    tees the appended lines as in {!v} — a resumed session has no header to
    re-emit, so a stream consumer sees only the new entries. *)
