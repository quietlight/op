(** Provider-neutral agent loop.

    [run] takes a model-call function and operates solely on {!Llm.message}
    values; wire serialization lives in the provider adapters. *)

type event =
  | Assistant_turn of Llm.assistant_turn
      (** Assistant turn, emitted before its tool calls run. *)
  | Tool_finished of Llm.tool_result
  | Tool_ids_repaired
      (** Duplicate or empty provider tool-call ids were replaced; emitted at
          most once per run. *)

type outcome =
  | Finished  (** Model stopped requesting tools. *)
  | Max_steps_reached  (** Step cap reached before completion. *)
  | Gate_killed of string
      (** Gate returned {!Gate.constructor-Code_red}; pending calls were
          answered with errors. Carries the reason. *)
  | Context_limit_reached
      (** Context window crossed the configured limit between turns. *)

val default_system : string
(** Default system prompt. *)

val system_prompt : cwd:string -> date:string -> tools:string list -> string
(** [system_prompt ~cwd ~date ~tools] builds the system prompt for the selected
    [tools], working directory [cwd], and [date]. *)

val run :
  call:(messages:Llm.message list -> (Llm.assistant_turn, string) result) ->
  tools:Tool.t list ->
  emit:(event -> unit) ->
  ?gate:(Llm.tool_call -> Gate.verdict) ->
  ?max_steps:int ->
  ?over_context:(Llm.message list -> bool) ->
  messages:Llm.message list ->
  unit ->
  (Llm.message list * outcome, string) result
(** [run ~call ~tools ~emit ?gate ?max_steps ?over_context ~messages ()] loops
    until the model finishes, [max_steps] (default 150) is reached,
    [over_context] returns [true] ({!constructor-Context_limit_reached}), or
    [gate] returns {!Gate.constructor-Code_red} ({!constructor-Gate_killed}).
    Caps are checked before each model call so the returned history has no
    unanswered tool calls. [gate] defaults to allowing all calls;
    {!Gate.constructor-Deny} produces an error tool result and
    {!Gate.constructor-Warn} appends a warning to the tool output. [emit] fires
    in history order. *)
