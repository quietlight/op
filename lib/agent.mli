(** Provider-neutral agent loop.

    [run] is parameterised over the model-call function so it can be unit-tested
    without a network (tests pass a scripted fake; bin/main passes a Client.call
    closure). The loop only ever sees Llm.message values; each provider adapter
    serializes them to its own wire format. *)

type event =
  | Assistant_turn of Llm.assistant_turn
      (** one assistant turn, before its tool calls (if any) run *)
  | Tool_finished of Llm.tool_result
  | Tool_ids_repaired
      (** a provider returned a duplicate or empty tool-call id and the loop
          remade the ids unique; emitted at most once per run *)

type outcome =
  | Finished  (** the model stopped requesting tools *)
  | Max_steps_reached  (** the step cap was hit before the model finished *)
  | Gate_killed of string
      (** the permission gate raised Code_red; the run was stopped after
          answering every pending tool call with an error. Carries the reason.
      *)
  | Context_limit_reached
      (** the context window filled past the configured limit; the run stopped
          between turns, with a complete history to hand off from *)

val default_system : string
(** [default_system] A minimal default system prompt. Seam for a richer prompt
    later. *)

val system_prompt : cwd:string -> date:string -> tools:string list -> string
(** [system_prompt ~cwd ~date ~tools] is {!val-default_system}, a one-sentence
    description of the capabilities granted by the [tools] names present (so the
    prompt reflects the actual selection), and an environment block describing
    the working directory [cwd] and current [date]. *)

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
(** [run ~call ~tools ~emit ?gate ?max_steps ?over_context ~messages ()] drives
    the conversation forward from [messages] until the model stops requesting
    tools, [max_steps] (default 75) is reached, [over_context] reports the
    context window full ({!constructor-Context_limit_reached}), or [gate] raises
    {!Gate.constructor-Code_red} ({!constructor-Gate_killed}). [over_context]
    (default: never) is a closure over the run's model and config, keeping the
    loop provider-neutral; it is consulted before each model call, so a stopped
    run's history has no unanswered tool calls. Returns the updated, wire-valid
    neutral history. [gate] is consulted before every tool call (default: allow
    all); a {!Gate.constructor-Deny} yields an [is_error] result and the loop
    continues, a {!Gate.constructor-Warn} appends to the tool's result. [emit]
    fires for every assistant turn and tool result as they happen, in history
    order. *)
