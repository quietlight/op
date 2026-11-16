(** Provider dispatch for the agent loop.

    Maps the configured provider's wire format (Config.api) to a concrete
    adapter and forwards the call. No provider is rejected: an unrecognized api
    string ([Config.Other]) is assumed to speak the OpenAI chat-completions
    protocol with native tool calling, the de facto universal wire format. *)

val call :
  ?max_tokens:int ->
  post:Transport.post_fn ->
  provider:Config.provider ->
  api_key:string ->
  model:string ->
  tools:Tool.t list ->
  messages:Llm.message list ->
  unit ->
  (Llm.assistant_turn, Transport.error) result
(** [call ?max_tokens ~post ~provider ~api_key ~model ~tools ~messages ()]
    dispatches a turn to the provider's adapter. [max_tokens] is the per-model
    maxTokens from models.json, if any. The [post] closure is forwarded to the
    selected adapter. *)
