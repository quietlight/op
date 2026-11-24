(** Provider dispatch from {!Config.api} to concrete adapters.

    Unrecognized api strings ({!Config.constructor-Other}) fall back to OpenAI
    chat-completions. *)

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
    dispatches one turn to the adapter matching [provider.api]. *)
