(** OpenAI chat-completions adapter with native function calling.

    Serializes [Tool.t list] into the OpenAI [tools] schema and parses
    [tool_calls] back into [Llm.assistant_turn]. This is the adapter for any
    provider that speaks the OpenAI chat-completions wire format with native
    tool support. *)

val call :
  ?max_tokens:int ->
  post:Transport.post_fn ->
  base_url:string ->
  api_key:string ->
  model:string ->
  tools:Tool.t list ->
  messages:Llm.message list ->
  unit ->
  (Llm.assistant_turn, Transport.error) result
(** [call ?max_tokens ~post ~base_url ~api_key ~model ~tools ~messages ()] posts
    a turn to [<base_url>/chat/completions] with native tool support and parses
    the assistant response. [max_tokens] is only sent when provided; providers
    apply their own default otherwise. *)

(** Internal hooks for serialization/parse tests. *)
module For_testing : sig
  val tool_specs : Tool.t list -> Yojson.Safe.t list
  (** [tool_specs tools] renders [tools] as OpenAI chat-completions tool-spec
      JSON. *)

  val build_body :
    ?max_tokens:int ->
    model:string ->
    tools:Tool.t list ->
    messages:Llm.message list ->
    unit ->
    string
  (** [build_body ?max_tokens ~model ~tools ~messages ()] is the JSON request
      body for an OpenAI chat-completions turn. *)

  val parse_response : string -> (Llm.assistant_turn, Transport.error) result
  (** [parse_response body] parses an OpenAI chat-completions response body into
      an assistant turn. *)
end
