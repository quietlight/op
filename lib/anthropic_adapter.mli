(** Anthropic Messages adapter for the agent loop.

    Serializes a provider-neutral [Llm.message list] into the Anthropic Messages
    wire format (native tool_use / tool_result blocks) and parses the response
    back into an [Llm.assistant_turn]. *)

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
    a turn to [<base_url>/v1/messages] and parses the assistant response.
    [max_tokens] caps the response length (default 4096; the Messages API
    requires the field). *)

(** Internal hooks for serialization/parse tests. *)
module For_testing : sig
  val tool_specs : Tool.t list -> Yojson.Safe.t list
  (** [tool_specs tools] renders [tools] as Anthropic Messages tool-spec JSON.
  *)

  val build_body :
    ?max_tokens:int ->
    model:string ->
    tools:Tool.t list ->
    messages:Llm.message list ->
    unit ->
    string
  (** [build_body ?max_tokens ~model ~tools ~messages ()] is the JSON request
      body for an Anthropic Messages turn. *)

  val parse_response : string -> (Llm.assistant_turn, Transport.error) result
  (** [parse_response body] parses an Anthropic Messages response body into an
      assistant turn. *)
end
