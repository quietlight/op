(** OpenAI Responses API adapter (/responses) with native function calling.

    Serializes [Tool.t list] into the flat Responses tool schema and the message
    history into the [input] item array, and parses the [output] array back into
    an [Llm.assistant_turn]. This is the adapter for providers that speak the
    OpenAI Responses wire format rather than chat-completions.

    Reasoning is captured for display but never replayed: parsed reasoning items
    carry a [None] [signature], and assistant turns are serialized without their
    [thinking]. *)

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
    a turn to [<base_url>/responses] with native tool support and parses the
    assistant response. [max_tokens] maps to [max_output_tokens] and is only
    sent when provided; providers apply their own default otherwise. *)

(** Internal hooks for serialization/parse tests. *)
module For_testing : sig
  val tool_specs : Tool.t list -> Yojson.Safe.t list
  (** [tool_specs tools] renders [tools] as OpenAI Responses tool-spec JSON. *)

  val build_body :
    ?max_tokens:int ->
    model:string ->
    tools:Tool.t list ->
    messages:Llm.message list ->
    unit ->
    string
  (** [build_body ?max_tokens ~model ~tools ~messages ()] is the JSON request
      body for an OpenAI Responses turn. *)

  val parse_response : string -> (Llm.assistant_turn, Transport.error) result
  (** [parse_response body] parses an OpenAI Responses response body into an
      assistant turn. *)
end
