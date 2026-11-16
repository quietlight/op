(** Google Generative AI (Gemini) adapter for the agent loop.

    Serializes a provider-neutral [Llm.message list] into the Gemini
    [generateContent] wire format (native functionCall / functionResponse parts)
    and parses the response back into an [Llm.assistant_turn]. *)

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
    a turn to [<base_url>/models/<model>:generateContent] and parses the
    assistant response. [max_tokens] caps the response length
    ([maxOutputTokens]); when omitted, Gemini applies its own default. *)

(** Internal hooks for serialization/parse tests. *)
module For_testing : sig
  val tool_specs : Tool.t list -> Yojson.Safe.t list
  (** [tool_specs tools] renders [tools] as Google generative-AI tool-spec JSON.
  *)

  val build_body :
    ?max_tokens:int ->
    model:string ->
    tools:Tool.t list ->
    messages:Llm.message list ->
    unit ->
    string
  (** [build_body ?max_tokens ~model ~tools ~messages ()] is the JSON request
      body for a Google generative-AI turn. *)

  val parse_response : string -> (Llm.assistant_turn, Transport.error) result
  (** [parse_response body] parses a Google generative-AI response body into an
      assistant turn. *)
end
