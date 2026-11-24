(** OpenAI Responses API adapter ([/responses]).

    Sends stateless ([store:false]) requests; reasoning is parsed for display
    only and never replayed. *)

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
    a turn to [<base_url>/responses]. [max_tokens] maps to [max_output_tokens]
    when provided. *)

(** Internal hooks for serialization and parsing tests. *)
module For_testing : sig
  val tool_specs : Tool.t list -> Yojson.Safe.t list
  (** [tool_specs tools] serializes [tools] to OpenAI Responses JSON. *)

  val build_body :
    ?max_tokens:int ->
    model:string ->
    tools:Tool.t list ->
    messages:Llm.message list ->
    unit ->
    string
  (** [build_body ?max_tokens ~model ~tools ~messages ()] builds the request
      JSON body. *)

  val parse_response : string -> (Llm.assistant_turn, Transport.error) result
  (** [parse_response body] parses an OpenAI Responses response body. *)
end
