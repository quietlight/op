(* Selects a provider adapter from the configured wire format and calls it.
   This is the single place that maps Config.api -> adapter; the agent loop is
   adapter-agnostic and only sees Llm.message / Llm.assistant_turn. *)

let call ?max_tokens ~post ~(provider : Config.provider) ~api_key ~model ~tools
    ~messages () =
  match provider.api with
  | Config.Anthropic_messages ->
      Anthropic_adapter.call ?max_tokens ~post ~base_url:provider.base_url
        ~api_key ~model ~tools ~messages ()
  | Config.Openai_completions | Config.Other _ ->
      (* Unrecognized api strings are assumed OpenAI-compatible: the
         chat-completions wire format with native tool calling is the de facto
         universal protocol. *)
      Openai_adapter.call ?max_tokens ~post ~base_url:provider.base_url ~api_key
        ~model ~tools ~messages ()
  | Config.Openai_responses ->
      Openai_responses_adapter.call ?max_tokens ~post
        ~base_url:provider.base_url ~api_key ~model ~tools ~messages ()
  | Config.Google_generative_ai ->
      Google_generative_ai_adapter.call ?max_tokens ~post
        ~base_url:provider.base_url ~api_key ~model ~tools ~messages ()
