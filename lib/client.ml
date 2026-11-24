let call ?max_tokens ~post ~(provider : Config.provider) ~api_key ~model ~tools
    ~messages () =
  match provider.api with
  | Config.Anthropic_messages ->
      Anthropic_adapter.call ?max_tokens ~post ~base_url:provider.base_url
        ~api_key ~model ~tools ~messages ()
  | Config.Openai_completions | Config.Other _ ->
      Openai_adapter.call ?max_tokens ~post ~base_url:provider.base_url ~api_key
        ~model ~tools ~messages ()
  | Config.Openai_responses ->
      Openai_responses_adapter.call ?max_tokens ~post
        ~base_url:provider.base_url ~api_key ~model ~tools ~messages ()
  | Config.Google_generative_ai ->
      Google_generative_ai_adapter.call ?max_tokens ~post
        ~base_url:provider.base_url ~api_key ~model ~tools ~messages ()
