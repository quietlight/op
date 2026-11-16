(* Tests for Op.Client provider dispatch.

   Client.call maps Config.api -> concrete adapter and forwards the [post]
   closure. We exercise the routing with a fake [post] that records the request
   URL/body and returns a canned, adapter-shaped response, then assert on which
   adapter was reached:
     - Anthropic posts to /v1/messages
     - OpenAI posts to /chat/completions with a "tool_choice" field
     - an unrecognized api string routes to the same OpenAI adapter *)

let provider ~api : Op.Config.provider =
  {
    name = "p";
    base_url = "https://example.test";
    api;
    api_key = Some "k";
    models = [];
  }

(* One real tool, so the OpenAI-shaped bodies carry the "tool_choice" marker
   the dispatch assertions look for (an empty tool set omits the field). *)
let fake_tool : Op.Tool.t =
  {
    name = "t";
    description = "d";
    parameters = `Assoc [];
    run = (fun _ -> Op.Tool.text "");
  }

(* Fake post recording the last request; always returns [response]. *)
let call ~api ~response =
  let last_url = ref "" and last_body = ref "" in
  let post ~url ~headers ~body =
    ignore headers;
    last_url := url;
    last_body := body;
    Ok response
  in
  let result =
    Op.Client.call ~post ~provider:(provider ~api) ~api_key:"k" ~model:"m"
      ~tools:[ fake_tool ]
      ~messages:[ Op.Llm.user "hi" ]
      ()
  in
  (result, !last_url, !last_body)

let anthropic_response =
  {|{"content":[{"type":"text","text":"hi"}],"stop_reason":"end_turn"}|}

let openai_response = {|{"choices":[{"message":{"content":"hi"}}]}|}

let responses_response =
  {|{"output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"hi"}]}]}|}

let google_response = {|{"candidates":[{"content":{"parts":[{"text":"hi"}]}}]}|}

let check_ok_text result =
  match result with
  | Ok (turn : Op.Llm.assistant_turn) ->
      Alcotest.(check string) "parsed text" "hi" turn.text
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_dispatch_anthropic () =
  let result, url, _ =
    call ~api:Op.Config.Anthropic_messages ~response:anthropic_response
  in
  Alcotest.(check bool)
    "anthropic endpoint" true
    (Test_support.contains url "/v1/messages");
  check_ok_text result

let test_dispatch_openai () =
  let result, url, body =
    call ~api:Op.Config.Openai_completions ~response:openai_response
  in
  Alcotest.(check bool)
    "chat endpoint" true
    (Test_support.contains url "/chat/completions");
  Alcotest.(check bool)
    "openai body has tool_choice" true
    (Test_support.contains body "tool_choice");
  check_ok_text result

let test_dispatch_openai_responses () =
  let result, url, body =
    call ~api:Op.Config.Openai_responses ~response:responses_response
  in
  Alcotest.(check bool)
    "responses endpoint" true
    (Test_support.contains url "/responses");
  Alcotest.(check bool)
    "responses body has input" true
    (Test_support.contains body "\"input\"");
  check_ok_text result

let test_dispatch_other () =
  let result, url, body =
    call ~api:(Op.Config.Other "custom") ~response:openai_response
  in
  Alcotest.(check bool)
    "chat endpoint" true
    (Test_support.contains url "/chat/completions");
  Alcotest.(check bool)
    "other apis get native tool calling" true
    (Test_support.contains body "tool_choice");
  check_ok_text result

let test_dispatch_google () =
  let result, url, body =
    call ~api:Op.Config.Google_generative_ai ~response:google_response
  in
  Alcotest.(check bool)
    "google endpoint" true
    (Test_support.contains url "/models/m:generateContent");
  Alcotest.(check bool)
    "google body has contents" true
    (Test_support.contains body "\"contents\"");
  check_ok_text result

let suite =
  ( "client",
    [
      ("dispatch anthropic", `Quick, test_dispatch_anthropic);
      ("dispatch openai", `Quick, test_dispatch_openai);
      ("dispatch openai responses", `Quick, test_dispatch_openai_responses);
      ("dispatch google", `Quick, test_dispatch_google);
      ("dispatch other api as openai", `Quick, test_dispatch_other);
    ] )
