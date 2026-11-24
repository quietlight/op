let provider ~api : Op.Config.provider =
  {
    name = "p";
    base_url = "https://example.test";
    api;
    api_key = Some "k";
    models = [];
  }

(* Non-empty tool list so OpenAI-shaped bodies include "tool_choice". *)
let fake_tool : Op.Tool.t =
  {
    name = "t";
    description = "d";
    parameters = `Assoc [];
    run = (fun _ -> Op.Tool.text "");
  }

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
