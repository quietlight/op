open Op

let fake_post ~response ~url ~headers ~body =
  print_endline ("POST " ^ url);
  List.iter (fun (k, v) -> print_endline (k ^ ": " ^ v)) headers;
  print_endline (Yojson.Safe.pretty_to_string (Yojson.Safe.from_string body));
  Ok response

let show_turn = function
  | Ok (t : Llm.assistant_turn) -> (
      print_endline ("text: " ^ t.text);
      (match t.thinking with
      | None -> print_endline "thinking: none"
      | Some (Reasoning { text; signature }) ->
          print_endline
            ("thinking: " ^ Option.value signature ~default:"" ^ " | " ^ text)
      | Some (Redacted data) -> print_endline ("thinking: redacted | " ^ data));
      List.iter
        (fun (tc : Llm.tool_call) ->
          print_endline
            ("tool_call " ^ tc.id ^ " " ^ tc.name ^ " "
            ^ Yojson.Safe.to_string tc.arguments))
        t.tool_calls;
      match t.usage with
      | None -> print_endline "usage: none"
      | Some u ->
          Fmt.pr "usage: %d in / %d out / %d cached@." u.input u.output
            u.cache_read)
  | Error e -> print_endline ("error: " ^ Transport.pp_error e)

let echo_tool : Tool.t =
  {
    name = "echo";
    description = "Echo a string back.";
    parameters =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc [ ("text", `Assoc [ ("type", `String "string") ]) ] );
          ("required", `List [ `String "text" ]);
        ];
    run = (fun _ -> Tool.text "");
  }

let%expect_test "anthropic request and response" =
  let response =
    {json|{"content":[{"type":"text","text":"calling echo"},{"type":"tool_use","id":"tu_1","name":"echo","input":{"text":"hi"}}],"usage":{"input_tokens":12,"output_tokens":7}}|json}
  in
  let post = fake_post ~response in
  let result =
    Anthropic_adapter.call ~max_tokens:512 ~post
      ~base_url:"https://api.example.com" ~api_key:"test-key"
      ~model:"test-model" ~tools:[ echo_tool ]
      ~messages:[ System "be brief"; Llm.user "hi" ]
      ()
  in
  show_turn result;
  [%expect
    {|
    POST https://api.example.com/v1/messages
    Content-Type: application/json
    x-api-key: test-key
    anthropic-version: 2023-06-01
    {
      "system": "be brief",
      "model": "test-model",
      "max_tokens": 512,
      "cache_control": { "type": "ephemeral" },
      "messages": [
        { "role": "user", "content": [ { "type": "text", "text": "hi" } ] }
      ],
      "tools": [
        {
          "name": "echo",
          "description": "Echo a string back.",
          "input_schema": {
            "type": "object",
            "properties": { "text": { "type": "string" } },
            "required": [ "text" ]
          }
        }
      ]
    }
    text: calling echo
    thinking: none
    tool_call tu_1 echo {"text":"hi"}
    usage: 12 in / 7 out / 0 cached
    |}]

let%expect_test "anthropic api error" =
  let response =
    {json|{"error":{"type":"invalid_request_error","message":"boom"}}|json}
  in
  let post = fake_post ~response in
  let result =
    Anthropic_adapter.call ~post ~base_url:"https://api.example.com"
      ~api_key:"test-key" ~model:"test-model" ~tools:[] ~messages:[] ()
  in
  show_turn result;
  [%expect
    {|
    POST https://api.example.com/v1/messages
    Content-Type: application/json
    x-api-key: test-key
    anthropic-version: 2023-06-01
    {
      "model": "test-model",
      "max_tokens": 4096,
      "cache_control": { "type": "ephemeral" },
      "messages": []
    }
    error: API error: boom
    |}]

let%expect_test "openai request and response" =
  let response =
    {json|{"choices":[{"message":{"role":"assistant","content":"calling echo","tool_calls":[{"id":"call_1","type":"function","function":{"name":"echo","arguments":"{\"text\":\"hi\"}"}}]}}],"usage":{"prompt_tokens":12,"completion_tokens":7,"prompt_tokens_details":{"cached_tokens":9}}}|json}
  in
  let post = fake_post ~response in
  let result =
    Openai_adapter.call ~max_tokens:512 ~post
      ~base_url:"https://api.example.com" ~api_key:"test-key"
      ~model:"test-model" ~tools:[ echo_tool ]
      ~messages:[ System "be brief"; Llm.user "hi" ]
      ()
  in
  show_turn result;
  [%expect
    {|
    POST https://api.example.com/chat/completions
    Content-Type: application/json
    Authorization: Bearer test-key
    {
      "model": "test-model",
      "messages": [
        { "role": "system", "content": "be brief" },
        { "role": "user", "content": "hi" }
      ],
      "tools": [
        {
          "type": "function",
          "function": {
            "name": "echo",
            "description": "Echo a string back.",
            "parameters": {
              "type": "object",
              "properties": { "text": { "type": "string" } },
              "required": [ "text" ]
            }
          }
        }
      ],
      "tool_choice": "auto",
      "max_tokens": 512
    }
    text: calling echo
    thinking: none
    tool_call call_1 echo {"text":"hi"}
    usage: 3 in / 7 out / 9 cached
    |}]

let%expect_test "openai reasoning round trip" =
  let response =
    {json|{"choices":[{"message":{"role":"assistant","content":"hi","reasoning_content":"thinking aloud"}}]}|json}
  in
  let post = fake_post ~response in
  let result =
    Openai_adapter.call ~post ~base_url:"https://api.example.com"
      ~api_key:"test-key" ~model:"test-model" ~tools:[] ~messages:[] ()
  in
  show_turn result;
  [%expect
    {|
    POST https://api.example.com/chat/completions
    Content-Type: application/json
    Authorization: Bearer test-key
    { "model": "test-model", "messages": [] }
    text: hi
    thinking:  | thinking aloud
    usage: none
    |}]

let%expect_test "openai usage: fully cached prompt clamps at zero" =
  let response =
    {json|{"choices":[{"message":{"role":"assistant","content":"hi"}}],"usage":{"prompt_tokens":100,"completion_tokens":4,"prompt_tokens_details":{"cached_tokens":128}}}|json}
  in
  let post = fake_post ~response in
  let result =
    Openai_adapter.call ~post ~base_url:"https://api.example.com"
      ~api_key:"test-key" ~model:"test-model" ~tools:[] ~messages:[] ()
  in
  (match result with
  | Ok (t : Llm.assistant_turn) -> (
      match t.usage with
      | Some u -> Fmt.pr "in=%d cached=%d@." u.input u.cache_read
      | None -> print_endline "none")
  | Error _ -> print_endline "error");
  [%expect
    {|
    POST https://api.example.com/chat/completions
    Content-Type: application/json
    Authorization: Bearer test-key
    { "model": "test-model", "messages": [] }
    in=0 cached=128
    |}]
