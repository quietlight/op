(* Section-local helpers from agent tests *)
let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

let test_openai_tool_specs () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "Run a command";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let specs = Op.Openai_adapter.For_testing.tool_specs [ tool ] in
  match specs with
  | [ spec ] ->
      let open Yojson.Safe.Util in
      Alcotest.(check string) "type" "function" (member "type" spec |> to_string);
      let func = member "function" spec in
      Alcotest.(check string) "name" "bash" (member "name" func |> to_string);
      Alcotest.(check bool)
        "has parameters" true
        (match member "parameters" func with `Assoc _ -> true | _ -> false)
  | _ -> Alcotest.fail "expected exactly one tool spec"

let test_openai_serializes_messages () =
  let messages =
    [
      Op.Llm.System "sys";
      Op.Llm.user "hello";
      Op.Llm.Assistant
        (turn
           [
             tool_call ~id:"call_1" ~name:"bash"
               ~args:(`Assoc [ ("command", `String "ls") ])
               ();
           ]);
      Op.Llm.Tool
        {
          id = "call_1";
          name = "bash";
          content = "files";
          is_error = false;
          details = None;
        };
    ]
  in
  let body =
    Op.Openai_adapter.For_testing.build_body ~model:"test-model" ~tools:[]
      ~messages ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "model" "test-model" (member "model" json |> to_string);
  Alcotest.(check bool)
    "tools omitted without tools" true
    (member "tools" json = `Null);
  Alcotest.(check bool)
    "tool_choice omitted without tools" true
    (member "tool_choice" json = `Null);
  let msgs = member "messages" json |> to_list in
  Alcotest.(check (list string))
    "roles"
    [ "system"; "user"; "assistant"; "tool" ]
    (List.map (fun m -> member "role" m |> to_string) msgs);
  (* Assistant message has tool_calls *)
  let assistant_msg = List.nth msgs 2 in
  let tool_calls = member "tool_calls" assistant_msg |> to_list in
  Alcotest.(check int) "one tool call" 1 (List.length tool_calls);
  let tc = List.hd tool_calls in
  Alcotest.(check string) "tc id" "call_1" (member "id" tc |> to_string);
  Alcotest.(check string) "tc type" "function" (member "type" tc |> to_string);
  Alcotest.(check string)
    "tc name" "bash"
    (member "function" tc |> member "name" |> to_string);
  (* Tool result message *)
  let tool_msg = List.nth msgs 3 in
  Alcotest.(check string)
    "tool_call_id" "call_1"
    (member "tool_call_id" tool_msg |> to_string);
  Alcotest.(check string)
    "content" "files"
    (member "content" tool_msg |> to_string)

let test_openai_parse_tool_calls () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"Let me look.","tool_calls":[{"id":"call_abc","type":"function","function":{"name":"bash","arguments":"{\"command\": \"ls\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":56,"completion_tokens":78,"prompt_tokens_details":{"cached_tokens":40}}}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "Let me look." turn.text;
      (match turn.usage with
      (* prompt_tokens (56) is the whole prompt and includes cached_tokens
         (40); Llm.usage keeps the fields disjoint, so input is the 16
         uncached tokens and input + cache_read recovers the wire total. *)
      | Some { input; output; cache_read; _ } ->
          Alcotest.(check int) "uncached input" 16 input;
          Alcotest.(check int) "usage output" 78 output;
          Alcotest.(check int) "cache read" 40 cache_read;
          Alcotest.(check int) "prompt total" 56 (input + cache_read)
      | None -> Alcotest.fail "expected usage");
      match turn.tool_calls with
      | [ tc ] -> (
          Alcotest.(check string) "id" "call_abc" tc.id;
          Alcotest.(check string) "name" "bash" tc.name;
          match Yojson.Safe.Util.member "command" tc.arguments with
          | `String "ls" -> ()
          | _ -> Alcotest.fail "expected command argument")
      | _ -> Alcotest.fail "expected one tool call")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_usage_without_details () =
  (* Kimi-style response: tool_calls present, empty content, and a usage object
     with no prompt_tokens_details. Must parse cleanly with cache_read = 0
     rather than raising on the absent nested object. *)
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"","tool_calls":[{"id":"bash_0","type":"function","function":{"name":"bash","arguments":"{\"command\":\"uname -s\"}"}}]},"finish_reason":"tool_calls"}],"usage":{"prompt_tokens":1499,"completion_tokens":92}}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check int) "one tool call" 1 (List.length turn.tool_calls);
      match turn.usage with
      | Some { input; cache_read; _ } ->
          Alcotest.(check int) "usage input" 1499 input;
          Alcotest.(check int) "cache read defaults to 0" 0 cache_read
      | None -> Alcotest.fail "expected usage")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_parse_reasoning_content () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"Done","reasoning_content":"because reasons"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          (* display-only: never replayed, so the signature is [None] *)
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "text" "because reasons" text
      | _ -> Alcotest.fail "expected reasoning from reasoning_content")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_reasoning_precedence () =
  (* Both "reasoning" and "reasoning_text" present: "reasoning" wins because it
     comes first in the probe order. *)
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"ok","reasoning":"first","reasoning_text":"second"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "text" "first" text
      | _ -> Alcotest.fail "expected reasoning")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_array_thinking () =
  (* Mistral's magistral models return [content] as an array of typed chunks: a
     "thinking" chunk (whose [thinking] is itself an array of text parts)
     followed by "text" chunks. Both must be recovered, not silently dropped. *)
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":[{"type":"thinking","thinking":[{"type":"text","text":"step one"},{"type":"text","text":"step two"}]},{"type":"text","text":"the visible answer"}]},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "the visible answer" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "thinking" "step onestep two" text
      | _ -> Alcotest.fail "expected reasoning from thinking chunk")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_array_text () =
  (* Array content with only text chunks (no reasoning): concatenate the text,
     no thinking. *)
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":[{"type":"text","text":"hello "},{"type":"text","text":"world"}]},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn ->
      Alcotest.(check string) "text" "hello world" turn.text;
      Alcotest.(check bool) "no thinking" true (turn.thinking = None)
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_thinking_not_replayed () =
  (* Reasoning is display-only on this adapter: an assistant turn carrying
     thinking must not echo it back on the wire (chat-completions providers
     don't want prior reasoning resent). *)
  let messages =
    [
      Op.Llm.Assistant
        (turn ~text:"hi"
           ~thinking:
             (Some (Op.Llm.Reasoning { text = "deep"; signature = Some "sig" }))
           []);
    ]
  in
  let body =
    Op.Openai_adapter.For_testing.build_body ~model:"m" ~tools:[] ~messages ()
  in
  let msg =
    Yojson.Safe.from_string body
    |> Yojson.Safe.Util.member "messages"
    |> Yojson.Safe.Util.to_list |> List.hd
  in
  let absent field =
    match Yojson.Safe.Util.member field msg with `Null -> true | _ -> false
  in
  Alcotest.(check bool)
    "no reasoning field replayed" true
    (absent "reasoning_content" && absent "reasoning" && absent "sig");
  Alcotest.(check string)
    "content is the visible text" "hi"
    Yojson.Safe.Util.(member "content" msg |> to_string)

let test_openai_parse_think_tags () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"<think>step by step</think>\n\nThe answer is 4"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "The answer is 4" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "text" "step by step" text
      | _ -> Alcotest.fail "expected thinking from think tags")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_think_no_open () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"step by step</think>\n\nanswer"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "answer" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "text" "step by step" text
      | _ -> Alcotest.fail "expected thinking from think tags")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_reasoning_over_tags () =
  (* Providers may duplicate field reasoning as inline tags (MiniMax-M2.7):
     the field wins as the reasoning, and the tags still leave the text. *)
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"<think>inline</think>\n\nvisible","reasoning_content":"field"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "visible" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          (* the field still wins as the reasoning; signature is [None] because
             this adapter never replays reasoning *)
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "text" "field" text
      | _ -> Alcotest.fail "expected thinking from field")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_no_think_tags () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"just text"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn ->
      Alcotest.(check string) "text" "just text" turn.text;
      Alcotest.(check bool) "no thinking" true (turn.thinking = None)
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_serializes_empty_signature () =
  let messages =
    [
      Op.Llm.Assistant
        (turn ~text:"hi"
           ~thinking:
             (Some (Op.Llm.Reasoning { text = "inline"; signature = None }))
           []);
    ]
  in
  let body =
    Op.Openai_adapter.For_testing.build_body ~model:"m" ~tools:[] ~messages ()
  in
  let json = Yojson.Safe.from_string body in
  let msg = Yojson.Safe.Util.(member "messages" json |> to_list |> List.hd) in
  Alcotest.(check bool)
    "no extra field for empty signature" true
    (match Yojson.Safe.Util.keys msg with
    | [ "role"; "content" ] -> true
    | _ -> false)

let test_openai_parse_end_turn () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"Done!"},"finish_reason":"stop"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn ->
      Alcotest.(check string) "text" "Done!" turn.text;
      Alcotest.(check int) "no tool calls" 0 (List.length turn.tool_calls)
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_multiple_tool_calls () =
  let json =
    {|{"choices":[{"message":{"role":"assistant","content":"","tool_calls":[{"id":"c1","type":"function","function":{"name":"bash","arguments":"{\"command\": \"ls\"}"}},{"id":"c2","type":"function","function":{"name":"read","arguments":"{\"path\": \"foo.ml\"}"}}]},"finish_reason":"tool_calls"}]}|}
  in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check int) "two tool calls" 2 (List.length turn.tool_calls);
      match turn.tool_calls with
      | [ tc1; tc2 ] ->
          Alcotest.(check string) "first name" "bash" tc1.name;
          Alcotest.(check string) "second name" "read" tc2.name
      | _ -> Alcotest.fail "expected exactly two tool calls")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_openai_error_prefix () =
  (* Tool errors are encoded as content with an "Error: " prefix, not an
     is_error field — the OpenAI spec has no such field. *)
  let messages =
    [
      Op.Llm.System "sys";
      Op.Llm.Assistant
        (turn
           [
             tool_call ~id:"e1" ~name:"read"
               ~args:(`Assoc [ ("path", `String "/nope") ])
               ();
           ]);
      Op.Llm.Tool
        {
          id = "e1";
          name = "read";
          content = "no such file";
          is_error = true;
          details = None;
        };
    ]
  in
  let body =
    Op.Openai_adapter.For_testing.build_body ~model:"m" ~tools:[] ~messages ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let msgs = member "messages" json |> to_list in
  let tool_msg = List.nth msgs 2 in
  Alcotest.(check string)
    "error content prefixed" "Error: no such file"
    (member "content" tool_msg |> to_string);
  (* No is_error field in the wire format *)
  Alcotest.(check bool)
    "no is_error field" true
    (match member "is_error" tool_msg with `Null -> true | _ -> false)

let test_openai_parse_malformed_choice () =
  (* A choice missing the "message" object should return an error, not crash *)
  let json = {|{"choices":[{"finish_reason":"stop"}]}|} in
  match Op.Openai_adapter.For_testing.parse_response json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for malformed response"

(* A non-empty tool set is advertised with tool_choice auto; the empty case
   (both fields omitted) is covered in test_openai_serializes_messages. *)
let test_openai_tools_advertised () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "d";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let json =
    Op.Openai_adapter.For_testing.build_body ~model:"m" ~tools:[ tool ]
      ~messages:[ Op.Llm.user "hi" ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "tools present" true
    (match member "tools" json with `List (_ :: _) -> true | _ -> false);
  Alcotest.(check string)
    "tool_choice" "auto"
    (member "tool_choice" json |> to_string)

let test_openai_max_tokens () =
  let body model_max =
    Op.Openai_adapter.For_testing.build_body ?max_tokens:model_max ~model:"m"
      ~tools:[]
      ~messages:[ Op.Llm.user "hi" ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "absent by default" true
    (body None |> member "max_tokens" = `Null);
  Alcotest.(check int)
    "configured" 1024
    (body (Some 1024) |> member "max_tokens" |> to_int)

let suite =
  ( "openai_adapter",
    [
      ("openai serializes tools", `Quick, test_openai_tool_specs);
      ("openai serializes messages", `Quick, test_openai_serializes_messages);
      ("openai advertises tools", `Quick, test_openai_tools_advertised);
      ("openai parses tool_calls", `Quick, test_openai_parse_tool_calls);
      ("openai usage without details", `Quick, test_openai_usage_without_details);
      ( "openai parses reasoning_content",
        `Quick,
        test_openai_parse_reasoning_content );
      ("openai reasoning precedence", `Quick, test_openai_reasoning_precedence);
      ( "openai parses array content with thinking",
        `Quick,
        test_openai_array_thinking );
      ("openai parses array content text only", `Quick, test_openai_array_text);
      ( "openai does not serialize thinking",
        `Quick,
        test_openai_thinking_not_replayed );
      ( "openai serializes empty signature",
        `Quick,
        test_openai_serializes_empty_signature );
      ("openai parses think tags", `Quick, test_openai_parse_think_tags);
      ("openai parses think tags no open", `Quick, test_openai_think_no_open);
      ( "openai reasoning field wins over tags",
        `Quick,
        test_openai_reasoning_over_tags );
      ("openai no think tags", `Quick, test_openai_no_think_tags);
      ("openai parses end_turn", `Quick, test_openai_parse_end_turn);
      ( "openai parses multiple tool_calls",
        `Quick,
        test_openai_multiple_tool_calls );
      ("openai error prefix", `Quick, test_openai_error_prefix);
      ( "openai parse malformed choice",
        `Quick,
        test_openai_parse_malformed_choice );
      ("openai max_tokens", `Quick, test_openai_max_tokens);
    ] )
