let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

module M = Op.Openai_responses_adapter.For_testing

let test_responses_tool_specs () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "Run a command";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  match M.tool_specs [ tool ] with
  | [ spec ] ->
      let open Yojson.Safe.Util in
      Alcotest.(check string) "type" "function" (member "type" spec |> to_string);
      Alcotest.(check string) "name" "bash" (member "name" spec |> to_string);
      Alcotest.(check bool)
        "no nested function" true
        (match member "function" spec with `Null -> true | _ -> false);
      Alcotest.(check bool)
        "has parameters" true
        (match member "parameters" spec with `Assoc _ -> true | _ -> false)
  | _ -> Alcotest.fail "expected exactly one tool spec"

let test_responses_serializes_input () =
  let messages =
    [
      Op.Llm.System "sys";
      Op.Llm.user "hello";
      Op.Llm.Assistant
        (turn ~text:"let me look"
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
  let json =
    M.build_body ~model:"m" ~tools:[] ~messages () |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "tools omitted without tools" true
    (member "tools" json = `Null);
  Alcotest.(check bool)
    "tool_choice omitted without tools" true
    (member "tool_choice" json = `Null);
  Alcotest.(check bool) "store false" true (member "store" json = `Bool false);
  let input = member "input" json |> to_list in
  Alcotest.(check int) "five input items" 5 (List.length input);
  let item n = List.nth input n in
  Alcotest.(check string)
    "system role" "system"
    (member "role" (item 0) |> to_string);
  let user_part = member "content" (item 1) |> to_list |> List.hd in
  Alcotest.(check string)
    "user part type" "input_text"
    (member "type" user_part |> to_string);
  Alcotest.(check string)
    "user text" "hello"
    (member "text" user_part |> to_string);
  Alcotest.(check string)
    "assistant item type" "message"
    (member "type" (item 2) |> to_string);
  let out_part = member "content" (item 2) |> to_list |> List.hd in
  Alcotest.(check string)
    "output_text type" "output_text"
    (member "type" out_part |> to_string);
  Alcotest.(check string)
    "function_call type" "function_call"
    (member "type" (item 3) |> to_string);
  Alcotest.(check string)
    "call_id" "call_1"
    (member "call_id" (item 3) |> to_string);
  Alcotest.(check bool)
    "arguments is a string" true
    (match member "arguments" (item 3) with `String _ -> true | _ -> false);
  Alcotest.(check string)
    "output item type" "function_call_output"
    (member "type" (item 4) |> to_string);
  Alcotest.(check string)
    "output call_id" "call_1"
    (member "call_id" (item 4) |> to_string);
  Alcotest.(check string)
    "output content" "files"
    (member "output" (item 4) |> to_string)

let test_responses_empty_assistant () =
  let messages =
    [ Op.Llm.Assistant (turn [ tool_call ~id:"c1" ~name:"bash" () ]) ]
  in
  let json =
    M.build_body ~model:"m" ~tools:[] ~messages () |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let input = member "input" json |> to_list in
  Alcotest.(check int) "only the function_call item" 1 (List.length input);
  Alcotest.(check string)
    "function_call" "function_call"
    (member "type" (List.hd input) |> to_string)

let test_responses_error_prefix () =
  let messages =
    [
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
  let json =
    M.build_body ~model:"m" ~tools:[] ~messages () |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let item = member "input" json |> to_list |> List.hd in
  Alcotest.(check string)
    "error output prefixed" "Error: no such file"
    (member "output" item |> to_string)

let test_responses_tools_advertised () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "d";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let json =
    M.build_body ~model:"m" ~tools:[ tool ] ~messages:[ Op.Llm.user "hi" ] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "tools present" true
    (match member "tools" json with `List (_ :: _) -> true | _ -> false);
  Alcotest.(check string)
    "tool_choice" "auto"
    (member "tool_choice" json |> to_string)

let test_responses_max_output_tokens () =
  let body model_max =
    M.build_body ?max_tokens:model_max ~model:"m" ~tools:[]
      ~messages:[ Op.Llm.user "hi" ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "absent by default" true
    (body None |> member "max_output_tokens" = `Null);
  Alcotest.(check int)
    "configured" 1024
    (body (Some 1024) |> member "max_output_tokens" |> to_int)

let test_responses_text_usage () =
  let json =
    {|{"id":"resp_1","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"Done!"}]}],"usage":{"input_tokens":56,"output_tokens":78,"input_tokens_details":{"cached_tokens":40}},"status":"completed"}|}
  in
  match M.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "Done!" turn.text;
      Alcotest.(check int) "no tool calls" 0 (List.length turn.tool_calls);
      match turn.usage with
      | Some { input; output; cache_read; _ } ->
          Alcotest.(check int) "uncached input" 16 input;
          Alcotest.(check int) "usage output" 78 output;
          Alcotest.(check int) "cache read" 40 cache_read;
          Alcotest.(check int) "prompt total" 56 (input + cache_read)
      | None -> Alcotest.fail "expected usage")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_responses_parse_function_call () =
  let json =
    {|{"id":"resp_1","output":[{"type":"function_call","call_id":"call_abc","name":"bash","arguments":"{\"command\": \"ls\"}"}],"status":"completed"}|}
  in
  match M.parse_response json with
  | Ok turn -> (
      match turn.tool_calls with
      | [ tc ] -> (
          Alcotest.(check string) "id is call_id" "call_abc" tc.id;
          Alcotest.(check string) "name" "bash" tc.name;
          match Yojson.Safe.Util.member "command" tc.arguments with
          | `String "ls" -> ()
          | _ -> Alcotest.fail "expected command argument")
      | _ -> Alcotest.fail "expected one tool call")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_responses_parse_reasoning () =
  let json =
    {|{"id":"resp_1","output":[{"type":"reasoning","summary":[{"type":"summary_text","text":"step one"},{"type":"summary_text","text":"step two"}]},{"type":"message","role":"assistant","content":[{"type":"output_text","text":"answer"}]}],"status":"completed"}|}
  in
  match M.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "answer" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "text" "step one\n\nstep two" text
      | _ -> Alcotest.fail "expected reasoning")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_responses_reasoning_fallback () =
  let json =
    {|{"id":"resp_1","output":[{"type":"reasoning","content":[{"type":"reasoning_text","text":"raw chain"}]}],"status":"completed"}|}
  in
  match M.parse_response json with
  | Ok turn -> (
      match turn.thinking with
      | Some (Reasoning { text; _ }) ->
          Alcotest.(check string) "text" "raw chain" text
      | _ -> Alcotest.fail "expected reasoning from content fallback")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_responses_parse_error () =
  let json = {|{"error":{"message":"bad request"}}|} in
  match M.parse_response json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for error response"

let suite =
  ( "openai_responses_adapter",
    [
      ("responses serializes flat tools", `Quick, test_responses_tool_specs);
      ("responses serializes input", `Quick, test_responses_serializes_input);
      ( "responses omits empty assistant message",
        `Quick,
        test_responses_empty_assistant );
      ("responses error prefix", `Quick, test_responses_error_prefix);
      ("responses advertises tools", `Quick, test_responses_tools_advertised);
      ("responses max_output_tokens", `Quick, test_responses_max_output_tokens);
      ("responses parses text and usage", `Quick, test_responses_text_usage);
      ( "responses parses function_call",
        `Quick,
        test_responses_parse_function_call );
      ("responses parses reasoning", `Quick, test_responses_parse_reasoning);
      ( "responses parses reasoning content fallback",
        `Quick,
        test_responses_reasoning_fallback );
      ("responses parses error", `Quick, test_responses_parse_error);
    ] )
