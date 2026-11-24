let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

let test_anthropic_parse_tool_use () =
  let json =
    {|{"content":[{"type":"text","text":"Hi"},{"type":"tool_use","id":"tu_1","name":"bash","input":{"command":"ls"}}],"stop_reason":"tool_use","usage":{"input_tokens":12,"output_tokens":34,"cache_read_input_tokens":100,"cache_creation_input_tokens":50}}|}
  in
  match Op.Anthropic_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "Hi" turn.text;
      (match turn.usage with
      | Some { input; output; cache_read; cache_creation } ->
          Alcotest.(check int) "usage input" 12 input;
          Alcotest.(check int) "usage output" 34 output;
          Alcotest.(check int) "cache read" 100 cache_read;
          Alcotest.(check int) "cache creation" 50 cache_creation
      | None -> Alcotest.fail "expected usage");
      match turn.tool_calls with
      | [ tc ] ->
          Alcotest.(check string) "id" "tu_1" tc.id;
          Alcotest.(check string) "name" "bash" tc.name
      | _ -> Alcotest.fail "expected one tool call")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_anthropic_parse_end_turn () =
  let json =
    {|{"content":[{"type":"text","text":"done"}],"stop_reason":"end_turn"}|}
  in
  match Op.Anthropic_adapter.For_testing.parse_response json with
  | Ok turn ->
      Alcotest.(check string) "text" "done" turn.text;
      Alcotest.(check int) "no tool calls" 0 (List.length turn.tool_calls);
      Alcotest.(check bool) "no usage" true (turn.usage = None)
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_anthropic_serializes_messages () =
  let messages =
    [
      Op.Llm.System "sys";
      Op.Llm.user "hello";
      Op.Llm.Assistant
        (turn
           [
             tool_call ~id:"t1" ~name:"bash"
               ~args:(`Assoc [ ("command", `String "ls") ])
               ();
           ]);
      Op.Llm.Tool
        {
          id = "t1";
          name = "bash";
          content = "files";
          is_error = false;
          details = None;
        };
    ]
  in
  let body =
    Op.Anthropic_adapter.For_testing.build_body ~model:"m" ~tools:[] ~messages
      ()
  in
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "system field" "sys"
    (member "system" json |> to_string);
  Alcotest.(check string)
    "automatic cache breakpoint" "ephemeral"
    (member "cache_control" json |> member "type" |> to_string);
  Alcotest.(check bool)
    "tools omitted without tools" true
    (member "tools" json = `Null);
  let msgs = member "messages" json |> to_list in
  Alcotest.(check (list string))
    "roles"
    [ "user"; "assistant"; "user" ]
    (List.map (fun m -> member "role" m |> to_string) msgs);
  let tool_result_block =
    List.nth msgs 2 |> member "content" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "tool_result type" "tool_result"
    (member "type" tool_result_block |> to_string);
  Alcotest.(check string)
    "tool_use_id" "t1"
    (member "tool_use_id" tool_result_block |> to_string)

let test_anthropic_parse_thinking () =
  let json =
    {|{"content":[{"type":"thinking","thinking":"let me reason","signature":"sig-xyz"},{"type":"text","text":"answer"}],"stop_reason":"end_turn"}|}
  in
  match Op.Anthropic_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "answer" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check string) "thinking text" "let me reason" text;
          Alcotest.(check (option string))
            "signature" (Some "sig-xyz") signature
      | _ -> Alcotest.fail "expected reasoning")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_anthropic_parse_redacted () =
  let json =
    {|{"content":[{"type":"redacted_thinking","data":"ENCRYPTED"},{"type":"text","text":"answer"}],"stop_reason":"end_turn"}|}
  in
  match Op.Anthropic_adapter.For_testing.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "answer" turn.text;
      match turn.thinking with
      | Some (Redacted data) ->
          Alcotest.(check string) "redacted data" "ENCRYPTED" data
      | _ -> Alcotest.fail "expected redacted")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let assistant_blocks ~thinking ~tool_calls =
  let messages = [ Op.Llm.Assistant (turn ~text:"hi" ~thinking tool_calls) ] in
  Op.Anthropic_adapter.For_testing.build_body ~model:"m" ~tools:[] ~messages ()
  |> Yojson.Safe.from_string
  |> Yojson.Safe.Util.member "messages"
  |> Yojson.Safe.Util.to_list |> List.hd
  |> Yojson.Safe.Util.member "content"
  |> Yojson.Safe.Util.to_list

let block_type b = Yojson.Safe.Util.(member "type" b |> to_string)

let test_anthropic_replays_thinking_first () =
  let blocks =
    assistant_blocks
      ~thinking:
        (Some (Op.Llm.Reasoning { text = "reason"; signature = Some "sig-1" }))
      ~tool_calls:[ tool_call ~id:"t1" ~name:"bash" () ]
  in
  Alcotest.(check (list string))
    "block order"
    [ "thinking"; "text"; "tool_use" ]
    (List.map block_type blocks);
  let first = List.hd blocks in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "thinking text" "reason"
    (member "thinking" first |> to_string);
  Alcotest.(check string)
    "signature" "sig-1"
    (member "signature" first |> to_string)

let test_anthropic_drops_unsigned_thinking () =
  let blocks =
    assistant_blocks
      ~thinking:(Some (Op.Llm.Reasoning { text = "reason"; signature = None }))
      ~tool_calls:[]
  in
  Alcotest.(check (list string))
    "no thinking block" [ "text" ]
    (List.map block_type blocks)

let test_anthropic_replays_redacted () =
  let blocks =
    assistant_blocks ~thinking:(Some (Op.Llm.Redacted "ENC")) ~tool_calls:[]
  in
  Alcotest.(check (list string))
    "redacted first"
    [ "redacted_thinking"; "text" ]
    (List.map block_type blocks);
  Alcotest.(check string)
    "data" "ENC"
    Yojson.Safe.Util.(member "data" (List.hd blocks) |> to_string)

let test_anthropic_tools_advertised () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "d";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let json =
    Op.Anthropic_adapter.For_testing.build_body ~model:"m" ~tools:[ tool ]
      ~messages:[ Op.Llm.user "hi" ]
      ()
    |> Yojson.Safe.from_string
  in
  Alcotest.(check bool)
    "tools present" true
    (match Yojson.Safe.Util.member "tools" json with
    | `List (_ :: _) -> true
    | _ -> false)

let test_anthropic_max_tokens () =
  let body model_max =
    Op.Anthropic_adapter.For_testing.build_body ?max_tokens:model_max ~model:"m"
      ~tools:[]
      ~messages:[ Op.Llm.user "hi" ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "default" 4096
    (body None |> member "max_tokens" |> to_int);
  Alcotest.(check int)
    "configured" 8192
    (body (Some 8192) |> member "max_tokens" |> to_int)

let suite =
  ( "anthropic_adapter",
    [
      ("anthropic parses tool_use", `Quick, test_anthropic_parse_tool_use);
      ("anthropic parses end_turn", `Quick, test_anthropic_parse_end_turn);
      ( "anthropic serializes messages",
        `Quick,
        test_anthropic_serializes_messages );
      ("anthropic parses thinking", `Quick, test_anthropic_parse_thinking);
      ("anthropic parses redacted", `Quick, test_anthropic_parse_redacted);
      ( "anthropic replays thinking first",
        `Quick,
        test_anthropic_replays_thinking_first );
      ( "anthropic drops unsigned thinking",
        `Quick,
        test_anthropic_drops_unsigned_thinking );
      ("anthropic replays redacted", `Quick, test_anthropic_replays_redacted);
      ("anthropic advertises tools", `Quick, test_anthropic_tools_advertised);
      ("anthropic max_tokens", `Quick, test_anthropic_max_tokens);
    ] )
