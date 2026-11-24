open Op

let turn ?(text = "") ?(tool_calls = []) () : Llm.assistant_turn =
  { text; thinking = None; tool_calls; usage = None }

let scripted turns =
  let remaining = ref turns in
  fun ~messages:_ ->
    match !remaining with
    | [] -> Error "script exhausted"
    | t :: rest ->
        remaining := rest;
        Ok t

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
    run =
      (fun args ->
        match Tool.required_string args "text" with
        | Ok text -> Tool.text ("echo: " ^ text)
        | Error e -> Error e);
  }

let emit (e : Agent.event) =
  match e with
  | Assistant_turn t -> print_endline ("assistant: " ^ t.text)
  | Tool_finished (tr : Llm.tool_result) ->
      print_endline
        ((if tr.is_error then "ERR" else "ok")
        ^ " " ^ tr.name ^ ": " ^ tr.content)
  | Tool_ids_repaired -> print_endline "repaired duplicate tool-call ids"

let show_result (history, outcome) =
  print_endline
    ("outcome: "
    ^
    match outcome with
    | Agent.Finished -> "Finished"
    | Agent.Max_steps_reached -> "Max_steps_reached"
    | Agent.Gate_killed msg -> "Gate_killed: " ^ msg
    | Agent.Context_limit_reached -> "Context_limit_reached");
  List.iter
    (function
      | Llm.System s -> print_endline ("system: " ^ s)
      | Llm.User { text; _ } -> print_endline ("user: " ^ text)
      | Llm.Assistant (t : Llm.assistant_turn) ->
          let n = List.length t.tool_calls in
          print_endline
            ("assistant: " ^ t.text
            ^ if n > 0 then Fmt.str " (+%d tool calls)" n else "")
      | Llm.Tool (tr : Llm.tool_result) ->
          print_endline
            ((if tr.is_error then "ERR" else "ok")
            ^ " " ^ tr.name ^ ": " ^ tr.content))
    history

let%expect_test "finish without tools" =
  let call = scripted [ turn ~text:"hello" () ] in
  let result =
    Agent.run ~call ~tools:[] ~emit ~messages:[ Llm.user "test" ] ()
  in
  (match result with
  | Ok r -> show_result r
  | Error e -> print_endline ("error: " ^ e));
  [%expect
    {|
    assistant: hello
    outcome: Finished
    user: test
    assistant: hello
    |}]

let%expect_test "one tool round trip" =
  let call =
    scripted
      [
        turn ~text:"using echo"
          ~tool_calls:
            [
              {
                Llm.id = "t1";
                name = "echo";
                arguments = `Assoc [ ("text", `String "hi") ];
              };
            ]
          ();
        turn ~text:"done" ();
      ]
  in
  let result =
    Agent.run ~call ~tools:[ echo_tool ] ~emit ~messages:[ Llm.user "test" ] ()
  in
  (match result with
  | Ok r -> show_result r
  | Error e -> print_endline ("error: " ^ e));
  [%expect
    {|
    assistant: using echo
    ok echo: echo: hi
    assistant: done
    outcome: Finished
    user: test
    assistant: using echo (+1 tool calls)
    ok echo: echo: hi
    assistant: done
    |}]

let%expect_test "duplicate tool-call ids are repaired and warned once" =
  let dup text =
    turn ~text
      ~tool_calls:
        [
          {
            Llm.id = "functions.bash:0";
            name = "echo";
            arguments = `Assoc [ ("text", `String "hi") ];
          };
        ]
      ()
  in
  let call =
    scripted [ dup "call one"; dup "call two"; turn ~text:"done" () ]
  in
  let result =
    Agent.run ~call ~tools:[ echo_tool ] ~emit ~messages:[ Llm.user "test" ] ()
  in
  (match result with
  | Ok (history, _) ->
      List.iter
        (function
          | Llm.Assistant (t : Llm.assistant_turn) ->
              List.iter
                (fun (tc : Llm.tool_call) -> print_endline ("id: " ^ tc.id))
                t.tool_calls
          | _ -> ())
        history
  | Error e -> print_endline ("error: " ^ e));
  [%expect
    {|
    assistant: call one
    ok echo: echo: hi
    repaired duplicate tool-call ids
    assistant: call two
    ok echo: echo: hi
    assistant: done
    id: functions.bash:0
    id: op_call_0
    |}]

let%expect_test "unknown tool is an error result" =
  let call =
    scripted
      [
        turn ~text:"using nope"
          ~tool_calls:
            [ { Llm.id = "t1"; name = "nope"; arguments = `Assoc [] } ]
          ();
        turn ~text:"done" ();
      ]
  in
  let result =
    Agent.run ~call ~tools:[ echo_tool ] ~emit ~messages:[ Llm.user "test" ] ()
  in
  (match result with
  | Ok r -> show_result r
  | Error e -> print_endline ("error: " ^ e));
  [%expect
    {|
    assistant: using nope
    ERR nope: unknown tool "nope"
    assistant: done
    outcome: Finished
    user: test
    assistant: using nope (+1 tool calls)
    ERR nope: unknown tool "nope"
    assistant: done
    |}]

let%expect_test "failing tool" =
  let fail_tool : Tool.t =
    {
      name = "fail";
      description = "Always fails.";
      parameters = `Assoc [];
      run = (fun _ -> Error "kaboom");
    }
  in
  let call =
    scripted
      [
        turn ~text:"using fail"
          ~tool_calls:
            [ { Llm.id = "t1"; name = "fail"; arguments = `Assoc [] } ]
          ();
        turn ~text:"done" ();
      ]
  in
  let result =
    Agent.run ~call ~tools:[ fail_tool ] ~emit ~messages:[ Llm.user "test" ] ()
  in
  (match result with
  | Ok r -> show_result r
  | Error e -> print_endline ("error: " ^ e));
  [%expect
    {|
    assistant: using fail
    ERR fail: kaboom
    assistant: done
    outcome: Finished
    user: test
    assistant: using fail (+1 tool calls)
    ERR fail: kaboom
    assistant: done
    |}]

let%expect_test "max steps reached" =
  let call =
    scripted
      [
        turn ~text:"tool1"
          ~tool_calls:
            [
              {
                Llm.id = "t1";
                name = "echo";
                arguments = `Assoc [ ("text", `String "a") ];
              };
            ]
          ();
        turn ~text:"tool2"
          ~tool_calls:
            [
              {
                Llm.id = "t2";
                name = "echo";
                arguments = `Assoc [ ("text", `String "b") ];
              };
            ]
          ();
        turn ~text:"tool3"
          ~tool_calls:
            [
              {
                Llm.id = "t3";
                name = "echo";
                arguments = `Assoc [ ("text", `String "c") ];
              };
            ]
          ();
        turn ~text:"done" ();
      ]
  in
  let result =
    Agent.run ~call ~tools:[ echo_tool ] ~emit
      ~messages:[ Llm.user "test" ]
      ~max_steps:2 ()
  in
  (match result with
  | Ok r -> show_result r
  | Error e -> print_endline ("error: " ^ e));
  [%expect
    {|
    assistant: tool1
    ok echo: echo: a
    assistant: tool2
    ok echo: echo: b
    outcome: Max_steps_reached
    user: test
    assistant: tool1 (+1 tool calls)
    ok echo: echo: a
    assistant: tool2 (+1 tool calls)
    ok echo: echo: b
    |}]
