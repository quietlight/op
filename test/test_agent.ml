open Test_support

let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

let scripted turns =
  let remaining = ref turns in
  fun ~messages:_ ->
    match !remaining with
    | t :: rest ->
        remaining := rest;
        Ok t
    | [] -> Ok (turn ~text:"done" [])

let fake_tool ~name ~result : Op.Tool.t =
  { name; description = "fake tool"; parameters = `Assoc []; run = result }

let no_emit _ = ()

let is_tool_result content = function
  | Op.Llm.Tool tr -> tr.content = content
  | _ -> false

let test_agent_no_tools () =
  let call = scripted [ turn ~text:"hello there" [] ] in
  match
    Op.Agent.run ~call ~tools:[] ~emit:no_emit ~messages:[ Op.Llm.user "hi" ] ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _outcome) ->
      let tool_results =
        List.filter (function Op.Llm.Tool _ -> true | _ -> false) history
      in
      Alcotest.(check int) "no tool results" 0 (List.length tool_results);
      let final =
        List.exists
          (function Op.Llm.Assistant a -> a.text = "hello there" | _ -> false)
          history
      in
      Alcotest.(check bool) "final assistant text present" true final

let test_agent_runs_tool () =
  let ran_with = ref None in
  let tool =
    fake_tool ~name:"echo" ~result:(fun args ->
        ran_with := Some args;
        Op.Tool.text "tool-output")
  in
  let call =
    scripted
      [
        turn
          [
            tool_call ~id:"1" ~name:"echo"
              ~args:(`Assoc [ ("x", `String "y") ])
              ();
          ];
        turn ~text:"final answer" [];
      ]
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _outcome) ->
      Alcotest.(check bool) "tool was invoked" true (!ran_with <> None);
      Alcotest.(check bool)
        "tool result in history" true
        (List.exists (is_tool_result "tool-output") history);
      Alcotest.(check bool)
        "final answer in history" true
        (List.exists
           (function
             | Op.Llm.Assistant a -> a.text = "final answer" | _ -> false)
           history)

let test_agent_unknown_tool () =
  let call =
    scripted
      [ turn [ tool_call ~id:"1" ~name:"nope" () ]; turn ~text:"recovered" [] ]
  in
  match
    Op.Agent.run ~call ~tools:[] ~emit:no_emit ~messages:[ Op.Llm.user "hi" ] ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _outcome) ->
      let errored =
        List.exists
          (function
            | Op.Llm.Tool tr ->
                tr.is_error && contains tr.content "unknown tool"
            | _ -> false)
          history
      in
      Alcotest.(check bool) "unknown tool recorded as error" true errored

let test_agent_tool_error_preserved () =
  let tool = fake_tool ~name:"boom" ~result:(fun _ -> Error "kaboom") in
  let call =
    scripted [ turn [ tool_call ~id:"1" ~name:"boom" () ]; turn ~text:"ok" [] ]
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _outcome) ->
      let preserved =
        List.exists
          (function
            | Op.Llm.Tool tr -> tr.is_error && tr.content = "kaboom"
            | _ -> false)
          history
      in
      Alcotest.(check bool) "tool error preserved" true preserved

let test_agent_tool_exception_caught () =
  let tool =
    fake_tool ~name:"crashy" ~result:(fun _ ->
        ignore (Yojson.Safe.Util.member "x" (`String "not an object"));
        assert false)
  in
  let call =
    scripted
      [
        turn [ tool_call ~id:"1" ~name:"crashy" () ]; turn ~text:"recovered" [];
      ]
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.failf "agent crashed: %s" e
  | Ok (history, _outcome) ->
      let caught =
        List.exists
          (function
            | Op.Llm.Tool tr ->
                tr.is_error && contains tr.content "raised an exception"
            | _ -> false)
          history
      in
      Alcotest.(check bool) "tool exception caught as error" true caught

let test_agent_max_steps () =
  let tool = fake_tool ~name:"echo" ~result:(fun _ -> Op.Tool.text "out") in
  let call ~messages:_ = Ok (turn [ tool_call ~id:"1" ~name:"echo" () ]) in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit ~max_steps:2
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (_, outcome) ->
      Alcotest.(check bool)
        "max_steps reached" true
        (match outcome with Op.Agent.Max_steps_reached -> true | _ -> false)

let test_agent_context_limit () =
  let tool = fake_tool ~name:"echo" ~result:(fun _ -> Op.Tool.text "out") in
  let call ~messages:_ = Ok (turn [ tool_call ~id:"1" ~name:"echo" () ]) in
  let over_context history =
    List.length
      (List.filter (function Op.Llm.Assistant _ -> true | _ -> false) history)
    >= 2
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit ~over_context
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, outcome) ->
      Alcotest.(check bool)
        "context limit reached" true
        (match outcome with
        | Op.Agent.Context_limit_reached -> true
        | _ -> false);
      let calls =
        List.concat_map
          (function
            | Op.Llm.Assistant a ->
                List.map (fun (c : Op.Llm.tool_call) -> c.id) a.tool_calls
            | _ -> [])
          history
      in
      let answered =
        List.filter_map
          (function Op.Llm.Tool tr -> Some tr.id | _ -> None)
          history
      in
      Alcotest.(check int)
        "every tool call answered" (List.length calls) (List.length answered)

let test_agent_no_context_limit_by_default () =
  let call = scripted [ turn ~text:"done" [] ] in
  match
    Op.Agent.run ~call ~tools:[] ~emit:no_emit ~messages:[ Op.Llm.user "hi" ] ()
  with
  | Error e -> Alcotest.fail e
  | Ok (_, outcome) ->
      Alcotest.(check bool)
        "finished" true
        (match outcome with Op.Agent.Finished -> true | _ -> false)

let test_agent_event_order () =
  let tool = fake_tool ~name:"echo" ~result:(fun _ -> Op.Tool.text "out") in
  let call =
    scripted
      [
        turn ~text:"working" [ tool_call ~id:"1" ~name:"echo" () ];
        turn ~text:"done" [];
      ]
  in
  let events = ref [] in
  let emit e = events := e :: !events in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit ~messages:[ Op.Llm.user "hi" ] ()
  with
  | Error e -> Alcotest.fail e
  | Ok _ ->
      let shape =
        List.rev_map
          (function
            | Op.Agent.Assistant_turn t -> "assistant:" ^ t.text
            | Op.Agent.Tool_finished tr -> "finished:" ^ tr.content
            | Op.Agent.Tool_ids_repaired -> "repaired")
          !events
      in
      Alcotest.(check (list string))
        "events in history order"
        [ "assistant:working"; "finished:out"; "assistant:done" ]
        shape

let has_sub ~needle haystack =
  let n = String.length needle and m = String.length haystack in
  let rec go i =
    i + n <= m && (String.sub haystack i n = needle || go (i + 1))
  in
  n = 0 || go 0

let test_system_prompt () =
  let cwd = "/tmp/work" and date = "2024-01-02" in
  let prompt =
    Op.Agent.system_prompt ~cwd ~date
      ~tools:[ "read"; "write"; "edit"; "glob"; "nu" ]
  in
  let contains sub = has_sub ~needle:sub prompt in
  Alcotest.(check bool) "contains cwd" true (contains cwd);
  Alcotest.(check bool) "contains date" true (contains date);
  Alcotest.(check bool)
    "contains default_system" true
    (contains Op.Agent.default_system);
  Alcotest.(check bool) "mentions the nu tool" true (contains "nu tool")

let test_system_prompt_no_exec () =
  let prompt =
    Op.Agent.system_prompt ~cwd:"/w" ~date:"2024-01-02"
      ~tools:[ "read"; "write"; "edit"; "glob" ]
  in
  Alcotest.(check bool)
    "does not offer nu" false
    (has_sub ~needle:"nu tool" prompt);
  Alcotest.(check bool)
    "still describes file tools" true
    (has_sub ~needle:"read" prompt)

let echo_tool = fake_tool ~name:"echo" ~result:(fun _ -> Op.Tool.text "out")

let call_ids history =
  List.concat_map
    (function
      | Op.Llm.Assistant a ->
          List.map (fun (tc : Op.Llm.tool_call) -> tc.id) a.tool_calls
      | _ -> [])
    history

let result_ids history =
  List.filter_map
    (function Op.Llm.Tool tr -> Some tr.Op.Llm.id | _ -> None)
    history

let distinct xs = List.length (List.sort_uniq compare xs) = List.length xs

let run_ids ?(messages = [ Op.Llm.user "hi" ]) turns =
  let repaired = ref 0 in
  let emit = function Op.Agent.Tool_ids_repaired -> incr repaired | _ -> () in
  match
    Op.Agent.run ~call:(scripted turns) ~tools:[ echo_tool ] ~emit ~messages ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _) -> (history, !repaired)

let test_ids_cross_turn_duplicates () =
  let dup () = turn [ tool_call ~id:"functions.bash:0" ~name:"echo" () ] in
  let history, repaired = run_ids [ dup (); dup (); turn ~text:"done" [] ] in
  let calls = call_ids history in
  Alcotest.(check int) "two calls" 2 (List.length calls);
  Alcotest.(check bool) "call ids distinct" true (distinct calls);
  Alcotest.(check (list string))
    "results mirror calls" calls (result_ids history);
  Alcotest.(check string)
    "first id preserved" "functions.bash:0" (List.hd calls);
  Alcotest.(check int) "warned exactly once" 1 repaired

let test_ids_within_turn_duplicates () =
  let history, _ =
    run_ids
      [
        turn
          [
            tool_call ~id:"dup" ~name:"echo" ();
            tool_call ~id:"dup" ~name:"echo" ();
          ];
        turn ~text:"done" [];
      ]
  in
  let calls = call_ids history in
  Alcotest.(check int) "two calls" 2 (List.length calls);
  Alcotest.(check bool) "call ids distinct" true (distinct calls);
  Alcotest.(check (list string))
    "results mirror calls" calls (result_ids history)

let test_ids_empty_rewritten () =
  let history, repaired =
    run_ids [ turn [ tool_call ~id:"" ~name:"echo" () ]; turn ~text:"done" [] ]
  in
  (match call_ids history with
  | [ id ] -> Alcotest.(check bool) "empty id replaced" true (id <> "")
  | _ -> Alcotest.fail "expected one tool call");
  Alcotest.(check int) "warned once" 1 repaired

let test_ids_clean_preserved () =
  let history, repaired =
    run_ids
      [
        turn [ tool_call ~id:"call_a" ~name:"echo" () ];
        turn [ tool_call ~id:"call_b" ~name:"echo" () ];
        turn ~text:"done" [];
      ]
  in
  Alcotest.(check (list string))
    "unique provider ids untouched" [ "call_a"; "call_b" ] (call_ids history);
  Alcotest.(check int) "no repair" 0 repaired

let test_ids_resume_seeding () =
  let prior =
    [
      Op.Llm.user "earlier";
      Op.Llm.Assistant (turn [ tool_call ~id:"x" ~name:"echo" () ]);
      Op.Llm.Tool
        {
          Op.Llm.id = "x";
          name = "echo";
          content = "prev";
          is_error = false;
          details = None;
        };
    ]
  in
  let history, repaired =
    run_ids ~messages:prior
      [ turn [ tool_call ~id:"x" ~name:"echo" () ]; turn ~text:"done" [] ]
  in
  (match call_ids history with
  | [ "x"; fresh ] ->
      Alcotest.(check bool) "new id rewritten" true (fresh <> "x")
  | other ->
      Alcotest.failf "unexpected call ids: [%s]" (String.concat "; " other));
  Alcotest.(check int) "warned once" 1 repaired

let test_gate_deny_continues () =
  let ran = ref false in
  let tool =
    fake_tool ~name:"echo" ~result:(fun _ ->
        ran := true;
        Op.Tool.text "out")
  in
  let call =
    scripted
      [ turn [ tool_call ~id:"1" ~name:"echo" () ]; turn ~text:"adapted" [] ]
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit
      ~gate:(fun _ -> Op.Gate.Deny "nope")
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, outcome) ->
      Alcotest.(check bool) "tool did not run" false !ran;
      Alcotest.(check bool)
        "blocked result recorded" true
        (List.exists
           (function
             | Op.Llm.Tool tr ->
                 tr.is_error && contains tr.content "blocked by gate"
             | _ -> false)
           history);
      Alcotest.(check bool)
        "loop continued to final answer" true
        (List.exists
           (function Op.Llm.Assistant a -> a.text = "adapted" | _ -> false)
           history);
      Alcotest.(check bool)
        "finished normally" true
        (match outcome with Op.Agent.Finished -> true | _ -> false)

let test_gate_warn_appends () =
  let tool = fake_tool ~name:"echo" ~result:(fun _ -> Op.Tool.text "out") in
  let call =
    scripted [ turn [ tool_call ~id:"1" ~name:"echo" () ]; turn ~text:"ok" [] ]
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit
      ~gate:(fun _ -> Op.Gate.Warn "careful")
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _) ->
      Alcotest.(check bool)
        "warning appended to result" true
        (List.exists
           (function
             | Op.Llm.Tool tr ->
                 (not tr.is_error) && contains tr.content "out"
                 && contains tr.content "[gate] warning: careful"
             | _ -> false)
           history)

let test_gate_code_red_kills () =
  let ran = ref false in
  let tool =
    fake_tool ~name:"echo" ~result:(fun _ ->
        ran := true;
        Op.Tool.text "out")
  in
  let call =
    scripted
      [
        turn
          [
            tool_call ~id:"1" ~name:"echo" (); tool_call ~id:"2" ~name:"echo" ();
          ];
        turn ~text:"never reached" [];
      ]
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit:no_emit
      ~gate:(fun _ -> Op.Gate.Code_red "hostile")
      ~messages:[ Op.Llm.user "hi" ]
      ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, outcome) ->
      Alcotest.(check bool) "tool did not run" false !ran;
      Alcotest.(check bool)
        "outcome is Gate_killed" true
        (match outcome with
        | Op.Agent.Gate_killed m -> m = "hostile"
        | _ -> false);
      Alcotest.(check (list string))
        "every pending call answered" (call_ids history) (result_ids history);
      Alcotest.(check bool)
        "results are gate terminations" true
        (List.for_all
           (function
             | Op.Llm.Tool tr ->
                 tr.is_error && contains tr.content "run terminated by gate"
             | _ -> true)
           history);
      Alcotest.(check bool)
        "run stopped before the next turn" false
        (List.exists
           (function
             | Op.Llm.Assistant a -> a.text = "never reached" | _ -> false)
           history)

let test_tool_details_survive_dispatch () =
  let details = `Assoc [ ("nextId", `Int 3) ] in
  let tool =
    fake_tool ~name:"echo" ~result:(fun _ ->
        Ok { Op.Tool.content = "out"; details = Some details })
  in
  let call = scripted [ turn [ tool_call ~id:"1" ~name:"echo" () ] ] in
  let seen = ref None in
  let emit = function
    | Op.Agent.Tool_finished (tr : Op.Llm.tool_result) -> seen := Some tr
    | _ -> ()
  in
  match
    Op.Agent.run ~call ~tools:[ tool ] ~emit ~messages:[ Op.Llm.user "hi" ] ()
  with
  | Error e -> Alcotest.fail e
  | Ok (history, _) ->
      let emitted = Option.get !seen in
      Alcotest.(check bool)
        "details on the emitted result" true
        (emitted.details = Some details);
      let in_history =
        List.exists
          (function
            | Op.Llm.Tool (tr : Op.Llm.tool_result) -> tr.details = Some details
            | _ -> false)
          history
      in
      Alcotest.(check bool) "details in history" true in_history

let test_details_never_reach_the_wire () =
  let with_details : Op.Llm.message =
    Tool
      {
        id = "t1";
        name = "echo";
        content = "out";
        is_error = false;
        details = Some (`Assoc [ ("secret", `String "state") ]);
      }
  in
  let without =
    Op.Llm.Tool
      {
        id = "t1";
        name = "echo";
        content = "out";
        is_error = false;
        details = None;
      }
  in
  let body messages =
    Op.Anthropic_adapter.For_testing.build_body ~model:"m" ~tools:[] ~messages
      ()
  in
  Alcotest.(check string)
    "identical to a details-free result" (body [ without ])
    (body [ with_details ])

let suite =
  ( "agent",
    [
      ("system_prompt embeds env", `Quick, test_system_prompt);
      ( "system_prompt reflects a no-exec tool set",
        `Quick,
        test_system_prompt_no_exec );
      ("gate deny continues", `Quick, test_gate_deny_continues);
      ("gate warn appends", `Quick, test_gate_warn_appends);
      ("gate code_red kills", `Quick, test_gate_code_red_kills);
      ("no tools", `Quick, test_agent_no_tools);
      ("runs tool", `Quick, test_agent_runs_tool);
      ("unknown tool", `Quick, test_agent_unknown_tool);
      ("tool error preserved", `Quick, test_agent_tool_error_preserved);
      ("tool exception caught", `Quick, test_agent_tool_exception_caught);
      ("max_steps caps loop", `Quick, test_agent_max_steps);
      ("context limit stops loop", `Quick, test_agent_context_limit);
      ( "no context limit by default",
        `Quick,
        test_agent_no_context_limit_by_default );
      ("events in history order", `Quick, test_agent_event_order);
      ("ids: cross-turn duplicates", `Quick, test_ids_cross_turn_duplicates);
      ("ids: within-turn duplicates", `Quick, test_ids_within_turn_duplicates);
      ("ids: empty rewritten", `Quick, test_ids_empty_rewritten);
      ("ids: clean preserved", `Quick, test_ids_clean_preserved);
      ("ids: resume seeding", `Quick, test_ids_resume_seeding);
      ( "tool details survive dispatch",
        `Quick,
        test_tool_details_survive_dispatch );
      ("details never reach the wire", `Quick, test_details_never_reach_the_wire);
    ] )
