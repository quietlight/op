open Op

let session_fields = [ ("sessionId", `String "s1"); ("parentId", `String "e2") ]

let metadata =
  [
    ("turns", `Int 3);
    ("toolCalls", `Int 2);
    ("toolErrors", `Int 0);
    ("tools", `List [ `String "bash" ]);
  ]

let show_stop = function Some s -> s | None -> "none"

let assistant ?usage () =
  Llm.Assistant { text = ""; thinking = None; tool_calls = []; usage }

let usage ~input ~output ~cache_read ~cache_creation =
  { Llm.input; output; cache_read; cache_creation }

let show_usage = function
  | None -> "none"
  | Some u ->
      Fmt.str "in=%d out=%d cacheRead=%d cacheWrite=%d" u.Llm.input u.output
        u.cache_read u.cache_creation

let%expect_test "tokens_line: cache counts shown only when nonzero" =
  print_endline
    (Run_report.tokens_line
       (usage ~input:200 ~output:80 ~cache_read:0 ~cache_creation:0));
  print_endline
    (Run_report.tokens_line
       (usage ~input:3 ~output:50 ~cache_read:1259 ~cache_creation:128));
  [%expect
    {|
    tokens: 200 in, 80 out
    tokens: 3 in, 50 out (1259 cache read, 128 cache write)
    |}]

let%expect_test "sum_usage: no usage-bearing turns is None" =
  print_endline (show_usage (Run_report.sum_usage []));
  print_endline
    (show_usage
       (Run_report.sum_usage [ Llm.user "hi"; assistant (); Llm.user "bye" ]));
  [%expect {|
    none
    none
    |}]

let%expect_test "sum_usage: totals all four fields across turns" =
  let history =
    [
      assistant
        ~usage:(usage ~input:3 ~output:50 ~cache_read:1000 ~cache_creation:128)
        ();
      Llm.user "more";
      assistant ();
      assistant
        ~usage:(usage ~input:5 ~output:20 ~cache_read:259 ~cache_creation:0)
        ();
    ]
  in
  print_endline (show_usage (Run_report.sum_usage history));
  [%expect {| in=8 out=70 cacheRead=1259 cacheWrite=128 |}]

let%expect_test "classify: finished" =
  let c = Run_report.classify (Ok ([], Agent.Finished)) in
  Fmt.pr "is_error=%b exit_code=%d stop_reason=%s@." c.is_error c.exit_code
    (show_stop c.stop_reason);
  [%expect {| is_error=false exit_code=0 stop_reason=finished |}]

let%expect_test "classify: max_steps" =
  let c = Run_report.classify (Ok ([], Agent.Max_steps_reached)) in
  Fmt.pr "is_error=%b exit_code=%d stop_reason=%s@." c.is_error c.exit_code
    (show_stop c.stop_reason);
  [%expect {| is_error=true exit_code=1 stop_reason=max_steps |}]

let%expect_test "classify: error" =
  let c = Run_report.classify (Error "boom" : (_, string) result) in
  Fmt.pr "is_error=%b exit_code=%d stop_reason=%s@." c.is_error c.exit_code
    (show_stop c.stop_reason);
  [%expect {| is_error=true exit_code=1 stop_reason=none |}]

let%expect_test "classify: gate_killed" =
  let c = Run_report.classify (Ok ([], Agent.Gate_killed "hostile")) in
  Fmt.pr "is_error=%b exit_code=%d stop_reason=%s@." c.is_error c.exit_code
    (show_stop c.stop_reason);
  [%expect {| is_error=true exit_code=1 stop_reason=gate_killed |}]

let%expect_test "classify: context_limit" =
  let c = Run_report.classify (Ok ([], Agent.Context_limit_reached)) in
  Fmt.pr "is_error=%b exit_code=%d stop_reason=%s@." c.is_error c.exit_code
    (show_stop c.stop_reason);
  [%expect {| is_error=true exit_code=1 stop_reason=context_limit |}]

let%expect_test "context_usage: last usage-bearing turn wins" =
  let history =
    [
      assistant
        ~usage:(usage ~input:10 ~output:5 ~cache_read:0 ~cache_creation:0)
        ();
      Llm.user "more";
      assistant
        ~usage:(usage ~input:80 ~output:20 ~cache_read:900 ~cache_creation:0)
        ();
      assistant ();
    ]
  in
  print_endline (show_usage (Run_report.context_usage history));
  print_endline (show_usage (Run_report.context_usage []));
  Fmt.pr "tokens=%d@."
    (Run_report.context_tokens
       (usage ~input:80 ~output:20 ~cache_read:900 ~cache_creation:8));
  [%expect
    {|
    in=80 out=20 cacheRead=900 cacheWrite=0
    none
    tokens=1008
    |}]

let%expect_test "context_fields: needs both a window and a reading" =
  let history =
    [
      assistant
        ~usage:
          (usage ~input:1000 ~output:200 ~cache_read:5000 ~cache_creation:0)
        ();
    ]
  in
  let show fields = print_endline (Yojson.Safe.to_string (`Assoc fields)) in
  show (Run_report.context_fields ~window:(Some 10000) history);
  show (Run_report.context_fields ~window:None history);
  show (Run_report.context_fields ~window:(Some 10000) [ assistant () ]);
  [%expect
    {|
    {"context":{"tokens":6200,"window":10000,"pct":0.62}}
    {}
    {}
    |}]

let%expect_test "sum_usage: base skips the replayed prefix" =
  let replayed =
    assistant
      ~usage:(usage ~input:900 ~output:90 ~cache_read:0 ~cache_creation:0)
  in
  let history =
    [
      replayed ();
      Llm.user "next";
      assistant
        ~usage:(usage ~input:10 ~output:2 ~cache_read:0 ~cache_creation:0)
        ();
    ]
  in
  print_endline (show_usage (Run_report.sum_usage history));
  print_endline (show_usage (Run_report.sum_usage ~base:2 history));
  [%expect
    {|
    in=910 out=92 cacheRead=0 cacheWrite=0
    in=10 out=2 cacheRead=0 cacheWrite=0
    |}]

let%expect_test "result: gate_killed envelope" =
  print_endline
    (Run_report.result ~answer:"partial" ~session_fields
       ~model:"anthropic/claude" ~input:3 ~output:5 ~cache_read:0 ~cache_write:0
       ~metadata ~outcome:(Agent.Gate_killed "hostile") ());
  [%expect
    {| {"answer":"partial","sessionId":"s1","parentId":"e2","model":"anthropic/claude","tokens":{"input":3,"output":5,"cacheRead":0,"cacheWrite":0},"turns":3,"toolCalls":2,"toolErrors":0,"tools":["bash"],"stopReason":"gate_killed","isError":true} |}]

let%expect_test "classify invariant: exit_code=0 iff not is_error" =
  let cases =
    [
      Ok ([], Agent.Finished);
      Ok ([], Agent.Max_steps_reached);
      (Error "e" : (_, string) result);
    ]
  in
  List.iter
    (fun r ->
      let c = Run_report.classify r in
      let holds = c.exit_code = 0 = not c.is_error in
      Fmt.pr "exit_code=%d is_error=%b invariant=%b@." c.exit_code c.is_error
        holds)
    cases;
  [%expect
    {|
    exit_code=0 is_error=false invariant=true
    exit_code=1 is_error=true invariant=true
    exit_code=1 is_error=true invariant=true
    |}]

let%expect_test "result: finished surfaces cache tokens" =
  print_endline
    (Run_report.result ~answer:"done" ~session_fields ~model:"anthropic/claude"
       ~input:3 ~output:50 ~cache_read:1259 ~cache_write:128 ~metadata
       ~outcome:Agent.Finished ());
  [%expect
    {| {"answer":"done","sessionId":"s1","parentId":"e2","model":"anthropic/claude","tokens":{"input":3,"output":50,"cacheRead":1259,"cacheWrite":128},"turns":3,"toolCalls":2,"toolErrors":0,"tools":["bash"],"stopReason":"finished","isError":false} |}]

let%expect_test "result: branch correction metadata is additive" =
  print_endline
    (Run_report.result ~answer:"done" ~session_fields ~model:"anthropic/claude"
       ~input:3 ~output:50 ~cache_read:0 ~cache_write:0
       ~metadata:
         ([
            ("branchCorrectedFrom", `String "a1");
            ("branchCorrectedTo", `String "u1");
          ]
         @ metadata)
       ~outcome:Agent.Finished ());
  [%expect
    {| {"answer":"done","sessionId":"s1","parentId":"e2","model":"anthropic/claude","tokens":{"input":3,"output":50,"cacheRead":0,"cacheWrite":0},"branchCorrectedFrom":"a1","branchCorrectedTo":"u1","turns":3,"toolCalls":2,"toolErrors":0,"tools":["bash"],"stopReason":"finished","isError":false} |}]

let%expect_test "result: max_steps keeps the partial answer, cache fields zero"
    =
  print_endline
    (Run_report.result ~answer:"partial work" ~session_fields
       ~model:"anthropic/claude" ~input:200 ~output:80 ~cache_read:0
       ~cache_write:0 ~metadata ~outcome:Agent.Max_steps_reached ());
  [%expect
    {| {"answer":"partial work","sessionId":"s1","parentId":"e2","model":"anthropic/claude","tokens":{"input":200,"output":80,"cacheRead":0,"cacheWrite":0},"turns":3,"toolCalls":2,"toolErrors":0,"tools":["bash"],"stopReason":"max_steps","isError":true} |}]

let%expect_test "error: with session and model" =
  print_endline
    (Run_report.error ~msg:"network failed" ~session_fields
       ~model:"anthropic/claude" ());
  [%expect
    {| {"error":"network failed","sessionId":"s1","parentId":"e2","model":"anthropic/claude","isError":true} |}]

let%expect_test "error: hint and metadata" =
  print_endline
    (Run_report.error ~msg:"no models.json" ~hint:"create one"
       ~metadata:[ ("branchCorrectedFrom", `String "a1") ]
       ());
  [%expect
    {| {"error":"no models.json","hint":"create one","branchCorrectedFrom":"a1","isError":true} |}]

let%expect_test "error: bare (early failure, no session or model)" =
  print_endline (Run_report.error ~msg:"no default model" ());
  [%expect {| {"error":"no default model","isError":true} |}]

let%expect_test "result: typed for --stream" =
  print_endline
    (Run_report.result ~typed:true ~answer:"done" ~session_fields
       ~model:"anthropic/claude" ~input:3 ~output:50 ~cache_read:0
       ~cache_write:0 ~metadata ~outcome:Agent.Finished ());
  [%expect
    {| {"type":"result","answer":"done","sessionId":"s1","parentId":"e2","model":"anthropic/claude","tokens":{"input":3,"output":50,"cacheRead":0,"cacheWrite":0},"turns":3,"toolCalls":2,"toolErrors":0,"tools":["bash"],"stopReason":"finished","isError":false} |}]

let%expect_test "error: typed for --stream" =
  print_endline (Run_report.error ~typed:true ~msg:"no default model" ());
  [%expect {| {"type":"result","error":"no default model","isError":true} |}]
