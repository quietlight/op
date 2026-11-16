open Op

(* Pure-JSON renderer tests for `op --list --json`. The renderers take data
   values and return one compact JSON line; bin/main.ml only adds data fetching,
   stderr diagnostics, and exit codes. So we build fixtures directly and assert the
   serialised string — the same in-process style as expect_agent_loop.ml. *)

let session ~id ~timestamp ~summary ~count =
  { Session_list.id; timestamp; summary; count }

let entry ~id ~parent_id ~role ~preview ~is_fork =
  { Session_list.id; parent_id; role; preview; is_fork }

let%expect_test "error envelope" =
  print_endline (List_report.error "boom");
  [%expect {| {"error":"boom","isError":true} |}]

let%expect_test "error envelope with hint" =
  print_endline (List_report.error ~hint:"try this" "boom");
  [%expect {| {"error":"boom","hint":"try this","isError":true} |}]

let%expect_test "sessions: non-empty" =
  print_endline
    (List_report.sessions
       [
         session ~id:"abc" ~timestamp:"2026-06-29T01:43:25.748Z"
           ~summary:"do the thing" ~count:2;
         session ~id:"def" ~timestamp:"2026-06-29T02:00:00.000Z"
           ~summary:"another" ~count:5;
       ]);
  [%expect
    {| {"kind":"sessions","sessions":[{"id":"abc","timestamp":"2026-06-29T01:43:25.748Z","count":2,"summary":"do the thing"},{"id":"def","timestamp":"2026-06-29T02:00:00.000Z","count":5,"summary":"another"}]} |}]

let%expect_test "sessions: empty is a success, not an error" =
  print_endline (List_report.sessions []);
  [%expect {| {"kind":"sessions","sessions":[]} |}]

let%expect_test "messages: root and forked entries" =
  print_endline
    (List_report.messages ~session_id:"s1"
       [
         entry ~id:"e1" ~parent_id:None ~role:"user" ~preview:"hi"
           ~is_fork:false;
         entry ~id:"e2" ~parent_id:(Some "e1") ~role:"assistant"
           ~preview:"hello" ~is_fork:true;
       ]);
  [%expect
    {| {"kind":"messages","sessionId":"s1","messages":[{"id":"e1","parentId":null,"role":"user","preview":"hi","isFork":false},{"id":"e2","parentId":"e1","role":"assistant","preview":"hello","isFork":true}]} |}]

let%expect_test "messages: empty" =
  print_endline (List_report.messages ~session_id:"s1" []);
  [%expect {| {"kind":"messages","sessionId":"s1","messages":[]} |}]

let%expect_test "models: one object per provider/model pair" =
  print_endline
    (List_report.models
       [ ("openai", "gpt-5"); ("anthropic", "claude"); ("anthropic", "haiku") ]);
  [%expect
    {| {"kind":"models","models":[{"provider":"openai","model":"gpt-5"},{"provider":"anthropic","model":"claude"},{"provider":"anthropic","model":"haiku"}]} |}]

let%expect_test "models: empty list" =
  print_endline (List_report.models []);
  [%expect {| {"kind":"models","models":[]} |}]

let%expect_test "default: success" =
  print_endline (List_report.default ~provider:"anthropic" ~model:"claude");
  [%expect {| {"kind":"default","provider":"anthropic","model":"claude"} |}]
