open Op

(* Human-transcript renderer tests. Render.entry/footer are pure — they take a
   session entry (or the result envelope) and return lines — so the fixtures
   here are the JSON op actually writes, taken from lib/session.ml's shapes, and
   the assertion is the rendered text. Colour is off throughout: the ANSI codes
   are the caller's concern, and plain text is what stays readable in a diff.

   The same in-process style as expect_run_json.ml. *)

let show lines = List.iter print_endline lines
let renderer () = Render.v ~color:false ~cwd:"/work" ()
let json s = Yojson.Safe.from_string s

(* One assistant turn: [text] (may be empty) plus a list of toolCall blocks. *)
let assistant ?(text = "") ?(usage = 42) calls =
  let text_block =
    if text = "" then []
    else [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
  in
  `Assoc
    [
      ("type", `String "message");
      ( "message",
        `Assoc
          [
            ("role", `String "assistant");
            ("model", `String "m-1");
            ("usage", `Assoc [ ("output", `Int usage) ]);
            ("content", `List (text_block @ calls));
          ] );
    ]

let call ~id ~name args =
  `Assoc
    [
      ("type", `String "toolCall");
      ("id", `String id);
      ("name", `String name);
      ("arguments", json args);
    ]

let result ?(is_error = false) ?details ~id ~name text =
  let details =
    match details with Some d -> [ ("details", json d) ] | None -> []
  in
  `Assoc
    [
      ("type", `String "message");
      ( "message",
        `Assoc
          ([
             ("role", `String "toolResult");
             ("toolCallId", `String id);
             ("toolName", `String name);
             ( "content",
               `List
                 [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
             );
             ("isError", `Bool is_error);
           ]
          @ details) );
    ]

(* A numbered read result, [n] lines, in the shape lib/read.ml emits. *)
let numbered n =
  String.concat "\n"
    (List.init n (fun i -> Fmt.str "%6d\tline %d" (i + 1) (i + 1)))

let%expect_test "session header, model change, and a plain assistant turn" =
  let t = renderer () in
  show
    (Render.entry t
       (json
          {|{"type":"session","id":"s-1","cwd":"/work","timestamp":"2026-08-11T03:00:00.000Z"}|}));
  show
    (Render.entry t
       (json {|{"type":"model_change","provider":"openai","modelId":"m-1"}|}));
  show (Render.entry t (assistant ~text:"Done." ~usage:12 []));
  [%expect
    {|
    ══ session s-1 ══
    cwd:  /work
    time: 2026-08-11T03:00:00.000Z
    [model → openai/m-1]

    ### ASSISTANT [m-1]  out=12 tok
    Done.
    |}]

let%expect_test "the user echo is off for a live run and on for a replay" =
  let entry =
    json
      {|{"type":"message","message":{"role":"user","content":[{"type":"text","text":"do the thing"}]}}|}
  in
  show (Render.entry (renderer ()) entry);
  print_endline "--- replay:";
  show
    (Render.entry (Render.v ~color:false ~cwd:"/work" ~echo_user:true ()) entry);
  [%expect {|
    --- replay:

    ### USER
    do the thing
    |}]

let%expect_test "read: the call names the slice, the result names the file" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant
          [
            call ~id:"c1" ~name:"read"
              {|{"path":"/work/lib/session.ml","offset":1,"limit":240}|};
          ]));
  show (Render.entry t (result ~id:"c1" ~name:"read" (numbered 129)));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → read  lib/session.ml  lines 1-240
      ← read  lib/session.ml  129 lines
             1	line 1
             2	line 2
             3	line 3
             4	line 4
             5	line 5
        …  129 lines total
    |}]

let%expect_test "read: a call with no slice, and a result the preview fits" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant
          [ call ~id:"c1" ~name:"read" {|{"path":"/elsewhere/notes.md"}|} ]));
  show (Render.entry t (result ~id:"c1" ~name:"read" (numbered 3)));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → read  /elsewhere/notes.md
      ← read  /elsewhere/notes.md  3 lines
             1	line 1
             2	line 2
             3	line 3
    |}]

(* The preview boundary: 5 lines is the budget, so 5 shows whole and 6 gets the
   count footer. *)
let%expect_test "preview boundary at exactly the budget" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant [ call ~id:"c1" ~name:"glob" {|{"pattern":"*.ml"}|} ]));
  let paths n =
    String.concat "\n" (List.init n (fun i -> Fmt.str "/work/lib/f%d.ml" i))
  in
  show (Render.entry t (result ~id:"c1" ~name:"glob" (paths 5)));
  show (Render.entry t (result ~id:"c1" ~name:"glob" (paths 6)));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → glob  *.ml
      ← glob  5 paths
        lib/f0.ml
        lib/f1.ml
        lib/f2.ml
        lib/f3.ml
        lib/f4.ml
      ← glob  6 paths
        lib/f0.ml
        lib/f1.ml
        lib/f2.ml
        lib/f3.ml
        lib/f4.ml
        …  6 lines total
    |}]

(* Two degradations that keep the nu twin (nu_modules/pi-session-view.nu)
   honest: glob's default root is not worth a column, and a result whose call
   was never seen has no path to name. *)
let%expect_test "a glob rooted at the cwd, and a result with no call" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant
          [
            call ~id:"c1" ~name:"glob" {|{"pattern":"*.ml","path":"/work"}|};
            call ~id:"c2" ~name:"glob" {|{"pattern":"*.ml","path":"/work/lib"}|};
          ]));
  show (Render.entry t (result ~id:"orphan" ~name:"read" "one\ntwo"));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → glob  *.ml
      → glob  *.ml  in lib
      ← read  2 lines
        one
        two
    |}]

let%expect_test "glob with a search root, write, and edit" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant
          [
            call ~id:"c1" ~name:"glob"
              {|{"pattern":"**/*.mli","path":"/work/lib"}|};
            call ~id:"c2" ~name:"write"
              {|{"path":"/work/lib/render.ml","content":"let x = 1\n"}|};
            call ~id:"c3" ~name:"edit"
              {|{"path":"/work/bin/main.ml","old_string":"let stream_sink mode =","new_string":"let sink mode =","replace_all":true}|};
          ]));
  show
    (Render.entry t
       (result ~id:"c2" ~name:"write" "Wrote 10 bytes to /work/lib/render.ml"));
  show
    (Render.entry t
       (result ~id:"c3" ~name:"edit"
          "Made 2 replacement(s) in /work/bin/main.ml"));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → glob  **/*.mli  in lib
      → write lib/render.ml  (10 B)
      → edit  bin/main.ml  let stream_sink mode = → let sink mode =  (all)
      ← write Wrote 10 bytes to /work/lib/render.ml
      ← edit  Made 2 replacement(s) in /work/bin/main.ml
    |}]

let%expect_test "nu: the script is shown, the exit status is lifted out" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant
          [
            call ~id:"c1" ~name:"nu"
              {|{"script":"ls lib\n| where size > 10kb\n| get name","timeout":120}|};
          ]));
  show
    (Render.entry t
       (result ~id:"c1" ~name:"nu"
          "config.ml\nconfig.mli\nsession.ml\n(exit code: 0)"));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → nu
        ls lib
        | where size > 10kb
        | get name
      ← nu    exit 0
        config.ml
        config.mli
        session.ml
    |}]

(* A nu result whose last line only *looks* like the exit footer must not be
   sliced as one — a truncated log is exactly the case Render promises never to
   raise on. Each of these is a non-match, so the line stays in the body and the
   summary falls back to "done". *)
let%expect_test "a malformed exit footer is left alone rather than parsed" =
  let t = renderer () in
  let nu text = Render.entry t (result ~id:"c1" ~name:"nu" text) in
  show (nu "out\n(exit code: ");
  show (nu "out\n(exit code: )");
  show (nu "out\n(exit code: 0");
  [%expect
    {|
      ← nu    done
        out
        (exit code:
      ← nu    done
        out
        (exit code: )
      ← nu    done
        out
        (exit code: 0
    |}]

let%expect_test "an error result is tagged and gets the long preview" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant [ call ~id:"c1" ~name:"nu" {|{"script":"open /nope"}|} ]));
  let body = String.concat "\n" (List.init 20 (fun i -> Fmt.str "err %d" i)) in
  show
    (Render.entry t
       (result ~is_error:true ~id:"c1" ~name:"nu" (body ^ "\n(exit code: 1)")));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → nu
        open /nope
      ← nu    [error] exit 1
        err 0
        err 1
        err 2
        err 3
        err 4
        err 5
        err 6
        err 7
        err 8
        err 9
        err 10
        err 11
        err 12
        err 13
        err 14
        …  20 lines total
    |}]

let%expect_test
    "an unknown tool degrades to a k=v summary, unknown entries to nothing" =
  let t = renderer () in
  show
    (Render.entry t
       (assistant
          [
            call ~id:"c1" ~name:"grep"
              {|{"pattern":"let v","path":"/work/lib"}|};
          ]));
  show
    (Render.entry t
       (result ~id:"c1" ~name:"grep" "lib/render.ml:48:let v ~color"));
  show
    (Render.entry t
       (json {|{"type":"thinking_level_change","thinkingLevel":"off"}|}));
  show (Render.entry t (json {|{"type":"compaction","summary":"…"}|}));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      → grep  pattern=let v, path=/work/lib
      ← grep  1 lines
        lib/render.ml:48:let v ~color
    |}]

let%expect_test "thinking blocks are shown, capped" =
  let t = renderer () in
  let thinking text =
    `Assoc [ ("type", `String "thinking"); ("thinking", `String text) ]
  in
  show
    (Render.entry t
       (assistant ~text:"Here is the answer."
          [
            thinking
              (String.concat "\n" (List.init 7 (fun i -> Fmt.str "step %d" i)));
          ]));
  [%expect
    {|
    ### ASSISTANT [m-1]  out=42 tok
      step 0
      step 1
      step 2
      step 3
      step 4
      …  7 lines of thinking
    Here is the answer.
    |}]

let%expect_test "footer: a finished run, with and without a context window" =
  let t = renderer () in
  let env ?context () =
    let context =
      match context with Some c -> [ ("context", json c) ] | None -> []
    in
    `Assoc
      ([
         ("answer", `String "ok");
         ("sessionId", `String "s-1");
         ("parentId", `String "e-9");
         ("model", `String "openai/m-1");
         ( "tokens",
           `Assoc
             [
               ("input", `Int 41235);
               ("output", `Int 3100);
               ("cacheRead", `Int 38900);
               ("cacheWrite", `Int 0);
             ] );
         ("turns", `Int 4);
         ("toolCalls", `Int 11);
         ("toolErrors", `Int 1);
       ]
      @ context
      @ [ ("stopReason", `String "finished"); ("isError", `Bool false) ])
  in
  show
    (Render.footer t
       (env ~context:{|{"tokens":44300,"window":200000,"pct":0.22}|} ()));
  show (Render.footer t (env ()));
  [%expect
    {|
    ────────────────────────────────────────────────────────
    finished  ·  4 turns  ·  11 tool calls  ·  1 tool error
    tokens: 41235 in, 3100 out (38900 cache read, 0 cache write)
    context: 44.3k / 200.0k  [==--------]  22%
    resume: op --sessionId s-1 --parentId e-9 "prompt"

    ────────────────────────────────────────────────────────
    finished  ·  4 turns  ·  11 tool calls  ·  1 tool error
    tokens: 41235 in, 3100 out (38900 cache read, 0 cache write)
    resume: op --sessionId s-1 --parentId e-9 "prompt"
    |}]

let%expect_test "footer: a truncated run and a hard failure" =
  let t = renderer () in
  show
    (Render.footer t
       (Run_report.result_json ~answer:"partial"
          ~session_fields:[ ("sessionId", `String "s-1") ]
          ~model:"openai/m-1" ~input:10 ~output:2 ~cache_read:0 ~cache_write:0
          ~metadata:[ ("turns", `Int 1); ("toolCalls", `Int 1) ]
          ~outcome:Agent.Max_steps_reached ()));
  show
    (Render.footer t
       (Run_report.error_json ~msg:"no models.json found"
          ~hint:"create ~/.op_global/agent/models.json" ()));
  [%expect
    {|
    ────────────────────────────────────────────────────────
    max_steps  ·  1 turn  ·  1 tool call
    tokens: 10 in, 2 out
    session: s-1
    error: no models.json found
    hint: create ~/.op_global/agent/models.json
    |}]
