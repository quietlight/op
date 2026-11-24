open Test_support

let read_messages ~path ?leaf () =
  Result.bind (Op.Session_read.load path) (fun t ->
      Op.Session_read.messages t ?leaf ())

let read_branch_point ~path ~leaf =
  Result.bind (Op.Session_read.load path) (fun t ->
      Op.Session_read.resolve_branch_point t ~leaf)

let leaf_ids ~path =
  match Op.Session_read.load path with
  | Ok t -> Op.Session_read.leaf_ids t
  | Error _ -> []

let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

let contains haystack needle =
  let hn = String.length haystack and nn = String.length needle in
  let rec at i =
    i + nn <= hn && (String.sub haystack i nn = needle || at (i + 1))
  in
  nn = 0 || at 0

let read_jsonl path =
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun l -> String.trim l <> "")
  |> List.map Yojson.Safe.from_string

let member = Yojson.Safe.Util.member
let str field json = Yojson.Safe.Util.(member field json |> to_string)

let write_sample_session dir =
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic" ~model:"claude-x"
        ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"do the thing";
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-x"
    (turn ~text:"on it"
       [
         tool_call ~id:"call_1" ~name:"bash"
           ~args:(`Assoc [ ("command", `String "ls") ])
           ();
       ]);
  Op.Session.append_tool_result session
    {
      Op.Llm.id = "call_1";
      name = "bash";
      content = "file.txt";
      is_error = false;
      details = None;
    };
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-x"
    (turn ~text:"done"
       ~usage:
         (Some { input = 12; output = 34; cache_read = 7; cache_creation = 5 })
       []);
  let path = Op.Session.path session in
  Op.Session.close session;
  path

let write_two_tool_session dir =
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic" ~model:"m" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"do two things";
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"m"
    (turn
       [
         tool_call ~id:"call_1" ~name:"bash"
           ~args:(`Assoc [ ("command", `String "first") ])
           ();
       ]);
  Op.Session.append_tool_result session
    {
      Op.Llm.id = "call_1";
      name = "bash";
      content = "one";
      is_error = false;
      details = None;
    };
  let first_tool_result_id = Option.get (Op.Session.leaf session) in
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"m"
    (turn
       [
         tool_call ~id:"call_2" ~name:"bash"
           ~args:(`Assoc [ ("command", `String "second") ])
           ();
       ]);
  let second_tool_use_id = Option.get (Op.Session.leaf session) in
  Op.Session.append_tool_result session
    {
      Op.Llm.id = "call_2";
      name = "bash";
      content = "two";
      is_error = false;
      details = None;
    };
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"m" (turn ~text:"done" []);
  let path = Op.Session.path session in
  Op.Session.close session;
  (path, first_tool_result_id, second_tool_use_id)

let load_messages path ?leaf () =
  match read_messages ~path ?leaf () with
  | Ok ms -> ms
  | Error e -> Alcotest.fail e

let test_session_load_round_trip () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  match load_messages path () with
  | [
   Op.Llm.User { text = u; _ };
   Op.Llm.Assistant a;
   Op.Llm.Tool tr;
   Op.Llm.Assistant final;
  ] ->
      Alcotest.(check string) "user text" "do the thing" u;
      Alcotest.(check string) "assistant text" "on it" a.text;
      (match a.tool_calls with
      | [ tc ] ->
          Alcotest.(check string) "tool call id" "call_1" tc.id;
          Alcotest.(check string) "tool call name" "bash" tc.name;
          Alcotest.(check string)
            "tool call arguments" "ls"
            (str "command" tc.arguments)
      | _ -> Alcotest.fail "expected exactly one tool call");
      Alcotest.(check string) "tool result id" "call_1" tr.id;
      Alcotest.(check string) "tool result name" "bash" tr.name;
      Alcotest.(check string) "tool result content" "file.txt" tr.content;
      Alcotest.(check bool) "tool result is_error" false tr.is_error;
      Alcotest.(check string) "final text" "done" final.text;
      Alcotest.(check int) "final tool calls" 0 (List.length final.tool_calls)
  | ms -> Alcotest.failf "unexpected message list (len %d)" (List.length ms)

let test_session_thinking_round_trip () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"moonshot" ~model:"kimi-k2.5"
        ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"hi";
  Op.Session.append_assistant session ~api:"openai-chat" ~provider:"moonshot"
    ~model:"kimi-k2.5"
    (turn ~text:"answer"
       ~thinking:
         (Some
            (Op.Llm.Reasoning
               { text = "let me think"; signature = Some "sig-abc" }))
       []);
  let path = Op.Session.path session in
  Op.Session.close session;
  match load_messages path () with
  | [ Op.Llm.User _; Op.Llm.Assistant a ] -> (
      Alcotest.(check string) "assistant text" "answer" a.text;
      match a.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string))
            "thinking signature" (Some "sig-abc") signature;
          Alcotest.(check string) "thinking text" "let me think" text
      | _ -> Alcotest.fail "expected thinking to round-trip")
  | ms -> Alcotest.failf "unexpected message list (len %d)" (List.length ms)

let test_session_thinking_empty_sig () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"minimax"
        ~model:"MiniMax-M2.7" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"hi";
  Op.Session.append_assistant session ~api:"openai-chat" ~provider:"minimax"
    ~model:"MiniMax-M2.7"
    (turn ~text:"answer"
       ~thinking:
         (Some (Op.Llm.Reasoning { text = "inline think"; signature = None }))
       []);
  let path = Op.Session.path session in
  Op.Session.close session;
  let file_contents = In_channel.with_open_text path In_channel.input_all in
  Alcotest.(check bool)
    "no thinkingSignature key" true
    (not (contains file_contents "\"thinkingSignature\""));
  match load_messages path () with
  | [ Op.Llm.User _; Op.Llm.Assistant a ] -> (
      Alcotest.(check string) "assistant text" "answer" a.text;
      match a.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "thinking signature" None signature;
          Alcotest.(check string) "thinking text" "inline think" text
      | _ -> Alcotest.fail "expected thinking to round-trip")
  | ms -> Alcotest.failf "unexpected message list (len %d)" (List.length ms)

let test_session_thinking_redacted () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic"
        ~model:"claude-opus-4-8" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"hi";
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-opus-4-8"
    (turn ~text:"answer" ~thinking:(Some (Op.Llm.Redacted "ENCRYPTED")) []);
  let path = Op.Session.path session in
  Op.Session.close session;
  match load_messages path () with
  | [ Op.Llm.User _; Op.Llm.Assistant a ] -> (
      Alcotest.(check string) "assistant text" "answer" a.text;
      match a.thinking with
      | Some (Redacted data) ->
          Alcotest.(check string) "redacted data" "ENCRYPTED" data
      | _ -> Alcotest.fail "expected redacted thinking to round-trip")
  | ms -> Alcotest.failf "unexpected message list (len %d)" (List.length ms)

let test_session_load_leaf () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let user_entry =
    read_jsonl path |> List.find (fun e -> member "type" e = `String "message")
  in
  let leaf = str "id" user_entry in
  match load_messages path ~leaf () with
  | [ Op.Llm.User { text = u; _ } ] ->
      Alcotest.(check string) "history up to leaf" "do the thing" u
  | ms ->
      Alcotest.failf "expected only the user message, got %d" (List.length ms)

let test_session_load_unknown_leaf () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  match read_messages ~path ~leaf:"deadbeef" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected an error for an unknown leaf id"

(* Raw JSONL parentId cycle (a -> b -> a) with tool-call entries so both
   [messages] and [resolve_branch_point] walk it. *)
let write_cyclic_session dir =
  let path = Filename.concat dir "cycle.jsonl" in
  let msg id parent call =
    Fmt.str
      {|{"type":"message","id":"%s","parentId":"%s","message":{"role":"assistant","content":[{"type":"toolCall","id":"%s","name":"nu","arguments":{"script":"ls"}}],"usage":{"input":1,"output":1}}}|}
      id parent call
  in
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc
        (String.concat "\n"
           [
             {|{"type":"session","version":3,"id":"cyc","timestamp":"2026-01-01T00:00:00.000Z","cwd":"/tmp"}|};
             msg "aaa" "bbb" "call_a";
             msg "bbb" "aaa" "call_b";
             "";
           ]));
  path

let test_session_parent_cycle_errors () =
  with_temp_dir @@ fun dir ->
  let path = write_cyclic_session dir in
  match read_messages ~path ~leaf:"aaa" () with
  | Error e when contains e "cycle" -> ()
  | Error e -> Alcotest.failf "expected a cycle error, got %S" e
  | Ok ms ->
      Alcotest.failf "expected a cycle error, got %d messages" (List.length ms)

let test_branch_point_parent_cycle_errors () =
  with_temp_dir @@ fun dir ->
  let path = write_cyclic_session dir in
  match read_branch_point ~path ~leaf:"aaa" with
  | Error e -> Alcotest.(check bool) "names the cycle" true (contains e "cycle")
  | Ok (id, _) -> Alcotest.failf "expected a cycle error, resolved to %s" id

let resolve_branch_point path leaf =
  match read_branch_point ~path ~leaf with
  | Ok resolved -> resolved
  | Error e -> Alcotest.fail e

let message_entries path =
  read_jsonl path |> List.filter (fun e -> member "type" e = `String "message")

let message_role entry = str "role" (member "message" entry)

let message_json_contains entry text =
  contains (Yojson.Safe.to_string (member "message" entry)) text

let test_resolve_branch_point_sample () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let entries = message_entries path in
  let user = List.find (fun e -> message_role e = "user") entries |> str "id" in
  let tool_use =
    List.find
      (fun e ->
        message_role e = "assistant" && message_json_contains e "call_1")
      entries
    |> str "id"
  in
  let tool_result =
    List.find (fun e -> message_role e = "toolResult") entries |> str "id"
  in
  let done_assistant =
    List.find
      (fun e -> message_role e = "assistant" && message_json_contains e "done")
      entries
    |> str "id"
  in
  (match resolve_branch_point path done_assistant with
  | id, adjusted ->
      Alcotest.(check string) "assistant text stays put" done_assistant id;
      Alcotest.(check bool) "assistant text not adjusted" false adjusted);
  (match resolve_branch_point path tool_result with
  | id, adjusted ->
      Alcotest.(check string) "tool result stays put" tool_result id;
      Alcotest.(check bool) "tool result not adjusted" false adjusted);
  (match resolve_branch_point path tool_use with
  | id, adjusted ->
      Alcotest.(check string) "tool-use assistant walks to user" user id;
      Alcotest.(check bool) "tool-use assistant adjusted" true adjusted);
  (match resolve_branch_point path user with
  | id, adjusted ->
      Alcotest.(check string) "user stays put" user id;
      Alcotest.(check bool) "user not adjusted" false adjusted);
  match read_branch_point ~path ~leaf:"deadbeef" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected an error for an unknown branch point"

let test_resolve_branch_point_walks_to_tool_result () =
  with_temp_dir @@ fun dir ->
  let path, first_tool_result_id, second_tool_use_id =
    write_two_tool_session dir
  in
  match resolve_branch_point path second_tool_use_id with
  | id, adjusted ->
      Alcotest.(check string)
        "second tool-use assistant walks to prior tool result"
        first_tool_result_id id;
      Alcotest.(check bool) "second tool-use assistant adjusted" true adjusted

let test_resolve_branch_point_interrupted_mid_call () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic" ~model:"claude-x"
        ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"do the thing";
  let user_id = Option.get (Op.Session.leaf session) in
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-x"
    (turn ~text:"on it"
       [
         tool_call ~id:"call_1" ~name:"bash"
           ~args:(`Assoc [ ("command", `String "ls") ])
           ();
       ]);
  let tool_use_id = Option.get (Op.Session.leaf session) in
  let path = Op.Session.path session in
  Op.Session.close session;
  (match resolve_branch_point path tool_use_id with
  | id, adjusted ->
      Alcotest.(check string) "interrupted tool-use walks to user" user_id id;
      Alcotest.(check bool) "interrupted tool-use adjusted" true adjusted);
  (match load_messages path ~leaf:tool_use_id () with
  | [ Op.Llm.User { text = u; _ }; Op.Llm.Assistant a ] ->
      Alcotest.(check string) "mid-call user text" "do the thing" u;
      Alcotest.(check string) "mid-call assistant text" "on it" a.text;
      Alcotest.(check int)
        "mid-call tool calls preserved" 1 (List.length a.tool_calls)
  | ms -> Alcotest.failf "expected [User; Assistant], got %d" (List.length ms));
  let forked =
    match Op.Session.resume ~path ~leaf:user_id () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_assistant forked ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-x"
    (turn ~text:"retry answer" []);
  let new_id = Option.get (Op.Session.leaf forked) in
  Op.Session.close forked;
  (match load_messages path ~leaf:new_id () with
  | [ Op.Llm.User { text = u; _ }; Op.Llm.Assistant a ] ->
      Alcotest.(check string) "forked mid-call user text" "do the thing" u;
      Alcotest.(check string)
        "forked mid-call assistant text" "retry answer" a.text
  | ms -> Alcotest.failf "expected [User; Assistant], got %d" (List.length ms));
  let entries =
    read_jsonl path
    |> List.filter (fun e -> member "type" e = `String "message")
  in
  let children =
    List.filter (fun e -> member "parentId" e = `String user_id) entries
  in
  Alcotest.(check int)
    "user entry has two children after mid-call fork" 2 (List.length children);
  Alcotest.(check int)
    "two leaves after mid-call fork" 2
    (List.length (leaf_ids ~path))

let test_session_resume_and_fork () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let user_id =
    str "id"
      (read_jsonl path
      |> List.find (fun e -> member "type" e = `String "message"))
  in
  let session =
    match Op.Session.resume ~path ~leaf:user_id () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-x"
    (turn ~text:"forked answer" []);
  let new_id =
    match Op.Session.leaf session with Some id -> id | None -> ""
  in
  Op.Session.close session;
  let entries =
    read_jsonl path
    |> List.filter (fun e -> member "type" e = `String "message")
  in
  let new_entry = List.find (fun e -> str "id" e = new_id) entries in
  Alcotest.(check string)
    "new entry parentId" user_id (str "parentId" new_entry);
  let children =
    List.filter (fun e -> member "parentId" e = `String user_id) entries
  in
  Alcotest.(check int) "user entry has two children" 2 (List.length children);
  match load_messages path ~leaf:new_id () with
  | [ Op.Llm.User { text = u; _ }; Op.Llm.Assistant a ] ->
      Alcotest.(check string) "forked user text" "do the thing" u;
      Alcotest.(check string) "forked assistant text" "forked answer" a.text
  | ms -> Alcotest.failf "expected [User; Assistant], got %d" (List.length ms)

let test_file_for_session () =
  with_temp_dir @@ fun dir ->
  let path1 = write_sample_session dir in
  let path2 = write_sample_session dir in
  let session_id1 = str "id" (List.hd (read_jsonl path1)) in
  let session_id2 = str "id" (List.hd (read_jsonl path2)) in
  (match Op.Session_read.file_for_session ~dir ~session_id:session_id1 with
  | Ok found -> Alcotest.(check string) "found first" path1 found
  | Error e -> Alcotest.fail e);
  (match Op.Session_read.file_for_session ~dir ~session_id:session_id2 with
  | Ok found -> Alcotest.(check string) "found second" path2 found
  | Error e -> Alcotest.fail e);
  match Op.Session_read.file_for_session ~dir ~session_id:"deadbeef" with
  | Error msg when contains msg "deadbeef" -> ()
  | Ok _ -> Alcotest.fail "expected an error for unknown session id"
  | Error msg -> Alcotest.failf "unexpected error: %s" msg

let test_leaf_ids () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  (match leaf_ids ~path with
  | [ id ] ->
      let entries =
        read_jsonl path
        |> List.filter (fun e -> member "type" e <> `String "session")
      in
      let expected_last = str "id" (List.hd (List.rev entries)) in
      Alcotest.(check string) "sole leaf is the tail" expected_last id
  | ids -> Alcotest.failf "expected one leaf, got %d" (List.length ids));
  let user_id =
    str "id"
      (read_jsonl path
      |> List.find (fun e -> member "type" e = `String "message"))
  in
  let forked =
    match Op.Session.resume ~path ~leaf:user_id () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_assistant forked ~api:"a" ~provider:"a" ~model:"m"
    (turn ~text:"other branch" []);
  Op.Session.close forked;
  Alcotest.(check int)
    "two leaves after forking" 2
    (List.length (leaf_ids ~path))

let test_list_sessions () =
  with_temp_dir @@ fun dir ->
  let _path1 = write_sample_session dir in
  let _path2 = write_sample_session dir in
  let sessions =
    match Op.Session_list.list_sessions ~dir () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check int) "two sessions" 2 (List.length sessions);
  let ids = List.map (fun (s : Op.Session_list.summary) -> s.id) sessions in
  Alcotest.(check bool) "ascending ids" true (ids = List.sort compare ids);
  (match sessions with
  | [ s1; s2 ] ->
      Alcotest.(check string) "summary1" "do the thing" s1.summary;
      Alcotest.(check string) "summary2" "do the thing" s2.summary;
      Alcotest.(check int) "count1" 4 s1.count
  | _ -> Alcotest.fail "expected exactly two sessions");
  match Op.Session_list.list_sessions ~dir () with
  | Ok (_ :: _ as l) ->
      Alcotest.(check int) "all sessions returned" 2 (List.length l)
  | Ok [] -> Alcotest.fail "expected sessions"
  | Error e -> Alcotest.fail e

let test_list_entries () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let entries =
    match Op.Session_list.list_entries ~path () with
    | Ok e -> e
    | Error e -> Alcotest.fail e
  in
  Alcotest.(check int) "user+assistant only" 3 (List.length entries);
  let roles =
    List.map (fun (e : Op.Session_list.entry_summary) -> e.role) entries
  in
  Alcotest.(check (list string))
    "roles"
    [ "user"; "assistant"; "assistant" ]
    roles;
  (match entries with
  | [ u; a1; a2 ] ->
      Alcotest.(check string) "user preview" "do the thing" u.preview;
      Alcotest.(check string) "first assistant" "on it" a1.preview;
      Alcotest.(check string) "second assistant" "done" a2.preview
  | _ -> Alcotest.fail "expected 3 entries");
  (match entries with
  | [ u; a1; a2 ] ->
      Alcotest.(check bool)
        "user parent is some" true
        (Option.is_some u.parent_id);
      Alcotest.(check (option string)) "a1 parent" (Some u.id) a1.parent_id;
      Alcotest.(check bool)
        "a2 parent is some" true
        (Option.is_some a2.parent_id);
      Alcotest.(check bool) "a2 parent not a1" true (a2.parent_id <> Some a1.id)
  | _ -> Alcotest.fail "expected 3 entries");
  match Op.Session_list.list_entries ~path () with
  | Ok [ _; e1; e2 ] ->
      Alcotest.(check string) "second-last role" "assistant" e1.role;
      Alcotest.(check string) "last role" "assistant" e2.role
  | Ok l -> Alcotest.failf "expected 3 entries, got %d" (List.length l)
  | Error e -> Alcotest.fail e

let test_tool_preview_and_fork () =
  with_temp_dir @@ fun dir ->
  let session =
    match Op.Session.v ~dir ~cwd:"/tmp/p" ~provider:"a" ~model:"m" () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"go";
  let user_id = Option.get (Op.Session.leaf session) in
  Op.Session.append_assistant session ~api:"a" ~provider:"a" ~model:"m"
    (turn ~text:""
       [
         tool_call ~id:"c1" ~name:"edit"
           ~args:(`Assoc [ ("path", `String "lib/x.ml") ])
           ();
       ]);
  let path = Op.Session.path session in
  Op.Session.close session;
  let forked =
    match Op.Session.resume ~path ~leaf:user_id () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_assistant forked ~api:"a" ~provider:"a" ~model:"m"
    (turn ~text:"other branch" []);
  Op.Session.close forked;
  let entries =
    match Op.Session_list.list_entries ~path () with
    | Ok e -> e
    | Error e -> Alcotest.fail e
  in
  let tool_turn =
    List.find
      (fun (e : Op.Session_list.entry_summary) -> contains e.preview "edit")
      entries
  in
  Alcotest.(check bool)
    "tool preview arrow" true
    (contains tool_turn.preview "→");
  Alcotest.(check bool)
    "tool preview path" true
    (contains tool_turn.preview "lib/x.ml");
  let user_entry =
    List.find
      (fun (e : Op.Session_list.entry_summary) -> e.id = user_id)
      entries
  in
  Alcotest.(check bool) "user entry is a fork" true user_entry.is_fork

let test_session_image_round_trip () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic" ~model:"m" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  let img = Op.Llm.Image { media_type = "image/png"; data = "QUJD" } in
  Op.Session.append_user session ~blocks:[ img ] ~text:"describe" ();
  let path = Op.Session.path session in
  Op.Session.close session;
  match load_messages path () with
  | [ Op.Llm.User { text; blocks = [ Op.Llm.Image { media_type; data } ] } ] ->
      Alcotest.(check string) "text" "describe" text;
      Alcotest.(check string) "media type" "image/png" media_type;
      Alcotest.(check string) "data" "QUJD" data
  | _ -> Alcotest.fail "expected a single User with one Image block"

let test_session_image_preview_elides () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic" ~model:"m" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  let img = Op.Llm.Image { media_type = "image/png"; data = "QUJD" } in
  Op.Session.append_user session ~blocks:[ img ] ~text:"" ();
  let path = Op.Session.path session in
  Op.Session.close session;
  match Op.Session_list.list_entries ~path () with
  | Ok [ e ] ->
      Alcotest.(check bool)
        "placeholder shown" true
        (contains e.preview "[image image/png");
      Alcotest.(check bool) "no base64" false (contains e.preview "QUJD")
  | _ -> Alcotest.fail "expected one entry"

let test_list_branches () =
  with_temp_dir @@ fun dir ->
  let session =
    match Op.Session.v ~dir ~cwd:"/tmp/p" ~provider:"a" ~model:"m" () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"go";
  let user_id = Option.get (Op.Session.leaf session) in
  Op.Session.append_assistant session ~api:"a" ~provider:"a" ~model:"m"
    (turn ~text:"working"
       [
         tool_call ~id:"c1" ~name:"edit"
           ~args:(`Assoc [ ("path", `String "lib/x.ml") ])
           ();
       ]);
  Op.Session.append_tool_result session
    {
      Op.Llm.id = "c1";
      name = "edit";
      content = "ok";
      is_error = false;
      details = None;
    };
  let path = Op.Session.path session in
  Op.Session.close session;
  (match Op.Session_list.list_branches ~path () with
  | Ok [ tip ] ->
      Alcotest.(check string)
        "linear session's tip is its leaf"
        (List.hd (leaf_ids ~path))
        tip.tip;
      Alcotest.(check string)
        "tool-result tip described by its assistant" "assistant" tip.role;
      Alcotest.(check string) "preview from the ancestor" "working" tip.preview
  | Ok ts -> Alcotest.failf "expected one tip, got %d" (List.length ts)
  | Error e -> Alcotest.fail e);
  let forked =
    match Op.Session.resume ~path ~leaf:user_id () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_assistant forked ~api:"a" ~provider:"a" ~model:"m"
    (turn ~text:"other branch" []);
  Op.Session.close forked;
  match Op.Session_list.list_branches ~path () with
  | Ok tips ->
      Alcotest.(check (list string))
        "one tip per branch, file order" (leaf_ids ~path)
        (List.map (fun (t : Op.Session_list.branch_tip) -> t.tip) tips);
      Alcotest.(check (list string))
        "each described"
        [ "working"; "other branch" ]
        (List.map (fun (t : Op.Session_list.branch_tip) -> t.preview) tips)
  | Error e -> Alcotest.fail e

let test_context_stripped_from_listings () =
  with_temp_dir @@ fun dir ->
  let session =
    match Op.Session.v ~dir ~cwd:"/tmp/p" ~provider:"a" ~model:"m" () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  let prompt =
    Op.Context.render
      ~files:[ ("CLAUDE.md", "# Project\nbuild with make") ]
      ~prompt:"audit the wav parser"
  in
  Op.Session.append_user session () ~text:prompt;
  Op.Session.append_assistant session ~api:"a" ~provider:"a" ~model:"m"
    (turn ~text:"on it" []);
  Op.Session.append_user session ()
    ~text:
      (Op.Context.render
         ~files:[ ("a.md", "one"); ("b.md", "two") ]
         ~prompt:"and now the tests");
  let path = Op.Session.path session in
  Op.Session.close session;
  (match Op.Session_list.list_sessions ~dir () with
  | Ok [ s ] ->
      Alcotest.(check string)
        "summary is the prompt" "audit the wav parser" s.summary
  | _ -> Alcotest.fail "expected one session summary");
  match Op.Session_list.list_entries ~path () with
  | Ok [ first; _; second ] ->
      Alcotest.(check string)
        "first user preview" "audit the wav parser" first.preview;
      Alcotest.(check string)
        "two blocks stripped" "and now the tests" second.preview
  | _ -> Alcotest.fail "expected three entries"

let test_context_kept_in_history () =
  with_temp_dir @@ fun dir ->
  let session =
    match Op.Session.v ~dir ~cwd:"/tmp/p" ~provider:"a" ~model:"m" () with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  let prompt =
    Op.Context.render ~files:[ ("CLAUDE.md", "rules") ] ~prompt:"go"
  in
  Op.Session.append_user session () ~text:prompt;
  let path = Op.Session.path session in
  Op.Session.close session;
  match load_messages path () with
  | [ Op.Llm.User { text; _ } ] ->
      Alcotest.(check string) "context replayed verbatim" prompt text
  | ms -> Alcotest.failf "unexpected message list (len %d)" (List.length ms)

let suite =
  ( "session_read",
    [
      ("image round trip", `Quick, test_session_image_round_trip);
      ("image preview elides", `Quick, test_session_image_preview_elides);
      ("load round trip", `Quick, test_session_load_round_trip);
      ("thinking round trip", `Quick, test_session_thinking_round_trip);
      ( "thinking round trip empty signature",
        `Quick,
        test_session_thinking_empty_sig );
      ("thinking round trip redacted", `Quick, test_session_thinking_redacted);
      ("load with leaf", `Quick, test_session_load_leaf);
      ("load unknown leaf", `Quick, test_session_load_unknown_leaf);
      ("load parent cycle errors", `Quick, test_session_parent_cycle_errors);
      ( "branch point parent cycle errors",
        `Quick,
        test_branch_point_parent_cycle_errors );
      ("resolve branch point sample", `Quick, test_resolve_branch_point_sample);
      ( "resolve branch point to tool result",
        `Quick,
        test_resolve_branch_point_walks_to_tool_result );
      ( "resolve branch point interrupted mid call",
        `Quick,
        test_resolve_branch_point_interrupted_mid_call );
      ("resume and fork", `Quick, test_session_resume_and_fork);
      ("file for session", `Quick, test_file_for_session);
      ("leaf ids", `Quick, test_leaf_ids);
      ("list sessions", `Quick, test_list_sessions);
      ("list entries", `Quick, test_list_entries);
      ("list entries tool preview and fork", `Quick, test_tool_preview_and_fork);
      ("list branches", `Quick, test_list_branches);
      ( "context stripped from listings",
        `Quick,
        test_context_stripped_from_listings );
      ("context kept in history", `Quick, test_context_kept_in_history);
    ] )
