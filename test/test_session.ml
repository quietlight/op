open Test_support

(* Writer tests for Op.Session: header/chain shape, message serialisation, cwd
   encoding, and resume error handling. Reading sessions back is covered by
   Test_session_read. *)

let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

let read_jsonl path =
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun l -> String.trim l <> "")
  |> List.map Yojson.Safe.from_string

let member = Yojson.Safe.Util.member
let str field json = Yojson.Safe.Util.(member field json |> to_string)

(* A populated session: user prompt, assistant turn with a tool call, the
   tool result, and a final assistant answer. *)
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

let test_session_header_and_chain () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let lines = read_jsonl path in
  Alcotest.(check int) "line count" 7 (List.length lines);
  let header = List.hd lines in
  Alcotest.(check string) "header type" "session" (str "type" header);
  Alcotest.(check int)
    "header version" 3
    Yojson.Safe.Util.(member "version" header |> to_int);
  Alcotest.(check string) "header cwd" "/tmp/proj" (str "cwd" header);
  (* file name is <timestamp>_<session id>.jsonl with : and . mapped to - *)
  let base = Filename.basename path in
  Alcotest.(check bool)
    "file named after session id" true
    (Filename.check_suffix base (str "id" header ^ ".jsonl"));
  (* every entry is the child of the previous one; the first has parentId null *)
  let entries = List.tl lines in
  Alcotest.(check string)
    "first entry" "model_change"
    (str "type" (List.hd entries));
  Alcotest.(check bool)
    "root parentId is null" true
    (member "parentId" (List.hd entries) = `Null);
  ignore
    (List.fold_left
       (fun parent entry ->
         (match parent with
         | None -> ()
         | Some p ->
             Alcotest.(check string) "parent chain" p (str "parentId" entry));
         Some (str "id" entry))
       None entries)

let test_session_message_shapes () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let messages =
    read_jsonl path
    |> List.filter (fun e -> member "type" e = `String "message")
    |> List.map (member "message")
  in
  match messages with
  | [ user; assistant; tool_result; final ] ->
      Alcotest.(check string) "user role" "user" (str "role" user);
      Alcotest.(check string)
        "user text" "do the thing"
        (str "text"
           (List.hd Yojson.Safe.Util.(member "content" user |> to_list)));
      Alcotest.(check string)
        "assistant role" "assistant" (str "role" assistant);
      Alcotest.(check string)
        "assistant api" "anthropic-messages" (str "api" assistant);
      Alcotest.(check string)
        "assistant stopReason" "toolUse"
        (str "stopReason" assistant);
      (let content = Yojson.Safe.Util.(member "content" assistant |> to_list) in
       Alcotest.(check int) "assistant blocks" 2 (List.length content);
       let call = List.nth content 1 in
       Alcotest.(check string) "toolCall block" "toolCall" (str "type" call);
       Alcotest.(check string) "toolCall id" "call_1" (str "id" call);
       Alcotest.(check string)
         "toolCall arguments" "ls"
         (str "command" (member "arguments" call)));
      Alcotest.(check string)
        "toolResult role" "toolResult" (str "role" tool_result);
      Alcotest.(check string)
        "toolCallId" "call_1"
        (str "toolCallId" tool_result);
      Alcotest.(check bool)
        "isError" false
        Yojson.Safe.Util.(member "isError" tool_result |> to_bool);
      Alcotest.(check string) "final stopReason" "stop" (str "stopReason" final);
      let usage = member "usage" final in
      Alcotest.(check bool) "usage input" true (member "input" usage = `Int 12);
      Alcotest.(check bool) "usage output" true (member "output" usage = `Int 34);
      Alcotest.(check bool)
        "usage totalTokens" true
        (member "totalTokens" usage = `Int 46)
  | l -> Alcotest.failf "expected 4 messages, got %d" (List.length l)

let test_session_file_is_private () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  (* Session logs can hold file contents and command output, so they are created
     private to the owner (0600), not the default world-readable 0644. *)
  let perm = (Unix.stat path).Unix.st_perm land 0o777 in
  Alcotest.(check int) "session file mode is 0600" 0o600 perm

(* parentSession appears in the header when given (the gate log points it at
   the run's main session file); a plain session header omits it. *)
let test_session_parent_session_header () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~parent_session:"/s/main.jsonl" ~dir ~cwd:"/tmp/proj"
        ~provider:"anthropic" ~model:"claude-x" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  let path = Op.Session.path session in
  Op.Session.close session;
  let header = List.hd (read_jsonl path) in
  Alcotest.(check string)
    "parentSession recorded" "/s/main.jsonl"
    (str "parentSession" header);
  let plain = List.hd (read_jsonl (write_sample_session dir)) in
  Alcotest.(check bool)
    "absent when not given" true
    (member "parentSession" plain = `Null)

let test_session_custom_entry () =
  with_temp_dir @@ fun dir ->
  let session =
    match
      Op.Session.v ~dir ~cwd:"/tmp/proj" ~provider:"anthropic" ~model:"claude-x"
        ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_custom session ~custom_type:"gate_verdict"
    ~data:(`Assoc [ ("verdict", `String "allow") ]);
  let path = Op.Session.path session in
  Op.Session.close session;
  let entry = List.rev (read_jsonl path) |> List.hd in
  Alcotest.(check string) "type" "custom" (str "type" entry);
  Alcotest.(check string) "customType" "gate_verdict" (str "customType" entry);
  Alcotest.(check string)
    "data carried" "allow"
    (str "verdict" (member "data" entry));
  (* custom entries join the parent chain like any other entry *)
  Alcotest.(check bool) "has parentId" true (member "parentId" entry <> `Null)

let test_session_dir_encoding () =
  Alcotest.(check string)
    "pi cwd encoding" "/root/--Users-x-my-proj--"
    (Op.Session.dir_for ~root:"/root" ~cwd:"/Users/x/my/proj")

let test_session_resume_unknown_leaf () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  match Op.Session.resume ~path ~leaf:"deadbeef" () with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected an error for an unknown leaf id"

(* The [--stream] tee: the sink must see exactly the file's lines, in order —
   header, model_change, thinking_level_change, then each appended entry. If
   the two ever diverge, a stream consumer is reading a different session than
   the one on disk. *)
let test_session_sink_mirrors_file () =
  with_temp_dir @@ fun dir ->
  let seen = ref [] in
  let sink json = seen := json :: !seen in
  let session =
    match
      Op.Session.v ~sink ~dir ~cwd:"/tmp/proj" ~provider:"anthropic"
        ~model:"claude-x" ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"hello";
  Op.Session.append_assistant session ~api:"anthropic-messages"
    ~provider:"anthropic" ~model:"claude-x" (turn ~text:"hi" []);
  let path = Op.Session.path session in
  Op.Session.close session;
  let streamed = List.rev !seen in
  Alcotest.(check (list string))
    "sink lines equal file lines"
    (List.map Yojson.Safe.to_string (read_jsonl path))
    (List.map Yojson.Safe.to_string streamed);
  Alcotest.(check (list string))
    "entry types in order"
    [ "session"; "model_change"; "thinking_level_change"; "message"; "message" ]
    (List.map (str "type") streamed)

(* A resumed session has no header to re-emit: the consumer already knows the
   id it passed to --sessionId, so the stream carries only the new entries. *)
let test_session_sink_on_resume () =
  with_temp_dir @@ fun dir ->
  let path = write_sample_session dir in
  let leaf =
    match List.rev (read_jsonl path) with
    | entry :: _ -> str "id" entry
    | [] -> Alcotest.fail "empty session"
  in
  let seen = ref [] in
  let session =
    match
      Op.Session.resume ~sink:(fun j -> seen := j :: !seen) ~path ~leaf ()
    with
    | Ok s -> s
    | Error e -> Alcotest.fail e
  in
  Op.Session.append_user session () ~text:"more";
  Op.Session.close session;
  Alcotest.(check (list string))
    "only the new entry is streamed" [ "message" ]
    (List.map (str "type") (List.rev !seen))

let suite =
  ( "session",
    [
      ("header and parent chain", `Quick, test_session_header_and_chain);
      ("message shapes", `Quick, test_session_message_shapes);
      ("session file is private", `Quick, test_session_file_is_private);
      ("parentSession header field", `Quick, test_session_parent_session_header);
      ("custom entry", `Quick, test_session_custom_entry);
      ("dir encoding", `Quick, test_session_dir_encoding);
      ("resume unknown leaf", `Quick, test_session_resume_unknown_leaf);
      ("sink mirrors the file", `Quick, test_session_sink_mirrors_file);
      ("sink on resume skips the header", `Quick, test_session_sink_on_resume);
    ] )
