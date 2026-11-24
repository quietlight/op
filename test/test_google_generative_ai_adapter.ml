let turn ?(text = "") ?(thinking = None) ?(usage = None) tool_calls :
    Op.Llm.assistant_turn =
  { text; thinking; tool_calls; usage }

let tool_call ?(args = `Assoc []) ~id ~name () : Op.Llm.tool_call =
  { id; name; arguments = args }

module G = Op.Google_generative_ai_adapter.For_testing

let test_google_tool_specs () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "Run a command";
      parameters = `Assoc [ ("type", `String "object") ];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let open Yojson.Safe.Util in
  match G.tool_specs [ tool ] with
  | [ spec ] -> (
      match member "functionDeclarations" spec |> to_list with
      | [ decl ] ->
          Alcotest.(check string) "name" "bash" (member "name" decl |> to_string);
          Alcotest.(check string)
            "description" "Run a command"
            (member "description" decl |> to_string);
          Alcotest.(check bool)
            "has parameters" true
            (match member "parameters" decl with
            | `Assoc _ -> true
            | _ -> false)
      | _ -> Alcotest.fail "expected exactly one function declaration")
  | _ -> Alcotest.fail "expected a single tools entry"

let test_google_serializes_messages () =
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
    G.build_body ~model:"gemini" ~tools:[] ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "system instruction" "sys"
    (member "systemInstruction" json
    |> member "parts" |> to_list |> List.hd |> member "text" |> to_string);
  let contents = member "contents" json |> to_list in
  Alcotest.(check (list string))
    "roles"
    [ "user"; "model"; "user" ]
    (List.map (fun c -> member "role" c |> to_string) contents);
  let model_parts = List.nth contents 1 |> member "parts" |> to_list in
  Alcotest.(check string)
    "assistant text" "let me look"
    (List.hd model_parts |> member "text" |> to_string);
  let fc = List.nth model_parts 1 |> member "functionCall" in
  Alcotest.(check string) "fc name" "bash" (member "name" fc |> to_string);
  Alcotest.(check string)
    "fc args" "ls"
    (member "args" fc |> member "command" |> to_string);
  let resp =
    List.nth contents 2 |> member "parts" |> to_list |> List.hd
    |> member "functionResponse"
  in
  Alcotest.(check string) "fr name" "bash" (member "name" resp |> to_string);
  Alcotest.(check string)
    "fr output" "files"
    (member "response" resp |> member "output" |> to_string)

let test_google_merges_tool_results () =
  let messages =
    [
      Op.Llm.Tool
        {
          id = "a";
          name = "bash";
          content = "x";
          is_error = false;
          details = None;
        };
      Op.Llm.Tool
        {
          id = "b";
          name = "read";
          content = "y";
          is_error = true;
          details = None;
        };
    ]
  in
  let json =
    G.build_body ~model:"gemini" ~tools:[] ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let contents = member "contents" json |> to_list in
  Alcotest.(check int) "single merged content" 1 (List.length contents);
  Alcotest.(check int)
    "two parts" 2
    (List.hd contents |> member "parts" |> to_list |> List.length);
  let parts = List.hd contents |> member "parts" |> to_list in
  Alcotest.(check string)
    "error key" "y"
    (List.nth parts 1 |> member "functionResponse" |> member "response"
   |> member "error" |> to_string)

let test_google_omits_tools () =
  let json =
    G.build_body ~model:"gemini" ~tools:[] ~messages:[ Op.Llm.user "hi" ] ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "tools omitted without tools" true
    (member "tools" json = `Null);
  Alcotest.(check bool)
    "toolConfig omitted without tools" true
    (member "toolConfig" json = `Null);
  Alcotest.(check bool)
    "generationConfig omitted without max_tokens" true
    (member "generationConfig" json = `Null)

let test_google_tools_advertised () =
  let tool : Op.Tool.t =
    {
      name = "bash";
      description = "d";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let json =
    G.build_body ~max_tokens:512 ~model:"gemini" ~tools:[ tool ]
      ~messages:[ Op.Llm.user "hi" ]
      ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "tools present" true
    (match member "tools" json with `List (_ :: _) -> true | _ -> false);
  Alcotest.(check string)
    "tool mode" "AUTO"
    (member "toolConfig" json
    |> member "functionCallingConfig"
    |> member "mode" |> to_string);
  Alcotest.(check int)
    "max output tokens" 512
    (member "generationConfig" json |> member "maxOutputTokens" |> to_int)

let test_google_parse_usage () =
  let json =
    {|{"candidates":[{"content":{"parts":[{"text":"Hello there"}]},"finishReason":"STOP"}],"usageMetadata":{"promptTokenCount":1200,"candidatesTokenCount":88,"cachedContentTokenCount":1024,"totalTokenCount":1288}}|}
  in
  match G.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "text" "Hello there" turn.text;
      Alcotest.(check int) "no tool calls" 0 (List.length turn.tool_calls);
      match turn.usage with
      | Some { input; output; cache_read; cache_creation } ->
          Alcotest.(check int) "uncached input" 176 input;
          Alcotest.(check int) "output" 88 output;
          Alcotest.(check int) "cache read" 1024 cache_read;
          Alcotest.(check int) "cache creation" 0 cache_creation;
          Alcotest.(check int) "prompt total" 1200 (input + cache_read)
      | None -> Alcotest.fail "expected usage")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_google_parse_function_call () =
  let json =
    {|{"candidates":[{"content":{"parts":[{"functionCall":{"name":"bash","args":{"command":"ls"},"id":"o3syfqcz"}}]}}]}|}
  in
  match G.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "no text" "" turn.text;
      Alcotest.(check bool) "no usage" true (turn.usage = None);
      match turn.tool_calls with
      | [ tc ] -> (
          Alcotest.(check string) "name" "bash" tc.name;
          Alcotest.(check string) "uses gemini id" "o3syfqcz" tc.id;
          match Yojson.Safe.Util.member "command" tc.arguments with
          | `String "ls" -> ()
          | _ -> Alcotest.fail "expected command argument")
      | _ -> Alcotest.fail "expected one tool call")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_google_parse_thought_signature () =
  let json =
    {|{"candidates":[{"content":{"parts":[{"functionCall":{"name":"bash","args":{},"id":"c1"},"thoughtSignature":"abc123"}]}}]}|}
  in
  match G.parse_response json with
  | Ok turn -> (
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check string) "no thought text" "" text;
          Alcotest.(check (option string)) "signature" (Some "abc123") signature
      | _ -> Alcotest.fail "expected thinking carrying the signature")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_google_echoes_thought_signature () =
  let messages =
    [
      Op.Llm.Assistant
        (turn
           ~thinking:
             (Some (Op.Llm.Reasoning { text = ""; signature = Some "sig9" }))
           [
             tool_call ~id:"c1" ~name:"bash"
               ~args:(`Assoc [ ("command", `String "ls") ])
               ();
           ]);
      Op.Llm.Tool
        {
          id = "c1";
          name = "bash";
          content = "files";
          is_error = false;
          details = None;
        };
    ]
  in
  let json =
    G.build_body ~model:"gemini" ~tools:[] ~messages ()
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  let contents = member "contents" json |> to_list in
  let model_part = List.hd contents |> member "parts" |> to_list |> List.hd in
  Alcotest.(check string)
    "signature echoed part-level" "sig9"
    (member "thoughtSignature" model_part |> to_string);
  Alcotest.(check string)
    "call id echoed in functionCall" "c1"
    (member "functionCall" model_part |> member "id" |> to_string);
  let resp_part = List.nth contents 1 |> member "parts" |> to_list |> List.hd in
  Alcotest.(check string)
    "id echoed in functionResponse" "c1"
    (member "functionResponse" resp_part |> member "id" |> to_string)

let test_google_parse_thought () =
  let json =
    {|{"candidates":[{"content":{"parts":[{"text":"thinking hard","thought":true},{"text":"the answer"}]}}]}|}
  in
  match G.parse_response json with
  | Ok turn -> (
      Alcotest.(check string) "visible text only" "the answer" turn.text;
      match turn.thinking with
      | Some (Reasoning { text; signature }) ->
          Alcotest.(check (option string)) "signature" None signature;
          Alcotest.(check string) "thought text" "thinking hard" text
      | _ -> Alcotest.fail "expected thinking from thought part")
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_google_parse_error () =
  let json =
    {|{"error":{"code":400,"message":"API key not valid","status":"INVALID_ARGUMENT"}}|}
  in
  match G.parse_response json with
  | Error (Op.Transport.Api_error msg) ->
      Alcotest.(check string) "error message" "API key not valid" msg
  | Error e -> Alcotest.fail ("unexpected error: " ^ Op.Transport.pp_error e)
  | Ok _ -> Alcotest.fail "expected an API error"

let suite =
  ( "google_generative_ai_adapter",
    [
      ("google serializes tools", `Quick, test_google_tool_specs);
      ("google serializes messages", `Quick, test_google_serializes_messages);
      ("google merges tool results", `Quick, test_google_merges_tool_results);
      ("google omits tools/config when absent", `Quick, test_google_omits_tools);
      ( "google advertises tools and config",
        `Quick,
        test_google_tools_advertised );
      ("google parses text and usage", `Quick, test_google_parse_usage);
      ("google parses function call", `Quick, test_google_parse_function_call);
      ("google parses thought part", `Quick, test_google_parse_thought);
      ( "google parses thought signature",
        `Quick,
        test_google_parse_thought_signature );
      ( "google echoes thought signature",
        `Quick,
        test_google_echoes_thought_signature );
      ("google surfaces api error", `Quick, test_google_parse_error);
    ] )
