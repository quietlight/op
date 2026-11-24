let ( let* ) = Result.bind

let tool_spec (t : Tool.t) : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "function");
      ("name", `String t.name);
      ("description", `String t.description);
      ("parameters", t.parameters);
    ]

let tool_specs tools = List.map tool_spec tools

let assistant_input_items text (tool_calls : Llm.tool_call list) =
  let message_items =
    if text = "" then []
    else
      [
        `Assoc
          [
            ("type", `String "message");
            ("role", `String "assistant");
            ( "content",
              `List
                [
                  `Assoc
                    [
                      ("type", `String "output_text");
                      ("text", `String text);
                      ("annotations", `List []);
                    ];
                ] );
          ];
      ]
  in
  let call_items =
    List.map
      (fun (tc : Llm.tool_call) ->
        `Assoc
          [
            ("type", `String "function_call");
            ("call_id", `String tc.id);
            ("name", `String tc.name);
            ("arguments", `String (Yojson.Safe.to_string tc.arguments));
          ])
      tool_calls
  in
  message_items @ call_items

let input_items : Llm.message -> Yojson.Safe.t list = function
  | System s ->
      [ `Assoc [ ("role", `String "system"); ("content", `String s) ] ]
  | User { text; blocks } ->
      let text_parts =
        if text = "" && blocks <> [] then []
        else
          [ `Assoc [ ("type", `String "input_text"); ("text", `String text) ] ]
      in
      let parts =
        text_parts
        @ List.map
            (function
              | Llm.Text s ->
                  `Assoc [ ("type", `String "input_text"); ("text", `String s) ]
              | Llm.Image { media_type; data } ->
                  `Assoc
                    [
                      ("type", `String "input_image");
                      ( "image_url",
                        `String (Fmt.str "data:%s;base64,%s" media_type data) );
                    ])
            blocks
      in
      [ `Assoc [ ("role", `String "user"); ("content", `List parts) ] ]
  | Assistant { text; tool_calls; _ } -> assistant_input_items text tool_calls
  | Tool { id; content; is_error; _ } ->
      (* [function_call_output] has no error flag. *)
      let content = if is_error then "Error: " ^ content else content in
      [
        `Assoc
          [
            ("type", `String "function_call_output");
            ("call_id", `String id);
            ("output", `String content);
          ];
      ]

let build_body ?max_tokens ~model ~tools ~messages () =
  let input = List.concat_map input_items messages in
  let fields =
    [
      ("model", `String model);
      ("input", `List input);
      (* Stateless: full history is resent every turn. *)
      ("store", `Bool false);
    ]
  in
  let fields =
    match tools with
    | [] -> fields
    | _ ->
        fields
        @ [
            ("tools", `List (tool_specs tools)); ("tool_choice", `String "auto");
          ]
  in
  let fields =
    match max_tokens with
    | Some n -> fields @ [ ("max_output_tokens", `Int n) ]
    | None -> fields
  in
  Yojson.Safe.to_string (`Assoc fields)

let message_text item =
  let open Yojson.Safe.Util in
  match member "content" item with
  | `List parts ->
      parts
      |> List.filter_map (fun p ->
          match (member "type" p, member "text" p) with
          | `String "output_text", `String s -> Some s
          | _ -> None)
      |> String.concat ""
  | _ -> ""

let function_call item : Llm.tool_call option =
  let open Yojson.Safe.Util in
  match (member "call_id" item, member "name" item) with
  | `String id, `String name ->
      let arguments =
        match member "arguments" item with
        | `String s -> (
            match Yojson.Safe.from_string s with
            | exception Yojson.Json_error _ -> `Assoc []
            | json -> json)
        | other -> other
      in
      Some { Llm.id; name; arguments }
  | _ -> None

let reasoning_text item =
  let open Yojson.Safe.Util in
  let parts_text field =
    match member field item with
    | `List parts ->
        parts
        |> List.filter_map (fun p ->
            match member "text" p with `String s -> Some s | _ -> None)
        |> String.concat "\n\n"
    | _ -> ""
  in
  let summary = parts_text "summary" in
  if summary <> "" then summary else parts_text "content"

let parse_usage json =
  let open Yojson.Safe.Util in
  match member "usage" json with
  | `Null -> None
  | u -> (
      match (member "input_tokens" u, member "output_tokens" u) with
      | `Int total, `Int output ->
          let cached =
            match member "input_tokens_details" u with
            | `Assoc _ as d -> (
                match member "cached_tokens" d with `Int n -> n | _ -> 0)
            | _ -> 0
          in
          Some (Llm.usage_uncached ~total ~cached ~output ~cache_creation:0)
      | _ -> None)

let parse_response json_str : (Llm.assistant_turn, Transport.error) result =
  let* json = Transport.json_of_string json_str in
  let open Yojson.Safe.Util in
  let* () = Transport.check_error json in
  let* output = Transport.list_field "output" json in
  let text =
    output
    |> List.filter_map (fun item ->
        match member "type" item with
        | `String "message" -> Some (message_text item)
        | _ -> None)
    |> String.concat ""
  in
  let tool_calls =
    output
    |> List.filter_map (fun item ->
        match member "type" item with
        | `String "function_call" -> function_call item
        | _ -> None)
  in
  let thinking =
    output
    |> List.find_map (fun item ->
        match member "type" item with
        | `String "reasoning" -> (
            match reasoning_text item with
            | "" -> None
            | text -> Some (Llm.Reasoning { text; signature = None }))
        | _ -> None)
  in
  let usage = parse_usage json in
  Ok { Llm.text; thinking; tool_calls; usage }

let call ?max_tokens ~post ~base_url ~api_key ~model ~tools ~messages () =
  let base = base_url |> String.trim |> Transport.strip_trailing_slash in
  let url = base ^ "/responses" in
  let headers =
    [
      ("Content-Type", "application/json");
      ("Authorization", "Bearer " ^ api_key);
    ]
  in
  let body = build_body ?max_tokens ~model ~tools ~messages () in
  let* response = post ~url ~headers ~body in
  parse_response response

module For_testing = struct
  let tool_specs = tool_specs
  let build_body = build_body
  let parse_response = parse_response
end
