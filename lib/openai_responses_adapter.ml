(* OpenAI Responses API adapter (/responses) with native function calling.

   The Responses API is not chat-completions with a different URL: history is an
   [input] array of typed items rather than a [messages] array, tool specs are
   flat (not nested under "function"), tool calls/results are their own items,
   and the response is a top-level [output] array. All of that wire shape is
   confined to this module; the agent loop only ever sees [Llm] types.

   op resends the full history every turn, so requests are stateless
   ([store:false]) and op's own tool-call ids are reused directly as the
   Responses [call_id], avoiding the call_id/item_id pairing scheme that
   stateful, reasoning-replaying clients need. Reasoning is captured for display
   but never replayed (its [signature] is [None]). *)

let ( let* ) = Result.bind

(* ------------------------------------------------------------------ *)
(* Tool schema serialization                                          *)
(* ------------------------------------------------------------------ *)

(* Responses tools are flat: { type: "function", name, description, parameters }
   — unlike chat-completions, which nests these under a "function" key. *)
let tool_spec (t : Tool.t) : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "function");
      ("name", `String t.name);
      ("description", `String t.description);
      ("parameters", t.parameters);
    ]

let tool_specs tools = List.map tool_spec tools

(* ------------------------------------------------------------------ *)
(* Input item serialization                                           *)
(* ------------------------------------------------------------------ *)

(* Map one neutral message to its Responses input item(s). An assistant turn
   expands to a message item (when it has text) plus one function_call item per
   tool call, so this returns a list. *)
(* Reasoning is intentionally not replayed (see module note). *)
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
      (* function_call_output carries no error flag; prefix the content. *)
      let content = if is_error then "Error: " ^ content else content in
      [
        `Assoc
          [
            ("type", `String "function_call_output");
            ("call_id", `String id);
            ("output", `String content);
          ];
      ]

(* ------------------------------------------------------------------ *)
(* Request body                                                       *)
(* ------------------------------------------------------------------ *)

let build_body ?max_tokens ~model ~tools ~messages () =
  let input = List.concat_map input_items messages in
  let fields =
    [
      ("model", `String model);
      ("input", `List input);
      (* op replays full history every turn; never persist server-side. *)
      ("store", `Bool false);
    ]
  in
  let fields =
    (* Only advertised when there are some: strict endpoints reject an empty
       tools array, and tool_choice without tools. *)
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

(* ------------------------------------------------------------------ *)
(* Response parsing                                                   *)
(* ------------------------------------------------------------------ *)

(* Concatenate the text of every output_text part in a "message" item. *)
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

(* A "function_call" item: call_id is reused as the Llm tool-call id, and
   arguments arrives as a JSON-encoded string that must be parsed. *)
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

(* Reasoning item: join the summary parts (or content parts) into one string.
   Captured for display only; signature [None] marks it as never replayed. *)
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
          (* OpenAI caching is automatic; the Responses API nests the cache-read
             count under input_tokens_details (no separate write count). Guard
             the nested lookup: [member] raises if the details object is absent
             (returns `Null) rather than an object. *)
          let cached =
            match member "input_tokens_details" u with
            | `Assoc _ as d -> (
                match member "cached_tokens" d with `Int n -> n | _ -> 0)
            | _ -> 0
          in
          (* input_tokens already counts the cached tokens. *)
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

(* ------------------------------------------------------------------ *)
(* Call                                                               *)
(* ------------------------------------------------------------------ *)

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
