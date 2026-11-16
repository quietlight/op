(* Anthropic Messages adapter: native tool_use / tool_result. All
   Anthropic-specific wire shapes are confined to this module. See the .mli. *)

let ( let* ) = Result.bind

(* { name; description; input_schema = t.parameters } per tool. *)
let tool_specs (tools : Tool.t list) : Yojson.Safe.t list =
  List.map
    (fun (t : Tool.t) ->
      `Assoc
        [
          ("name", `String t.name);
          ("description", `String t.description);
          ("input_schema", t.parameters);
        ])
    tools

let text_block s = `Assoc [ ("type", `String "text"); ("text", `String s) ]

(* Native Anthropic image block: base64 source with its media type. *)
let image_block ~media_type ~data =
  `Assoc
    [
      ("type", `String "image");
      ( "source",
        `Assoc
          [
            ("type", `String "base64");
            ("media_type", `String media_type);
            ("data", `String data);
          ] );
    ]

(* User content blocks: the text body (when non-empty) followed by each
   attachment in order. A text-only message stays a single text block, matching
   the pre-image wire shape. *)
let user_blocks ~text ~blocks =
  let text_blocks =
    if text = "" && blocks <> [] then [] else [ text_block text ]
  in
  text_blocks
  @ List.map
      (function
        | Llm.Text s -> text_block s
        | Llm.Image { media_type; data } -> image_block ~media_type ~data)
      blocks

(* Map one neutral message to (role, content-blocks). [System] is handled
   separately (it becomes the top-level [system] field), so it yields [None]. *)
let blocks_of_message : Llm.message -> (string * Yojson.Safe.t list) option =
  function
  | System _ -> None
  | User { text; blocks } -> Some ("user", user_blocks ~text ~blocks)
  | Assistant { text; thinking; tool_calls; _ } ->
      (* Anthropic requires thinking blocks to come first, and rejects a
         [thinking] block that lacks its [signature] — so an unsigned
         [Reasoning] (which only ever originates from the OpenAI adapters) is
         dropped rather than replayed. *)
      let thinking_blocks =
        match thinking with
        | Some (Reasoning { text; signature = Some signature }) ->
            [
              `Assoc
                [
                  ("type", `String "thinking");
                  ("thinking", `String text);
                  ("signature", `String signature);
                ];
            ]
        | Some (Redacted data) ->
            [
              `Assoc
                [
                  ("type", `String "redacted_thinking"); ("data", `String data);
                ];
            ]
        | Some (Reasoning _) | None -> []
      in
      let text_blocks = if text = "" then [] else [ text_block text ] in
      let tool_blocks =
        List.map
          (fun (tc : Llm.tool_call) ->
            `Assoc
              [
                ("type", `String "tool_use");
                ("id", `String tc.id);
                ("name", `String tc.name);
                ("input", tc.arguments);
              ])
          tool_calls
      in
      Some ("assistant", thinking_blocks @ text_blocks @ tool_blocks)
  | Tool { id; content; is_error; _ } ->
      Some
        ( "user",
          [
            `Assoc
              [
                ("type", `String "tool_result");
                ("tool_use_id", `String id);
                ("content", `String content);
                ("is_error", `Bool is_error);
              ];
          ] )

let system_text messages =
  messages
  |> List.filter_map (function Llm.System s -> Some s | _ -> None)
  |> String.concat "\n"

let messages_json messages =
  messages
  |> List.filter_map blocks_of_message
  |> Util.merge_adjacent
  |> List.map (fun (role, blocks) ->
      `Assoc [ ("role", `String role); ("content", `List blocks) ])

(* Anthropic requires max_tokens; this is the fallback when models.json does
   not set maxTokens for the model. *)
let default_max_tokens = 4096

let build_body ?(max_tokens = default_max_tokens) ~model ~tools ~messages () =
  let system = system_text messages in
  let fields =
    [
      ("model", `String model);
      ("max_tokens", `Int max_tokens);
      (* Automatic prompt caching: the API caches the prefix up to the last
         cacheable block and moves the breakpoint forward as history grows.
         Always safe to send — sub-minimum prompts are silently not cached. *)
      ("cache_control", `Assoc [ ("type", `String "ephemeral") ]);
      ("messages", `List (messages_json messages));
    ]
  in
  let fields =
    (* Only advertised when there are some, matching the other adapters — the
       field is omitted rather than sent as an empty array. *)
    match tools with
    | [] -> fields
    | _ -> fields @ [ ("tools", `List (tool_specs tools)) ]
  in
  let fields =
    if system = "" then fields else ("system", `String system) :: fields
  in
  Yojson.Safe.to_string (`Assoc fields)

(* First thinking / redacted_thinking block of a response. op does not send the
   interleaved-thinking beta, so a turn carries at most one. *)
let first_thinking blocks =
  let open Yojson.Safe.Util in
  blocks
  |> List.find_map (fun b ->
      match member "type" b with
      | `String "thinking" -> (
          match member "thinking" b with
          | `String text ->
              let signature =
                match member "signature" b with
                | `String s -> Some s
                | _ -> None
              in
              Some (Llm.Reasoning { text; signature })
          | _ -> None)
      | `String "redacted_thinking" -> (
          match member "data" b with
          | `String data -> Some (Llm.Redacted data)
          | _ -> None)
      | _ -> None)

let parse_response json_str : (Llm.assistant_turn, Transport.error) result =
  let* json = Transport.json_of_string json_str in
  let open Yojson.Safe.Util in
  let* () = Transport.check_error json in
  let* blocks = Transport.list_field "content" json in
  let text =
    blocks
    |> List.filter_map (fun b ->
        match (member "type" b, member "text" b) with
        | `String "text", `String s -> Some s
        | _ -> None)
    |> String.concat ""
  in
  let tool_calls =
    blocks
    |> List.filter_map (fun b ->
        match member "type" b with
        | `String "tool_use" -> (
            match (member "id" b, member "name" b) with
            | `String id, `String name ->
                Some { Llm.id; name; arguments = member "input" b }
            | _ -> None)
        | _ -> None)
  in
  let thinking = first_thinking blocks in
  let usage =
    match member "usage" json with
    | `Null -> None
    | u -> (
        let int_or_zero name =
          match member name u with `Int n -> n | _ -> 0
        in
        match (member "input_tokens" u, member "output_tokens" u) with
        (* Anthropic's input_tokens already excludes both cache counts, which
           is exactly Llm.usage's disjoint convention — no subtraction here,
           unlike the OpenAI and Gemini adapters. *)
        | `Int input, `Int output ->
            Some
              {
                Llm.input;
                output;
                cache_read = int_or_zero "cache_read_input_tokens";
                cache_creation = int_or_zero "cache_creation_input_tokens";
              }
        | _ -> None)
  in
  Ok { Llm.text; thinking; tool_calls; usage }

let call ?max_tokens ~post ~base_url ~api_key ~model ~tools ~messages () =
  let base = base_url |> String.trim |> Transport.strip_trailing_slash in
  let url = base ^ "/v1/messages" in
  let headers =
    [
      ("Content-Type", "application/json");
      ("x-api-key", api_key);
      ("anthropic-version", "2023-06-01");
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
