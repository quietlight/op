(* Google Generative AI (Gemini) adapter: native functionCall / functionResponse
   parts over the generateContent REST API. All Gemini-specific wire shapes are
   confined to this module. See the .mli. *)

let ( let* ) = Result.bind

(* ------------------------------------------------------------------ *)
(* Tool schema serialization                                          *)
(* ------------------------------------------------------------------ *)

(* Gemini groups all tools under a single { functionDeclarations: [...] } entry.
   Each declaration is { name; description; parameters = t.parameters }. *)
let tool_specs (tools : Tool.t list) : Yojson.Safe.t list =
  let declarations =
    List.map
      (fun (t : Tool.t) ->
        `Assoc
          [
            ("name", `String t.name);
            ("description", `String t.description);
            ("parameters", t.parameters);
          ])
      tools
  in
  [ `Assoc [ ("functionDeclarations", `List declarations) ] ]

(* ------------------------------------------------------------------ *)
(* Message serialization                                              *)
(* ------------------------------------------------------------------ *)

let text_part s = `Assoc [ ("text", `String s) ]

(* Gemini inline image part: base64 data under its MIME type. *)
let inline_data_part ~media_type ~data =
  `Assoc
    [
      ( "inline_data",
        `Assoc [ ("mime_type", `String media_type); ("data", `String data) ] );
    ]

let user_parts ~text ~blocks =
  let text_parts =
    if text = "" && blocks <> [] then [] else [ text_part text ]
  in
  text_parts
  @ List.map
      (function
        | Llm.Text s -> text_part s
        | Llm.Image { media_type; data } -> inline_data_part ~media_type ~data)
      blocks

(* Gemini has no [system] role: system text goes in the top-level
   [systemInstruction] field. Concatenate all [System] strings, like the
   Anthropic adapter does. *)
let system_text messages =
  messages
  |> List.filter_map (function Llm.System s -> Some s | _ -> None)
  |> String.concat "\n"

(* The opaque [thoughtSignature] Gemini attaches to its reasoning. We carry it in
   the neutral [thinking] field (like Anthropic's signed thinking) and must echo
   it back on the [functionCall] part, or Gemini 3.x rejects the next turn with a
   400 "Function call is missing a thought_signature". *)
let thought_signature : Llm.thinking option -> string option = function
  | Some (Reasoning { signature; _ }) -> signature
  | Some (Redacted _) | None -> None

(* One [functionCall] part. Gemini correlates a call to its response by [id]
   (echoed on both); [thoughtSignature] is part-level, sibling to [functionCall],
   and — for parallel calls — only the first part carries it. *)
let function_call_part ?signature (tc : Llm.tool_call) =
  let fields =
    [
      ( "functionCall",
        `Assoc
          [
            ("name", `String tc.name);
            ("args", tc.arguments);
            ("id", `String tc.id);
          ] );
    ]
  in
  match signature with
  | Some s -> `Assoc (fields @ [ ("thoughtSignature", `String s) ])
  | None -> `Assoc fields

(* Map one neutral message to (role, parts). [System] is handled separately (it
   becomes [systemInstruction]), so it yields [None]. Gemini roles are "user"
   and "model"; tool results are sent as a [functionResponse] part under role
   "user", correlated to the call by name and id. *)
let content_of_message : Llm.message -> (string * Yojson.Safe.t list) option =
  function
  | System _ -> None
  | User { text; blocks } -> Some ("user", user_parts ~text ~blocks)
  | Assistant { text; thinking; tool_calls; _ } ->
      (* The thinking text itself is display-only and not resent; only the
         opaque signature is echoed, attached to the first functionCall part. *)
      let text_parts = if text = "" then [] else [ text_part text ] in
      let signature = thought_signature thinking in
      let call_parts =
        List.mapi
          (fun i tc ->
            function_call_part ?signature:(if i = 0 then signature else None) tc)
          tool_calls
      in
      Some ("model", text_parts @ call_parts)
  | Tool { id; name; content; is_error; _ } ->
      (* Gemini's functionResponse.response must be a JSON object; wrap the
         tool's string output under "error" or "output". The [id] echoes the
         originating functionCall so parallel calls stay correlated. *)
      let key = if is_error then "error" else "output" in
      Some
        ( "user",
          [
            `Assoc
              [
                ( "functionResponse",
                  `Assoc
                    [
                      ("name", `String name);
                      ("response", `Assoc [ (key, `String content) ]);
                      ("id", `String id);
                    ] );
              ];
          ] )

let contents_json messages =
  messages
  |> List.filter_map content_of_message
  |> Util.merge_adjacent
  |> List.map (fun (role, parts) ->
      `Assoc [ ("role", `String role); ("parts", `List parts) ])

let build_body ?max_tokens ~model ~tools ~messages () =
  ignore (model : string);
  let system = system_text messages in
  let fields = [ ("contents", `List (contents_json messages)) ] in
  let fields =
    if system = "" then fields
    else
      fields
      @ [
          ("systemInstruction", `Assoc [ ("parts", `List [ text_part system ]) ]);
        ]
  in
  let fields =
    (* Gemini rejects an empty functionDeclarations list, so only advertise
       tools when there are some. *)
    match tools with
    | [] -> fields
    | _ ->
        fields
        @ [
            ("tools", `List (tool_specs tools));
            ( "toolConfig",
              `Assoc
                [
                  ("functionCallingConfig", `Assoc [ ("mode", `String "AUTO") ]);
                ] );
          ]
  in
  let fields =
    (* Only sent when configured; Gemini applies its own default otherwise. *)
    match max_tokens with
    | Some n ->
        fields
        @ [ ("generationConfig", `Assoc [ ("maxOutputTokens", `Int n) ]) ]
    | None -> fields
  in
  Yojson.Safe.to_string (`Assoc fields)

(* ------------------------------------------------------------------ *)
(* Response parsing                                                   *)
(* ------------------------------------------------------------------ *)

let is_thought part =
  match Yojson.Safe.Util.member "thought" part with `Bool b -> b | _ -> false

let parse_usage json =
  let open Yojson.Safe.Util in
  match member "usageMetadata" json with
  | `Null -> None
  | u ->
      let int_or_zero name = match member name u with `Int n -> n | _ -> 0 in
      (* promptTokenCount is the whole prompt, cachedContentTokenCount a subset
         of it. *)
      Some
        (Llm.usage_uncached
           ~total:(int_or_zero "promptTokenCount")
           ~cached:(int_or_zero "cachedContentTokenCount")
           ~output:(int_or_zero "candidatesTokenCount")
           ~cache_creation:0)

(* Visible text is every non-thought text part concatenated. *)
let parts_text parts =
  let open Yojson.Safe.Util in
  parts
  |> List.filter_map (fun p ->
      if is_thought p then None
      else match member "text" p with `String s -> Some s | _ -> None)
  |> String.concat ""

(* Reasoning, kept for display/session and to carry the opaque thoughtSignature
   that must be echoed on the next turn. Text comes from [thought:true] parts (if
   any); the signature is the first [thoughtSignature] found (Gemini attaches it
   part-level, on the first functionCall part). [None] when neither is present. *)
let parts_thinking parts =
  let open Yojson.Safe.Util in
  let text =
    parts
    |> List.filter_map (fun p ->
        if is_thought p then
          match member "text" p with `String s -> Some s | _ -> None
        else None)
    |> String.concat ""
  in
  let signature =
    parts
    |> List.find_map (fun p ->
        match member "thoughtSignature" p with `String s -> Some s | _ -> None)
  in
  match (text, signature) with
  | "", None -> None
  | _ -> Some (Llm.Reasoning { text; signature })

(* Each functionCall part becomes a tool call. Gemini 3.x emits its own call id;
   fall back to a synthesized one for older models that don't. *)
let parts_tool_calls parts =
  let open Yojson.Safe.Util in
  parts
  |> List.filter_map (fun p ->
      match member "functionCall" p with
      | `Null -> None
      | fc -> (
          match member "name" fc with
          | `String name ->
              let arguments =
                match member "args" fc with `Null -> `Assoc [] | a -> a
              in
              let id =
                match member "id" fc with
                | `String s -> s
                | _ -> Util.next_call_id ()
              in
              Some { Llm.id; name; arguments }
          | _ -> None))

let parse_response json_str : (Llm.assistant_turn, Transport.error) result =
  let* json = Transport.json_of_string json_str in
  let open Yojson.Safe.Util in
  let* () = Transport.check_error json in
  let* candidates = Transport.list_field "candidates" json in
  match candidates with
  | candidate :: _ -> (
      try
        let parts =
          match member "content" candidate |> member "parts" with
          | `List ps -> ps
          | _ -> []
        in
        Ok
          {
            Llm.text = parts_text parts;
            thinking = parts_thinking parts;
            tool_calls = parts_tool_calls parts;
            usage = parse_usage json;
          }
      with Yojson.Safe.Util.Type_error _ ->
        Error (Transport.Unexpected_response json))
  | [] -> Error (Transport.Unexpected_response json)

(* ------------------------------------------------------------------ *)
(* Call                                                               *)
(* ------------------------------------------------------------------ *)

let call ?max_tokens ~post ~base_url ~api_key ~model ~tools ~messages () =
  (* [base_url] carries the API version segment (e.g. .../v1beta), matching the
     OpenAI convention where the base includes /v1; we append only the resource. *)
  let base = base_url |> String.trim |> Transport.strip_trailing_slash in
  let url = Fmt.str "%s/models/%s:generateContent" base model in
  let headers =
    [ ("Content-Type", "application/json"); ("x-goog-api-key", api_key) ]
  in
  let body = build_body ?max_tokens ~model ~tools ~messages () in
  let* response = post ~url ~headers ~body in
  parse_response response

module For_testing = struct
  let tool_specs = tool_specs
  let build_body = build_body
  let parse_response = parse_response
end
