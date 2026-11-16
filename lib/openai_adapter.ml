(* OpenAI chat-completions adapter with native function calling.

   Serializes [Tool.t list] into the OpenAI [tools] schema and parses
   [tool_calls] back into [Llm.assistant_turn]. This is the adapter for any
   provider that speaks the OpenAI chat-completions wire format with native
   tool support — including any provider with an unrecognized api string, which
   [Client] assumes is OpenAI-compatible. *)

let ( let* ) = Result.bind

(* ------------------------------------------------------------------ *)
(* Tool schema serialization                                          *)
(* ------------------------------------------------------------------ *)

(* Convert a [Tool.t] into the OpenAI tools JSON schema:
   { type: "function", function: { name, description, parameters } } *)
let tool_spec (t : Tool.t) : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [
            ("name", `String t.name);
            ("description", `String t.description);
            ("parameters", t.parameters);
          ] );
    ]

let tool_specs tools = List.map tool_spec tools

(* ------------------------------------------------------------------ *)
(* Message serialization                                              *)
(* ------------------------------------------------------------------ *)

(* A user message with attachments: content becomes a typed part list, text
   first then each block. ollama's OpenAI-compatible endpoint accepts base64
   data URIs in image_url. A text-only message keeps a bare string (see
   [message_json]). *)
let user_parts_json ~text ~blocks =
  let text_parts =
    if text = "" then []
    else [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
  in
  let part = function
    | Llm.Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
    | Llm.Image { media_type; data } ->
        `Assoc
          [
            ("type", `String "image_url");
            ( "image_url",
              `Assoc
                [
                  ("url", `String (Fmt.str "data:%s;base64,%s" media_type data));
                ] );
          ]
  in
  `List (text_parts @ List.map part blocks)

(* Map one neutral message to an OpenAI chat-completions message object.
   [System] becomes { role: "system", content: ... }.
   Assistant turns with tool calls include the [tool_calls] array.
   Tool results use { role: "tool", tool_call_id, content }. *)
let message_json : Llm.message -> Yojson.Safe.t option = function
  | System s ->
      Some (`Assoc [ ("role", `String "system"); ("content", `String s) ])
  | User { text; blocks = [] } ->
      (* Text-only: bare string content, byte-identical to the pre-image wire. *)
      Some (`Assoc [ ("role", `String "user"); ("content", `String text) ])
  | User { text; blocks } ->
      Some
        (`Assoc
           [
             ("role", `String "user"); ("content", user_parts_json ~text ~blocks);
           ])
  | Assistant { text; tool_calls; _ } ->
      (* Reasoning is captured for display/session but never replayed here:
         chat-completions providers don't want prior reasoning resent (see
         Llm.thinking). *)
      let base_fields =
        [ ("role", `String "assistant"); ("content", `String text) ]
      in
      let fields =
        match tool_calls with
        | [] -> base_fields
        | _ ->
            let calls =
              List.map
                (fun (tc : Llm.tool_call) ->
                  `Assoc
                    [
                      ("id", `String tc.id);
                      ("type", `String "function");
                      ( "function",
                        `Assoc
                          [
                            ("name", `String tc.name);
                            ( "arguments",
                              `String (Yojson.Safe.to_string tc.arguments) );
                          ] );
                    ])
                tool_calls
            in
            base_fields @ [ ("tool_calls", `List calls) ]
      in
      Some (`Assoc fields)
  | Tool { id; content; is_error; _ } ->
      (* OpenAI tool messages carry only tool_call_id + content; there is
         no is_error field in the spec. Prefix the content instead. *)
      let content = if is_error then "Error: " ^ content else content in
      Some
        (`Assoc
           [
             ("role", `String "tool");
             ("tool_call_id", `String id);
             ("content", `String content);
           ])

(* ------------------------------------------------------------------ *)
(* Request body                                                       *)
(* ------------------------------------------------------------------ *)

let build_body ?max_tokens ~model ~tools ~messages () =
  let json_messages = List.filter_map message_json messages in
  let fields =
    [ ("model", `String model); ("messages", `List json_messages) ]
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
    (* Only sent when configured: chat-completions providers apply their own
       defaults when the field is absent. *)
    match max_tokens with
    | Some n -> fields @ [ ("max_tokens", `Int n) ]
    | None -> fields
  in
  Yojson.Safe.to_string (`Assoc fields)

(* ------------------------------------------------------------------ *)
(* Response parsing                                                   *)
(* ------------------------------------------------------------------ *)

(* Parse the [tool_calls] array from a chat-completions response message. *)
let parse_tool_calls json =
  let open Yojson.Safe.Util in
  match member "tool_calls" json with
  | `Null -> []
  | `List calls ->
      List.filter_map
        (fun call ->
          match member "type" call with
          | `String "function" -> (
              let func = member "function" call in
              let id =
                match member "id" call with `String s -> s | _ -> "unknown"
              in
              let name = member "name" func in
              let arguments = member "arguments" func in
              match (name, arguments) with
              | `String name_str, `String args_str -> (
                  (* arguments is a JSON string that must be parsed *)
                  match Yojson.Safe.from_string args_str with
                  | exception Yojson.Json_error _ ->
                      Some { Llm.id; name = name_str; arguments = `Assoc [] }
                  | args_json ->
                      Some { Llm.id; name = name_str; arguments = args_json })
              | `String name_str, _ ->
                  (* arguments wasn't a string; use as-is (unlikely) *)
                  Some { Llm.id; name = name_str; arguments }
              | _ -> None)
          | _ -> None)
        calls
  | _ -> []

(* Split inline <think> tags out of assistant content.
   Returns Some (reasoning, visible_text) when a closing </think> is found,
   otherwise None.  Opening <think> is optional. *)
let split_think_tags text : (string * string) option =
  let close = "</think>" in
  let close_len = String.length close in
  let rec find i =
    if i > String.length text - close_len then None
    else if String.sub text i close_len = close then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> None
  | Some idx ->
      let before = String.trim (String.sub text 0 idx) in
      let after =
        let raw =
          String.sub text (idx + close_len)
            (String.length text - idx - close_len)
        in
        let len = String.length raw in
        let rec drop_leading i =
          if
            i < len
            && (raw.[i] = ' '
               || raw.[i] = '\t'
               || raw.[i] = '\n'
               || raw.[i] = '\r')
          then drop_leading (i + 1)
          else i
        in
        let start = drop_leading 0 in
        String.sub raw start (len - start)
      in
      let reasoning =
        let open_tag = "<think>" in
        let open_len = String.length open_tag in
        if
          String.length before >= open_len
          && String.sub before 0 open_len = open_tag
        then
          String.trim
            (String.sub before open_len (String.length before - open_len))
        else before
      in
      Some (reasoning, after)

(* Extract visible text and any inline reasoning from an assistant [content]
   value. OpenAI-compatible providers return [content] either as a plain string
   or as an array of typed chunks (e.g. Mistral's magistral reasoning models: a
   "thinking" chunk whose [thinking] is itself an array of {type:text} parts,
   followed by "text" chunks carrying the answer). Concatenate the text chunks
   for the visible answer and the thinking chunks for the reasoning, so the
   answer isn't silently dropped. Mirrors the array handling in
   [openai_responses_adapter]. *)
let parse_content message =
  let open Yojson.Safe.Util in
  match member "content" message with
  | `String s -> (s, None)
  | `List parts ->
      let text =
        parts
        |> List.filter_map (fun p ->
            match (member "type" p, member "text" p) with
            | `String "text", `String s -> Some s
            | _ -> None)
        |> String.concat ""
      in
      let thinking =
        parts
        |> List.filter_map (fun p ->
            match member "type" p with
            | `String "thinking" -> (
                match member "thinking" p with
                | `List inner ->
                    Some
                      (inner
                      |> List.filter_map (fun q ->
                          match member "text" q with
                          | `String s -> Some s
                          | _ -> None)
                      |> String.concat "")
                | `String s -> Some s
                | _ -> None)
            | _ -> None)
        |> String.concat ""
      in
      (text, if thinking = "" then None else Some thinking)
  | _ -> ("", None)

(* Resolve thinking field and strip inline <think> tags from text. Captured for
   display/session only — the [signature] is always [None] because this adapter
   never replays reasoning on the wire. *)
let resolve_thinking ?content_thinking message text =
  let open Yojson.Safe.Util in
  let reasoning text = Some (Llm.Reasoning { text; signature = None }) in
  let probe field =
    match member field message with
    | `String s when s <> "" -> Some s
    | _ -> None
  in
  let from_field =
    match content_thinking with
    | Some t when t <> "" -> Some t
    | _ ->
        List.find_map probe
          [ "reasoning_content"; "reasoning"; "reasoning_text" ]
  in
  (* Some providers (MiniMax-M2.7) duplicate field reasoning as inline
     <think> tags in the content, so the tags are stripped from the
     visible text either way; the wire field wins as the reasoning. *)
  match (from_field, split_think_tags text) with
  | Some r, Some (_, visible) -> (reasoning r, visible)
  | Some r, None -> (reasoning r, text)
  | None, Some (r, visible) when r <> "" -> (reasoning r, visible)
  | None, Some (_, visible) -> (None, visible)
  | None, None -> (None, text)

let parse_usage json =
  let open Yojson.Safe.Util in
  match member "usage" json with
  | `Null -> None
  | u -> (
      match (member "prompt_tokens" u, member "completion_tokens" u) with
      | `Int total, `Int output ->
          (* OpenAI caching is automatic; it reports only cache reads
             (cached_tokens), never a separate write count. The details object
             is absent for many providers (e.g. Kimi), and [member] raises on a
             non-object, so guard the nested lookup. *)
          let cached =
            match member "prompt_tokens_details" u with
            | `Assoc _ as d -> (
                match member "cached_tokens" d with `Int n -> n | _ -> 0)
            | _ -> 0
          in
          (* prompt_tokens already counts the cached tokens. *)
          Some (Llm.usage_uncached ~total ~cached ~output ~cache_creation:0)
      | _ -> None)

(* Parse a full chat-completions response into an [Llm.assistant_turn]. *)
let parse_response json_str : (Llm.assistant_turn, Transport.error) result =
  let* json = Transport.json_of_string json_str in
  let open Yojson.Safe.Util in
  let* () = Transport.check_error json in
  let* choices = Transport.list_field "choices" json in
  match choices with
  | choice :: _ -> (
      try
        let message = member "message" choice in
        let text, content_thinking = parse_content message in
        let thinking, text = resolve_thinking ?content_thinking message text in
        let tool_calls = parse_tool_calls message in
        let usage = parse_usage json in
        Ok { Llm.text; thinking; tool_calls; usage }
      with Yojson.Safe.Util.Type_error _ ->
        Error (Transport.Unexpected_response json))
  | [] -> Error (Transport.Unexpected_response json)

(* ------------------------------------------------------------------ *)
(* Call                                                               *)
(* ------------------------------------------------------------------ *)

let call ?max_tokens ~post ~base_url ~api_key ~model ~tools ~messages () =
  let base = base_url |> String.trim |> Transport.strip_trailing_slash in
  let url = base ^ "/chat/completions" in
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
