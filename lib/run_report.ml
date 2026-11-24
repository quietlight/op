let to_line json = Yojson.Safe.to_string json

let sum_usage ?(base = 0) history =
  let zero =
    { Llm.input = 0; output = 0; cache_read = 0; cache_creation = 0 }
  in
  let history =
    if base <= 0 then history else List.filteri (fun i _ -> i >= base) history
  in
  List.fold_left
    (fun acc -> function
      | Llm.Assistant { usage = Some u; _ } ->
          let acc = Option.value acc ~default:zero in
          Some
            {
              Llm.input = acc.input + u.input;
              output = acc.output + u.output;
              cache_read = acc.cache_read + u.cache_read;
              cache_creation = acc.cache_creation + u.cache_creation;
            }
      | _ -> acc)
    None history

let context_tokens (u : Llm.usage) =
  u.input + u.output + u.cache_read + u.cache_creation

let context_usage history =
  List.fold_left
    (fun acc -> function
      | Llm.Assistant { usage = Some u; _ } -> Some u | _ -> acc)
    None history

let context_fields ~window history =
  match (window, Option.map context_tokens (context_usage history)) with
  | Some window, Some tokens when window > 0 ->
      [
        ( "context",
          `Assoc
            [
              ("tokens", `Int tokens);
              ("window", `Int window);
              ( "pct",
                `Float
                  (Float.round
                     (float_of_int tokens /. float_of_int window *. 100.)
                  /. 100.) );
            ] );
      ]
  | _ -> []

let of_outcome : Agent.outcome -> bool * string = function
  | Agent.Finished -> (false, "finished")
  | Agent.Max_steps_reached -> (true, "max_steps")
  | Agent.Gate_killed _ -> (true, "gate_killed")
  | Agent.Context_limit_reached -> (true, "context_limit")

type classification = {
  is_error : bool;
  exit_code : int;
  stop_reason : string option;
}

let classify = function
  | Ok (_, outcome) ->
      let is_error, stop_reason = of_outcome outcome in
      {
        is_error;
        exit_code = (if is_error then 1 else 0);
        stop_reason = Some stop_reason;
      }
  | Error _ -> { is_error = true; exit_code = 1; stop_reason = None }

let tokens_line (u : Llm.usage) =
  let cache =
    if u.cache_read = 0 && u.cache_creation = 0 then ""
    else
      Fmt.str " (%d cache read, %d cache write)" u.cache_read u.cache_creation
  in
  Fmt.str "tokens: %d in, %d out%s" u.input u.output cache

let tag typed = if typed then [ ("type", `String "result") ] else []

let result_json ?(typed = false) ~answer ~session_fields ~model ~input ~output
    ~cache_read ~cache_write ~metadata ~outcome () =
  let is_error, stop_reason = of_outcome outcome in
  `Assoc
    (tag typed
    @ (("answer", `String answer) :: session_fields)
    @ [
        ("model", `String model);
        ( "tokens",
          `Assoc
            [
              ("input", `Int input);
              ("output", `Int output);
              ("cacheRead", `Int cache_read);
              ("cacheWrite", `Int cache_write);
            ] );
      ]
    @ metadata
    @ [ ("stopReason", `String stop_reason); ("isError", `Bool is_error) ])

let result ?typed ~answer ~session_fields ~model ~input ~output ~cache_read
    ~cache_write ~metadata ~outcome () =
  to_line
    (result_json ?typed ~answer ~session_fields ~model ~input ~output
       ~cache_read ~cache_write ~metadata ~outcome ())

let error_json ?(typed = false) ~msg ?hint ?session_fields ?model
    ?(metadata = []) () =
  let sfs = Option.value session_fields ~default:[] in
  let hint_field =
    match hint with Some h -> [ ("hint", `String h) ] | None -> []
  in
  let model_field =
    match model with Some m -> [ ("model", `String m) ] | None -> []
  in
  `Assoc
    (tag typed
    @ ("error", `String msg)
      :: (hint_field @ sfs @ model_field @ metadata
         @ [ ("isError", `Bool true) ]))

let error ?typed ~msg ?hint ?session_fields ?model ?metadata () =
  to_line (error_json ?typed ~msg ?hint ?session_fields ?model ?metadata ())

let last_assistant_text history =
  List.filter_map
    (function Llm.Assistant a -> Some a.text | _ -> None)
    history
  |> List.rev
  |> List.find_opt (fun s -> s <> "")

let run_metadata ~base history =
  let new_turns = List.drop base history in
  let tool_results =
    List.filter_map (function Llm.Tool tr -> Some tr | _ -> None) new_turns
  in
  let turns =
    List.length
      (List.filter (function Llm.Assistant _ -> true | _ -> false) new_turns)
  in
  let tools =
    List.fold_left
      (fun acc (tr : Llm.tool_result) ->
        if List.mem tr.name acc then acc else acc @ [ tr.name ])
      [] tool_results
  in
  [
    ("turns", `Int turns);
    ("toolCalls", `Int (List.length tool_results));
    ( "toolErrors",
      `Int (List.length (List.filter (fun tr -> tr.Llm.is_error) tool_results))
    );
    ("tools", `List (List.map (fun n -> `String n) tools));
  ]

let session_fields session =
  match session with
  | None -> []
  | Some s -> (
      let id = ("sessionId", `String (Session.id s)) in
      match Session.leaf s with
      | Some leaf -> [ id; ("parentId", `String leaf) ]
      | None -> [ id ])

let branch_correction_metadata = function
  | None -> []
  | Some (from_id, to_id) ->
      [
        ("branchCorrectedFrom", `String from_id);
        ("branchCorrectedTo", `String to_id);
      ]
