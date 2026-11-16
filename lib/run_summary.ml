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
