type event =
  | Assistant_turn of Llm.assistant_turn
  | Tool_finished of Llm.tool_result
  | Tool_ids_repaired

type outcome =
  | Finished
  | Max_steps_reached
  | Gate_killed of string
  | Context_limit_reached

let default_system =
  "You are op, a terminal coding agent. Work in steps: call a tool, observe \
   the result, then continue until the task is complete. Give a short final \
   answer."

let rec join_and = function
  | [] -> ""
  | [ x ] -> x
  | [ x; y ] -> x ^ " and " ^ y
  | x :: rest -> x ^ ", " ^ join_and rest

let capabilities ~tools =
  let has t = List.mem t tools in
  let files =
    [ ("read", "read"); ("write", "write"); ("edit", "edit") ]
    |> List.filter_map (fun (n, v) -> if has n then Some v else None)
  in
  let exec =
    if has "nu" then
      [
        "run Nushell scripts with the nu tool (external commands are limited \
         to a small configured toolbox)";
      ]
    else []
  in
  let clauses =
    (match files with [] -> [] | fs -> [ Fmt.str "%s files" (join_and fs) ])
    @ exec
  in
  match clauses with
  | [] -> "You have no tools available for this run."
  | cs -> "You can " ^ join_and cs ^ " using provided tools."

let system_prompt ~cwd ~date ~tools =
  Fmt.str
    "%s %s\n\n\
     # Environment\n\
     - working directory: %s\n\
     - date: %s\n\
     Use absolute paths in file tools and commands; the working directory is \
     not guaranteed to persist between tool calls."
    default_system (capabilities ~tools) cwd date

(* Catch non-cancellation tool exceptions and surface them as error results. *)
let dispatch_tool ~tools (tc : Llm.tool_call) : Llm.tool_result =
  match List.find_opt (fun (t : Tool.t) -> t.name = tc.name) tools with
  | None ->
      {
        Llm.id = tc.id;
        name = tc.name;
        content = Fmt.str "unknown tool %S" tc.name;
        is_error = true;
        details = None;
      }
  | Some t -> (
      let ran =
        match t.run tc.arguments with
        | result -> Ok result
        | exception (Eio.Cancel.Cancelled _ as e) -> raise e
        | exception exn -> Error exn
      in
      match ran with
      | Error exn ->
          {
            Llm.id = tc.id;
            name = tc.name;
            content =
              Fmt.str "tool %S raised an exception: %s" tc.name
                (Printexc.to_string exn);
            is_error = true;
            details = None;
          }
      | Ok (Ok { Tool.content; details }) ->
          {
            Llm.id = tc.id;
            name = tc.name;
            content = Util.to_valid_utf8 content;
            is_error = false;
            details;
          }
      | Ok (Error content) ->
          {
            Llm.id = tc.id;
            name = tc.name;
            content = Util.to_valid_utf8 content;
            is_error = true;
            details = None;
          })

(* Ensure tool-call ids are unique across the run, seeded from prior history.
   Some OpenAI-compatible providers reuse ids (e.g. "functions.bash:0"), which
   breaks correlation and causes 400s on Anthropic. Keep the first occurrence
   and rewrite empty or duplicate ids, emitting [Tool_ids_repaired] once. *)
let uniquifier ~emit messages =
  let seen = Hashtbl.create 16 in
  List.iter
    (function
      | Llm.Assistant { tool_calls; _ } ->
          List.iter
            (fun (tc : Llm.tool_call) -> Hashtbl.replace seen tc.id ())
            tool_calls
      | Llm.Tool { id; _ } -> Hashtbl.replace seen id ()
      | _ -> ())
    messages;
  let counter = ref 0 and notified = ref false in
  let rec mint () =
    let id = Fmt.str "op_call_%d" !counter in
    incr counter;
    if Hashtbl.mem seen id then mint () else id
  in
  fun (tc : Llm.tool_call) ->
    let id =
      if tc.id <> "" && not (Hashtbl.mem seen tc.id) then tc.id
      else begin
        if not !notified then (
          emit Tool_ids_repaired;
          notified := true);
        mint ()
      end
    in
    Hashtbl.replace seen id ();
    { tc with id }

let gated_result ~tools ~emit ~verdict (tc : Llm.tool_call) =
  let result =
    match verdict with
    | Gate.Allow -> dispatch_tool ~tools tc
    | Gate.Warn m ->
        let r = dispatch_tool ~tools tc in
        { r with Llm.content = r.content ^ "\n\n[gate] warning: " ^ m }
    | Gate.Deny m | Gate.Unparsed m ->
        {
          Llm.id = tc.id;
          name = tc.name;
          content = "blocked by gate: " ^ m;
          is_error = true;
          details = None;
        }
    | Gate.Code_red _ -> assert false
  in
  emit (Tool_finished result);
  result

let terminated_result ~emit ~msg (tc : Llm.tool_call) =
  let r =
    {
      Llm.id = tc.id;
      name = tc.name;
      content = "run terminated by gate: " ^ msg;
      is_error = true;
      details = None;
    }
  in
  emit (Tool_finished r);
  Llm.Tool r

(* Run tool calls sequentially through [gate]. On [Code_red], answer remaining
   calls as errors so the session has no unanswered tool calls. *)
let run_calls ~gate ~tools ~emit calls =
  let rec go acc = function
    | [] -> `Done (List.rev acc)
    | (tc : Llm.tool_call) :: rest -> (
        match gate tc with
        | Gate.Code_red msg ->
            let drained =
              List.map (terminated_result ~emit ~msg) (tc :: rest)
            in
            `Killed (List.rev_append acc drained, msg)
        | verdict ->
            let result = gated_result ~tools ~emit ~verdict tc in
            go (Llm.Tool result :: acc) rest)
  in
  go [] calls

let run ~call ~tools ~emit ?(gate = fun _ -> Gate.Allow) ?(max_steps = 150)
    ?(over_context = fun _ -> false) ~messages () =
  let uniquify = uniquifier ~emit messages in
  let rec loop steps history =
    (* Check caps before calling the model so history stays wire-valid. *)
    if steps >= max_steps then Ok (history, Max_steps_reached)
    else if over_context history then Ok (history, Context_limit_reached)
    else
      match call ~messages:history with
      | Error _ as e -> e
      | Ok (turn : Llm.assistant_turn) -> (
          let turn =
            { turn with tool_calls = List.map uniquify turn.tool_calls }
          in
          emit (Assistant_turn turn);
          let history = history @ [ Llm.Assistant turn ] in
          match turn.tool_calls with
          | [] -> Ok (history, Finished)
          | calls -> (
              match run_calls ~gate ~tools ~emit calls with
              | `Done results -> loop (steps + 1) (history @ results)
              | `Killed (results, msg) -> Ok (history @ results, Gate_killed msg)
              ))
  in
  loop 0 messages
