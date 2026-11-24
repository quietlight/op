(* One-shot agent loop and executable boundary.

   Keep Eio orchestration here: TLS, Stdenv capabilities, tools, and loop setup. *)

let ( let* ) = Result.bind

type app_error = List_cmd.app_error = { msg : string; hint : string option }

let app_error ?hint msg = Error { msg; hint }

let err_unknown_provider name =
  Fmt.kstr (fun s -> app_error s) "unknown provider '%s'" name

let err_no_api_key name =
  Fmt.kstr
    (fun s -> app_error s)
    "no API key for provider '%s' (add an \"apiKey\" to its entry in \
     ~/.op_global/agent/models.json)"
    name

let err_no_default =
  app_error
    "no default model configured; set one with op --set default PROVIDER/MODEL \
     (saved in ~/.op_global/agent/settings.json), or pick one per run with \
     --model"

let resolve_default ~override ~default =
  match (override, default) with
  | Some x, _ | None, Some x -> Ok x
  | None, None -> err_no_default

let parse_override = function
  | Some spec ->
      let provider, model = Op.Config.parse_model_spec spec in
      (provider, Some model)
  | None -> (None, None)

let load_context model_spec =
  let* config =
    Op.Config.load_config () |> Result.map_error List_cmd.app_error_of_config
  in
  let override_provider, override_model = parse_override model_spec in
  let* provider_name =
    resolve_default ~override:override_provider
      ~default:(Op.Config.default_provider config)
  in
  let* model =
    resolve_default ~override:override_model
      ~default:(Op.Config.default_model config)
  in
  let* provider =
    match Op.Config.provider config provider_name with
    | Some provider -> Ok provider
    | None -> err_unknown_provider provider_name
  in
  let* api_key =
    match provider.Op.Config.api_key with
    | Some api_key -> Ok api_key
    | None -> err_no_api_key provider_name
  in
  Ok (config, provider, api_key, model)

let open_session ?sink ?parent_session ~cwd (provider : Op.Config.provider)
    model =
  let session =
    let* root = Op.Session.default_root () in
    let dir = Op.Session.dir_for ~root ~cwd in
    Op.Session.v ?sink ?parent_session ~dir ~cwd ~provider:provider.name ~model
      ()
  in
  match session with
  | Ok s -> Some s
  | Error e ->
      Fmt.epr "warning: session log disabled: %s\n%!" e;
      None

let log_event log (provider : Op.Config.provider) model : Op.Agent.event -> unit
    =
  let api = Op.Config.string_of_api provider.api in
  function
  | Op.Agent.Assistant_turn turn ->
      Op.Session.append_assistant log ~api ~provider:provider.name ~model turn
  | Tool_finished tr -> Op.Session.append_tool_result log tr
  | Tool_ids_repaired -> ()

let resolve_prompt ~(fail : string -> 'a) ~resuming ~prompt_file ~prompt =
  let file =
    match prompt_file with
    | Some path ->
        let content =
          try In_channel.with_open_bin path In_channel.input_all
          with Sys_error msg -> fail msg
        in
        Some content
    | None -> None
  in
  let stdin =
    match file with
    | Some _ -> None
    | None when Unix.isatty Unix.stdin -> None
    | None -> Some (In_channel.input_all In_channel.stdin)
  in
  match Op.Prompt.resolve ~file ~arg:prompt ~stdin with
  | Ok p -> p
  | Error e -> fail (Op.Prompt.message ~file_path:prompt_file ~resuming e)

let warn msg = Fmt.epr "warning: %s@." msg

(* Build requested tools; drop [nu] on platforms without Eio process support. *)
let build_tools ~env ~profile ~tool_names ~depth =
  let nu =
    if not (List.mem "nu" tool_names) then None
    else
      match Eio.Stdenv.process_mgr env with
      | proc_mgr ->
          Op.Nu_sandbox.setup ~proc_mgr ~clock:(Eio.Stdenv.clock env) ~warn
            ~profile ~depth
      | exception Failure _ ->
          warn
            "nu tool requested but this platform cannot spawn processes; \
             dropping it";
          None
  in
  (Op.Tools.select ~nu:(Option.map fst nu) tool_names, Option.map snd nu)

let setup_call ~env ~tls ~provider ~api_key ~model ~max_tokens ~tools =
  let post =
    Op.Transport.post ~tls_config:tls ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env)
  in
  let call ~messages =
    Op.Client.call ?max_tokens ~post ~provider ~api_key ~model ~tools ~messages
      ()
    |> Result.map_error Op.Transport.pp_error
  in
  call

let build_messages ~cwd ~prompt ~image_blocks ~prior_history ~tool_names =
  let date =
    let tm = Unix.localtime (Unix.time ()) in
    Fmt.str "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
  in
  Op.Llm.System (Op.Agent.system_prompt ~cwd ~date ~tools:tool_names)
  :: prior_history
  @ [ Op.Llm.user ~blocks:image_blocks prompt ]

(* Emit a stderr resume hint for [--json] only; human mode includes it in the
   footer and [--stream] carries ids in its result line. *)
let close_session ~(mode : Cli.output) session =
  Option.iter
    (fun s ->
      match (mode, Op.Session.leaf s) with
      | (Cli.Human | Cli.Stream), _ -> ()
      | _, Some leaf ->
          Fmt.epr "resume: op --sessionId %s --parentId %s \"prompt\"\n%!"
            (Op.Session.id s) leaf
      | _, None -> Fmt.epr "session: %s\n%!" (Op.Session.id s))
    session;
  Option.iter Op.Session.close session

let warn_branch_correction = function
  | None -> ()
  | Some (from_id, to_id) ->
      Fmt.epr
        "warning: %s has unanswered tool calls; branching from the nearest \
         complete turn %s instead\n\
         %!"
        from_id to_id

let use_color () =
  Unix.isatty Unix.stdout
  && Sys.getenv_opt "NO_COLOR" = None
  && Sys.getenv_opt "TERM" <> Some "dumb"

(* Lazy renderer shared by entry sink and footer so tool results can name their
   call arguments. *)
let renderer =
  lazy
    (let cwd = try Some (Sys.getcwd ()) with Sys_error _ -> None in
     Op.Render.v ~color:(use_color ()) ?cwd ())

let report_result ~(mode : Cli.output) ~session ~base ~window ~model
    ~extra_metadata result =
  let typed = mode = Cli.Stream in
  let session_fields = Op.Run_report.session_fields session in
  let envelope =
    match result with
    | Error e ->
        Op.Run_report.error_json ~typed ~msg:e ~session_fields ~model
          ~metadata:extra_metadata ()
    | Ok (history, outcome) ->
        let answer =
          Option.value ~default:"" (Op.Run_report.last_assistant_text history)
        in
        let u =
          (* Count only turns from this run; replayed history carries prior usage. *)
          Option.value
            (Op.Run_report.sum_usage ~base history)
            ~default:
              {
                Op.Llm.input = 0;
                output = 0;
                cache_read = 0;
                cache_creation = 0;
              }
        in
        Op.Run_report.result_json ~typed ~answer ~session_fields ~model
          ~input:u.input ~output:u.output ~cache_read:u.cache_read
          ~cache_write:u.cache_creation
          ~metadata:
            (extra_metadata
            @ Op.Run_report.run_metadata ~base history
            @ Op.Run_report.context_fields ~window history)
          ~outcome ()
  in
  match mode with
  | Cli.Human ->
      List.iter print_endline (Op.Render.footer (Lazy.force renderer) envelope)
  | Cli.Stream | Cli.Json -> print_endline (Yojson.Safe.to_string envelope)

let build_emit ~log ~provider ~model =
  let log = log_event log provider model in
  fun event ->
    (match event with
    | Op.Agent.Tool_ids_repaired ->
        Fmt.epr
          "warning: provider returned duplicate tool-call ids; repairing to \
           keep the session valid\n\
           %!"
    | Op.Agent.Assistant_turn _ | Op.Agent.Tool_finished _ -> ());
    log event

let session_for_run ?sink ?parent_session ~fail ~cwd ~provider ~model
    ~session_id ~parent_id () =
  match (session_id, parent_id) with
  | None, None ->
      (open_session ?sink ?parent_session ~cwd provider model, [], None)
  | None, Some _ -> fail "--parentId requires --sessionId"
  | Some session_id, parent_id -> (
      match
        Op.Session_resume.resume ?sink ~cwd ~session_id ~parent_id ~provider
          ~model ()
      with
      | Ok (session, history, branch_correction) ->
          (Some session, history, branch_correction)
      | Error e -> fail e)

(* Hard failure: prose on stderr in human mode; JSON envelope on stdout in
   machine modes. *)
let fail_with (mode : Cli.output) ?hint msg =
  (match mode with
  | Cli.Human ->
      List.iter (Fmt.epr "%s@.")
        (Op.Render.footer (Lazy.force renderer)
           (Op.Run_report.error_json ~msg ?hint ()))
  | Cli.Stream | Cli.Json ->
      print_endline
        (Op.Run_report.error ~typed:(mode = Cli.Stream) ~msg ?hint ()));
  exit 1

(* Flush each session line to stdout as written (raw JSON for --stream,
   rendered for human). *)
let stream_sink (mode : Cli.output) =
  match mode with
  | Cli.Stream ->
      Some
        (fun json ->
          print_endline (Yojson.Safe.to_string json);
          flush stdout)
  | Cli.Human ->
      Some
        (fun json ->
          List.iter print_endline (Op.Render.entry (Lazy.force renderer) json);
          flush stdout)
  | Cli.Json -> None

(* Stop between turns once reported usage reaches [contextLimit] of
   [contextWindow], if configured for this model. *)
let context_guard ~config ~provider ~model =
  match Op.Config.context_window_for provider ~model with
  | None -> (None, None)
  | Some window ->
      let budget =
        int_of_float (float_of_int window *. Op.Config.context_limit config)
      in
      let over history =
        match Op.Run_report.context_usage history with
        | Some u -> Op.Run_report.context_tokens u >= budget
        | None -> false
      in
      (Some window, Some over)

let finish ~(mode : Cli.output) ~session ~base ~window ~model ~extra_metadata
    result =
  report_result ~mode ~session ~base ~window ~model ~extra_metadata result;
  close_session ~mode session;
  exit (Op.Run_report.classify result).exit_code

let run_with_context ~env ~mode ~cwd ~config ~provider ~api_key ~model ~tls
    ~session_id ~parent_id ~parent_session ~max_steps ~tool_names ~depth
    ~profile ~prompt ~image_blocks =
  let fail msg = fail_with mode msg in
  let max_tokens = Op.Config.max_tokens_for provider ~model in
  let window, over_context = context_guard ~config ~provider ~model in
  let session, prior_history, branch_correction =
    session_for_run ?sink:(stream_sink mode) ?parent_session ~fail ~cwd
      ~provider ~model ~session_id ~parent_id ()
  in
  warn_branch_correction branch_correction;
  let extra_metadata =
    Op.Run_report.branch_correction_metadata branch_correction
  in
  (* Emit entries even if session logging failed; only [session] is reported in
     envelopes and resume hints. *)
  let log =
    match session with
    | Some s -> s
    | None ->
        Op.Session.unsaved ?sink:(stream_sink mode) ?parent_session ~cwd
          ~provider:provider.Op.Config.name ~model ()
  in
  let emit = build_emit ~log ~provider ~model in
  let tools, nu_gate = build_tools ~env ~profile ~tool_names ~depth in
  let call =
    setup_call ~env ~tls ~provider ~api_key ~model ~max_tokens ~tools
  in
  let gate = Op.Gate.v ?nu:nu_gate ~cwd () in
  Op.Session.append_user log ~blocks:image_blocks ~text:prompt ();
  (* Reflect the tools actually built; [nu] may have been dropped. *)
  let tool_names = List.map (fun (t : Op.Tool.t) -> t.name) tools in
  let messages =
    build_messages ~cwd ~prompt ~image_blocks ~prior_history ~tool_names
  in
  let result =
    Op.Agent.run ~call ~tools ~emit ~gate:(Op.Gate.check gate) ?max_steps
      ?over_context ~messages ()
  in
  finish ~mode ~session ~base:(List.length messages) ~window
    ~model:(provider.Op.Config.name ^ "/" ^ model)
    ~extra_metadata result

(* Raw markdown by default for piping back into op; JSON record with file lists
   under --json. *)
let report_handoff ~json ~goal ~session_id ~model ~files document =
  if not json then print_string (Op.Render.scrub_string document)
  else
    let paths ps = `List (List.map (fun p -> `String p) ps) in
    print_endline
      (Yojson.Safe.to_string
         (`Assoc
            [
              ("kind", `String "handoff");
              ("goal", `String goal);
              ("handoffPrompt", `String document);
              ("sourceSessionId", `String session_id);
              ("model", `String model);
              ( "files",
                `Assoc
                  [
                    ("read", paths files.Op.Handoff.read);
                    ("modified", paths files.Op.Handoff.modified);
                  ] );
              ("isError", `Bool false);
            ]))

(* Single tool-less call to summarize a session; source session is read-only. *)
let run_handoff ~env ~(mode : Cli.output) ~cwd ~provider ~api_key ~model ~tls
    ~session_id ~parent_id ~goal =
  (* Keep errors off stdout in human mode so a piped handoff never feeds an
     error as the next prompt. *)
  let fail msg = fail_with mode msg in
  let json = mode = Cli.Json in
  let history =
    match Op.Session_resume.load ~cwd ~session_id ~parent_id () with
    | Ok (_, history) -> history
    | Error e -> fail e
  in
  let files = Op.Handoff.files_of_history history in
  let call =
    setup_call ~env ~tls ~provider ~api_key ~model
      ~max_tokens:(Op.Config.max_tokens_for provider ~model)
      ~tools:[]
  in
  let messages =
    (Op.Llm.System Op.Handoff.system_prompt :: history)
    @ [ Op.Llm.user (Op.Handoff.user_prompt ~goal) ]
  in
  match call ~messages with
  | Error e -> fail e
  | Ok (turn : Op.Llm.assistant_turn) ->
      report_handoff ~json ~goal ~session_id
        ~model:(provider.Op.Config.name ^ "/" ^ model)
        ~files
        (Op.Handoff.assemble ~summary:turn.text ~files)

(* Use [handoffModel] unless --model overrides it; re-read config so
   [load_context] owns config errors. *)
let handoff_model_spec model_spec =
  match model_spec with
  | Some _ -> model_spec
  | None -> (
      match Op.Config.load_config () with
      | Ok config -> Op.Config.handoff_model config
      | Error _ -> None)

(* Tool precedence: --tools > profile.tools > all known tools. Validate
   profile.tools here since it comes from user JSON. *)
let resolve_tool_names ~tools ~(profile : Op.Config.nu_profile) =
  match (tools, profile.tools) with
  | Some names, _ -> Ok names
  | None, None -> Ok Op.Tools.known
  | None, Some names ->
      Result.map_error
        (fun msg -> Fmt.str "profile '%s': %s" profile.name msg)
        (Op.Tools.validate names)

let expand_prompt ~fail ~context prompt =
  let files =
    List.map
      (fun path ->
        match Op.Util.read_file path with
        | Ok contents -> (path, contents)
        (* [read_file] already includes the path. *)
        | Error msg -> Fmt.kstr fail "--context: %s" msg)
      context
  in
  Op.Context.render ~files ~prompt

let run_prompt ~env ~mode ~session_id ~parent_id ~from_session ~model_spec
    ~prompt_file ~max_steps ~tools ~profile ~images ~context ~prompt =
  let fail msg = fail_with mode msg in
  (* Nested-op handshake: restore real PATH from OP_HOST_PATH and bound
     recursion via OP_DEPTH. *)
  Option.iter (Unix.putenv "PATH") (Sys.getenv_opt "OP_HOST_PATH");
  (* Reject malformed OP_DEPTH rather than defaulting to 0. *)
  let depth =
    match Sys.getenv_opt "OP_DEPTH" with
    | None -> 0
    | Some d -> (
        match int_of_string_opt d with
        | Some n when n >= 0 -> n
        | _ -> Fmt.kstr fail "op: invalid OP_DEPTH=%S; refusing to run" d)
  in
  if depth > 2 then
    Fmt.kstr fail "op recursion too deep (OP_DEPTH=%d > 2); refusing to run"
      depth;
  let fail_app e = fail_with mode ?hint:e.hint e.msg in
  (* Resolve cwd once for session dir, resume, and system prompt; catch
     removed-cwd Sys_error to keep the --json error contract. *)
  let cwd =
    try Sys.getcwd ()
    with Sys_error msg ->
      Fmt.kstr fail "cannot determine working directory: %s" msg
  in
  let parent_session =
    Option.map
      (fun id ->
        match Op.Session_resume.path_of ~cwd ~session_id:id with
        | Ok path -> path
        | Error e -> Fmt.kstr fail "--fromSession: %s" e)
      from_session
  in
  let prompt =
    resolve_prompt ~fail
      ~resuming:(Option.is_some session_id || Option.is_some parent_id)
      ~prompt_file ~prompt
  in
  let prompt = expand_prompt ~fail ~context prompt in
  let image_blocks =
    match Op.Image.load_all images with Ok bs -> bs | Error e -> fail e
  in
  match (load_context model_spec, Op.Transport.tls_config ()) with
  | Error e, _ -> fail_app e
  | _, Error e -> fail (Op.Transport.pp_error e)
  | Ok (config, provider, api_key, model), Ok tls -> (
      match Op.Config.resolve_nu_profile config ~name:profile with
      | Error e -> fail e
      | Ok profile -> (
          match resolve_tool_names ~tools ~profile with
          | Error e -> fail e
          | Ok tool_names ->
              run_with_context ~env ~mode ~cwd ~config ~provider ~api_key ~model
                ~tls ~session_id ~parent_id ~parent_session ~max_steps
                ~tool_names ~depth ~profile ~prompt ~image_blocks))

(* Goal comes from PROMPT or --prompt only; an empty goal is valid and must not
   block on stdin. *)
let handoff_cmd ~env ~(mode : Cli.output) ~session_id ~parent_id ~model_spec
    ~prompt_file ~prompt =
  let fail msg = fail_with mode msg in
  let cwd =
    try Sys.getcwd ()
    with Sys_error msg ->
      Fmt.kstr fail "cannot determine working directory: %s" msg
  in
  let goal =
    match (prompt_file, prompt) with
    | Some _, _ -> resolve_prompt ~fail ~resuming:true ~prompt_file ~prompt
    | None, Some goal -> goal
    | None, None -> ""
  in
  match
    (load_context (handoff_model_spec model_spec), Op.Transport.tls_config ())
  with
  | Error e, _ -> fail_with mode ?hint:e.hint e.msg
  | _, Error e -> fail (Op.Transport.pp_error e)
  | Ok (_, provider, api_key, model), Ok tls ->
      run_handoff ~env ~mode ~cwd ~provider ~api_key ~model ~tls ~session_id
        ~parent_id ~goal

(* Replay a session file or live --stream through the transcript renderer,
   echoing user messages. *)
let render_cmd ~session_id ~file =
  let fail msg = fail_with Cli.Human msg in
  let cwd = try Some (Sys.getcwd ()) with Sys_error _ -> None in
  let path =
    match (session_id, file) with
    | Some _, Some _ -> fail "--render takes --sessionId or a FILE, not both"
    | Some session_id, None -> (
        match
          Op.Session_resume.path_of
            ~cwd:(Option.value cwd ~default:".")
            ~session_id
        with
        | Ok path -> Some path
        | Error e -> fail e)
    | None, file -> file
  in
  let t = Op.Render.v ~color:(use_color ()) ?cwd ~echo_user:true () in
  let replay ic =
    In_channel.fold_lines
      (fun () text ->
        List.iter print_endline (Op.Render.line t text);
        flush stdout)
      () ic
  in
  match path with
  | None -> replay In_channel.stdin
  | Some path -> (
      try In_channel.with_open_bin path replay with Sys_error msg -> fail msg)

let with_eio f =
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  f env

let run : Cli.command -> unit = function
  | Set { kind = Set_default; json; value } -> List_cmd.set_default ~json value
  | List { json; stray_prompt = true; _ } ->
      List_cmd.list_fail ~text_prefix:"" ~json
        "--list does not accept a PROMPT argument"
  | List { kind; json; limit; session_id; stray_prompt = false } ->
      List_cmd.run_list ~json ~limit ~session_id kind
  | Render { session_id; file } -> render_cmd ~session_id ~file
  | Handoff { mode; session_id; parent_id; model_spec; prompt_file; prompt } ->
      with_eio @@ fun env ->
      handoff_cmd ~env ~mode ~session_id ~parent_id ~model_spec ~prompt_file
        ~prompt
  | Run r ->
      with_eio @@ fun env ->
      run_prompt ~env ~mode:r.mode ~session_id:r.session_id
        ~parent_id:r.parent_id ~from_session:r.from_session
        ~model_spec:r.model_spec ~prompt_file:r.prompt_file
        ~max_steps:r.max_steps ~tools:r.tools ~profile:r.profile
        ~images:r.images ~context:r.context ~prompt:r.prompt

let () = exit (Cmdliner.Cmd.eval (Cli.cmd run))
