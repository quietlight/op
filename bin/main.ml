(* op - minimal one-shot agentic loop.

   Keep Eio-facing orchestration in this file: TLS construction, Stdenv
   capability extraction, Eio-backed tools, and the agent loop wiring should be
   visible at the executable boundary. *)

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

(* Build the selected tool set. [nu] is constructed only when requested, and
   brings its gate with it. A backend with no process support (eio_windows
   raises [Failure] from [process_mgr]) drops the tool: op still runs, without
   [nu]. *)
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

(* [resume] is a stderr trailer for the machine modes, whose stdout is JSON a
   human is not reading. The human transcript already ends with the same line in
   its footer, so repeating it here would just print it twice. *)
let close_session ~(mode : Cli.output) session =
  Option.iter
    (fun s ->
      match (mode, Op.Session.leaf s) with
      | Cli.Human, _ -> ()
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

(* Colour is for a human at a terminal only: off down a pipe, off when NO_COLOR
   asks, off for a terminal that cannot render it. *)
let use_color () =
  Unix.isatty Unix.stdout
  && Sys.getenv_opt "NO_COLOR" = None
  && Sys.getenv_opt "TERM" <> Some "dumb"

(* One renderer per run, built lazily so the machine modes never construct it.
   It is shared by the entry sink and the footer: the tool calls it remembers
   are what let a result name the file it read. *)
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
          (* Base-scoped: a resumed run replays turns that carry the usage the
             session recorded, and reporting those again would double-count. *)
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

(* Per-event sink: session logging, plus the one warning the session lines
   cannot carry (repaired tool-call ids) on stderr. *)
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

(* Open a fresh session, or resume/branch an existing one per the CLI flags.
   Returns the session (if logging is possible), the prior history to replay,
   and any leaf-correction to warn about. *)
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

(* Report a hard failure and exit 1. Human mode says it in prose on stderr —
   stdout is the transcript, and half a transcript plus a JSON object is not a
   thing to read; the machine modes keep the error envelope on stdout, tagged
   under --stream where it shares the stream with the session entry lines. *)
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

(* What each session line does on its way to stdout. Under --stream it is teed
   verbatim, byte-identical to the file's; under the default it is rendered.
   Either way it is flushed per entry, so the run reads live. Absent under
   --json, which reports the envelope alone. *)
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

(* The context cap: the run stops once the last turn's reported usage crosses
   [contextLimit] of the model's [contextWindow]. Returns [None] — no cap, and
   no [context] object in the envelope — when models.json does not give this
   model a window, which is also the case for a --model override naming an
   unlisted one. *)
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

(* The run's tail: report the outcome (one envelope, tagged when it shares
   stdout with the streamed entry lines), print the stderr resume hint, close
   the session, and exit with the classified code. *)
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
  (* The entries are produced whether or not they can be saved: a run whose
     log could not be opened still prints its transcript. [session] stays the
     saved one, which is what the envelope and the resume hint may name. *)
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
  (* The prompt reflects the tools actually built (nu may have been dropped). *)
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

(* The handoff document on stdout: raw markdown by default, so it pipes straight
   back into op, or the full record under --json for hop and other callers that
   also want the file paths as data. This is the one place op's
   default stdout is not JSON — in handoff mode the prompt *is* the payload,
   just as --list's default is a table. *)
let report_handoff ~json ~goal ~session_id ~model ~files document =
  if not json then print_string document
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

(* --handoff: read a session and ask a model to write the opening prompt for a
   fresh one. Deliberately not the agent loop — no tools, no gate, one call —
   and the source session is only read, never appended to. *)
let run_handoff ~env ~json ~cwd ~provider ~api_key ~model ~tls ~session_id
    ~parent_id ~goal =
  let fail msg = fail_with (if json then Cli.Json else Cli.Stream) msg in
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

(* --handoff picks its own model: the document seeds the next thread, so it is
   worth the strongest model configured, and it must hold the whole session. An
   explicit --model still wins. Config is re-read here rather than threaded
   through, so [load_context] stays the single place that reports config
   errors. *)
let handoff_model_spec model_spec =
  match model_spec with
  | Some _ -> model_spec
  | None -> (
      match Op.Config.load_config () with
      | Ok config -> Op.Config.handoff_model config
      | Error _ -> None)

(* The run's tool set: --tools wins, then the profile's [tools] list, then
   every known tool. --tools was validated by its cmdliner conv; the profile's
   list is hand-edited JSON and is validated here, so a typo fails the run
   rather than silently granting the full set. *)
let resolve_tool_names ~tools ~(profile : Op.Config.nu_profile) =
  match (tools, profile.tools) with
  | Some names, _ -> Ok names
  | None, None -> Ok Op.Tools.known
  | None, Some names ->
      Result.map_error
        (fun msg -> Fmt.str "profile '%s': %s" profile.name msg)
        (Op.Tools.validate names)

(* Prepend the --context file blocks to the user's prompt. *)
let expand_prompt ~fail ~context prompt =
  let files =
    List.map
      (fun path ->
        match Op.Util.read_file path with
        | Ok contents -> (path, contents)
        (* [read_file]'s message already names the path. *)
        | Error msg -> Fmt.kstr fail "--context: %s" msg)
      context
  in
  Op.Context.render ~files ~prompt

let run_prompt ~env ~mode ~session_id ~parent_id ~from_session ~model_spec
    ~prompt_file ~max_steps ~tools ~profile ~images ~context ~prompt =
  let fail msg = fail_with mode msg in
  (* Nested-op handshake: a nested op (spawned from inside a nu script) inherits
     the nu toolbox PATH plus the OP_HOST_PATH/OP_DEPTH markers. Restore the real
     PATH so this op's own tools and toolbox resolve, and refuse to run past a
     bounded recursion depth so op->nu->op cannot run away. *)
  Option.iter (Unix.putenv "PATH") (Sys.getenv_opt "OP_HOST_PATH");
  let depth =
    match Sys.getenv_opt "OP_DEPTH" with
    | Some d -> Option.value (int_of_string_opt d) ~default:0
    | None -> 0
  in
  if depth > 2 then
    Fmt.kstr fail "op recursion too deep (OP_DEPTH=%d > 2); refusing to run"
      depth;
  let fail_app e = fail_with mode ?hint:e.hint e.msg in
  (* Resolve the working directory once so every downstream consumer — session
     dir, resume, and the system prompt — agrees on it. *)
  let cwd =
    (* [Sys.getcwd] raises if the working directory was removed under us; keep
       the --json error contract instead of leaking a backtrace. *)
    try Sys.getcwd ()
    with Sys_error msg ->
      Fmt.kstr fail "cannot determine working directory: %s" msg
  in
  (* --fromSession records provenance on a *fresh* session: the new thread
     continues the old one's work with a clean context (see --handoff). *)
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

(* The --handoff entry point. Its goal comes from the PROMPT argument or
   --prompt only: an absent goal is legitimate ("carry on from where this left
   off"), so it must not fall through to reading stdin, which would block. *)
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
      run_handoff ~env ~json:(mode = Cli.Json) ~cwd ~provider ~api_key ~model
        ~tls ~session_id ~parent_id ~goal

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
