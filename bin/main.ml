(* op - minimal one-shot agentic loop.

   Keep Eio-facing orchestration in this file: TLS construction, Stdenv
   capability extraction, Eio-backed tools, and the agent loop wiring should be
   visible at the executable boundary. *)

let ( let* ) = Result.bind

type app_error = { msg : string; hint : string option }

let app_error ?hint msg = Error { msg; hint }

let app_error_of_config e =
  let msg, hint = Op.Config.json_error e in
  { msg; hint }

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
    Op.Config.load_config () |> Result.map_error app_error_of_config
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

let log_event session (provider : Op.Config.provider) model :
    Op.Agent.event -> unit =
  let api = Op.Config.string_of_api provider.api in
  fun event ->
    match (session, event) with
    | Some s, Op.Agent.Assistant_turn turn ->
        Op.Session.append_assistant s ~api ~provider:provider.name ~model turn
    | Some s, Tool_finished tr -> Op.Session.append_tool_result s tr
    | _, _ -> ()

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

(* The context the nu gate needs, carried out of tool construction: the AST
   closure, toolbox allowlist, per-call module scanner, and plugin names. *)
type nu_ctx = {
  ast : string -> (Yojson.Safe.t, string) result;
  externals : string list;
  scan_modules : unit -> (string * string) list;
  plugins : string list;
}

(* First hit for [name] on the [Util.path_list_sep]-separated [path] (a small [which] walk).
   Requires an executable regular file, as a real [which] does: [Sys.file_exists]
   alone also matches a *directory* named e.g. "nu", which would then be
   symlinked into the toolbox as if it were the tool, or handed to
   [register_plugin] to execute. Follows symlinks ([Unix.stat], not [lstat]) —
   a PATH entry that is a symlink to the real binary is the normal case. *)
let which ~path name =
  let is_executable_file candidate =
    match Unix.stat candidate with
    | { Unix.st_kind = Unix.S_REG; _ } -> (
        try
          Unix.access candidate [ Unix.X_OK ];
          true
        with Unix.Unix_error _ -> false)
    | _ | (exception Unix.Unix_error _) -> false
  in
  String.split_on_char Op.Util.path_list_sep.[0] path
  |> List.find_map (fun dir ->
      if dir = "" then None
      else
        let candidate = Filename.concat dir name in
        if is_executable_file candidate then Some candidate else None)

(* Remove the toolbox temp dir and its symlinks. op is one-shot, so this runs
   at process exit; best-effort, since a leaked temp dir must never abort a run. *)
let cleanup_toolbox dir =
  (try
     Sys.readdir dir
     |> Array.iter (fun name ->
         try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
   with Sys_error _ -> ());
  try Unix.rmdir dir with Unix.Unix_error _ -> ()

(* Build the per-run toolbox: a fresh temp dir of symlinks to each allowed
   external, resolved against the host [path]. Names not found are skipped with
   a stderr warning (a nu script naming a missing external will simply fail at
   runtime). The dir is the nu process's entire PATH, cleaned up at exit. *)
let build_toolbox ~path ~externals =
  let dir = Filename.temp_file "op-toolbox" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  at_exit (fun () -> cleanup_toolbox dir);
  List.iter
    (fun name ->
      match which ~path name with
      | Some target -> (
          try Unix.symlink target (Filename.concat dir name)
          with Unix.Unix_error (e, _, _) ->
            Fmt.epr "warning: nu toolbox: cannot link %s: %s@." name
              (Unix.error_message e))
      | None ->
          Fmt.epr
            "warning: nu toolbox: external %s not found on PATH; skipping@."
            name)
    externals;
  dir

let expand_home path =
  if String.starts_with ~prefix:"~/" path then
    match Op.Util.home_dir () with
    | Some home ->
        Filename.concat home (String.sub path 2 (String.length path - 2))
    | None -> path
  else path

(* Expand every configured module dir for nu's include path. Missing dirs are
   warned once but retained: an agent may create the dir and a module mid-run. *)
let module_include_path dirs =
  match List.map expand_home dirs with
  | [] -> None
  | dirs ->
      List.iter
        (fun d ->
          if not (Sys.file_exists d && Sys.is_directory d) then
            Fmt.epr
              "warning: nu modules: %s is not a directory yet; keeping it in \
               the include path@."
              d)
        dirs;
      Some (dirs, String.concat Op.Util.path_list_sep dirs)

(* Build a scanner that re-reads [*.nu] files on each call. Missing dirs are
   silently skipped after the startup warning; first dir wins name collisions,
   mirroring PATH. Dot-prefixed modules retain the existing behavior. *)
let scan_module_sources dirs () =
  let read_dir dir =
    if not (Sys.file_exists dir && Sys.is_directory dir) then []
    else
      Sys.readdir dir |> Array.to_list |> List.sort String.compare
      |> List.filter_map (fun name ->
          if not (Filename.check_suffix name ".nu") then None
          else
            let path = Filename.concat dir name in
            (* Cap the read: module dirs are writable by the nu tool alone, so a
               runaway run can drop a huge .nu file that would otherwise be read
               in full on every gate call (memory), fed to `nu ast`, and — if it
               names a sensitive path — appended to the judge payload. Skip an
               oversized file before reading it; it becomes un-importable (a
               clean "module not found" deny) rather than an amplifier. *)
            let oversized =
              match Unix.stat path with
              | { st_size; _ } -> st_size > Op.Gate_nu.max_script_chars
              | exception _ -> false
            in
            if oversized then (
              Fmt.epr
                "warning: nu modules: %s skipped (larger than the %d-byte \
                 module cap)@."
                path Op.Gate_nu.max_script_chars;
              None)
            else
              match Op.Util.read_file path with
              | Ok source -> Some (name, source)
              | Error msg ->
                  Fmt.epr "warning: nu modules: cannot read %s: %s@." path msg;
                  None)
  in
  List.concat_map read_dir dirs
  |> List.fold_left
       (fun acc (name, src) ->
         if List.mem_assoc name acc then acc else (name, src) :: acc)
       []
  |> List.rev

(* The names every profile inherits: locale, home and temp dir — enough for a
   script (and the build drivers a [pathMode: host] profile allows) to behave
   normally, and nothing that carries a credential. Anything else a profile
   needs is named in its [envAllow]. *)
let base_env_allow =
  [ "HOME"; "USER"; "LOGNAME"; "LANG"; "LC_ALL"; "TERM"; "TMPDIR"; "TZ" ]

(* The nu child's environment: an *allowlist* of the host env — [base_env_allow]
   plus the profile's [envAllow] — with PATH replaced by [child_path] (the
   toolbox dir, or toolbox:host for a [pathMode: host] profile), plus the
   OP_HOST_PATH handshake (so a nested op restores its real PATH), an
   incremented OP_DEPTH (so op->nu->op recursion is bounded), and the
   OP_NU_PROFILE / OP_NU_EXTERNALS / OP_NU_MODULES introspection vars so a
   script can see which profile it runs under and what that profile grants.

   Allowlist, not deny-list: reading [$env] needs no external command and names
   no sensitive path, so neither gate layer fires on it — an inherited
   ANTHROPIC_API_KEY or SSH_AUTH_SOCK would be one clean-verdict line of nu away
   from the tool result, and thence the model and the provider. A name-pattern
   blocklist would fail open on the next variable someone invents; the set a
   sandboxed script legitimately needs is small enough to enumerate. *)
let nu_child_env ~child_path ~host_path ~depth ~profile_name ~externals
    ~module_names ~env_allow =
  let allowed = base_env_allow @ env_allow in
  (* Split at '=' rather than matching a "NAME=" prefix, so HOME admits HOME
     alone and not HOMEBREW_PREFIX. The OP_* / PATH entries appended below need
     no exclusion here: an allowlist cannot admit them in the first place. *)
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun kv ->
        match String.index_opt kv '=' with
        | None -> false
        | Some i -> List.mem (String.sub kv 0 i) allowed)
  in
  Array.of_list
    (inherited
    @ [
        "PATH=" ^ child_path;
        "OP_HOST_PATH=" ^ host_path;
        Fmt.str "OP_DEPTH=%d" depth;
        "OP_NU_PROFILE=" ^ profile_name;
        "OP_NU_EXTERNALS=" ^ String.concat "," externals;
        "OP_NU_MODULES=" ^ String.concat "," module_names;
      ])

let warn_plugins fmt = Fmt.kstr (Fmt.epr "warning: nu plugins: %s@.") fmt

(* Ensure the empty env/config stubs that replace [--no-config-file] (which
   hard-disables the plugin registry in nu 0.114); returns the stub dir. *)
let ensure_stub_dir agent_dir =
  let stub_dir = Filename.concat agent_dir "nu-stubs" in
  (try Unix.mkdir stub_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter
    (fun f ->
      let p = Filename.concat stub_dir f in
      if not (Sys.file_exists p) then Out_channel.with_open_bin p (fun _ -> ()))
    [ "env.nu"; "config.nu" ];
  stub_dir

(* Resolve [nu_plugin_<name>] binaries and (re)build the op-managed plugin
   registry when it is missing or older than a binary. [plugin add] executes
   the binary, so only profile-listed plugins ever run. Unresolvable or
   unregisterable plugins are warned and dropped, like missing externals. *)
let setup_plugins ~proc_mgr ~clock ~executable ~host_path names =
  let mtime path =
    match Unix.stat path with
    | st -> Some st.Unix.st_mtime
    | exception Unix.Unix_error _ -> None
  in
  match (names, Op.Util.home_dir ()) with
  | [], _ -> None
  | _, None ->
      warn_plugins "HOME not set; skipping";
      None
  | names, Some home -> (
      let agent_dir = Filename.concat home ".op_global/agent" in
      let stub_dir = ensure_stub_dir agent_dir in
      let registry = Filename.concat agent_dir "nu-plugin.msgpackz" in
      let resolved =
        List.filter_map
          (fun n ->
            let bin = "nu_plugin_" ^ n in
            match which ~path:host_path bin with
            | Some b -> Some (n, b)
            | None ->
                warn_plugins "%s not found on PATH; skipping" bin;
                None)
          names
      in
      (* Stale when a plugin binary — or settings.json, where the plugin list
         lives — is newer than the registry: a freshly listed plugin must
         trigger a rebuild even though its binary predates the registry. *)
      let stale =
        match mtime registry with
        | None -> resolved <> []
        | Some t ->
            let newer p =
              match mtime p with Some pt -> pt > t | None -> false
            in
            newer (Filename.concat agent_dir "settings.json")
            || List.exists (fun (_, b) -> newer b) resolved
      in
      let registered =
        if not stale then List.map fst resolved
        else (
          (try Sys.remove registry with Sys_error _ -> ());
          List.filter_map
            (fun (n, binary) ->
              match
                Op.Nu.register_plugin ~proc_mgr ~clock ~executable ~registry
                  ~binary
              with
              | Ok () -> Some n
              | Error msg ->
                  warn_plugins "%s" msg;
                  None)
            resolved)
      in
      match registered with
      | [] -> None
      | names -> Some { Op.Nu.names; registry; stub_dir })

(* Build the nu tool and its gate context, or warn and return [None] when nu
   itself is not installed (the tool is then simply absent from the run). The
   same fallback covers a backend with no process support (eio_windows raises
   [Failure] from [process_mgr]): op still runs, without [nu]. *)
let setup_nu ~env ~profile ~depth =
  let proc_mgr =
    try Some (Eio.Stdenv.process_mgr env) with Failure _ -> None
  in
  let clock = Eio.Stdenv.clock env in
  let host_path = try Sys.getenv "PATH" with Not_found -> "" in
  match (proc_mgr, which ~path:host_path "nu") with
  | None, _ ->
      Fmt.epr
        "warning: nu tool requested but this platform cannot spawn processes; \
         dropping it@.";
      None
  | _, None ->
      Fmt.epr "warning: nu tool requested but nu is not on PATH; dropping it@.";
      None
  | Some proc_mgr, Some executable ->
      let externals = profile.Op.Config.externals in
      let module_path = module_include_path profile.Op.Config.module_dirs in
      let module_dirs =
        match module_path with Some (dirs, _) -> dirs | None -> []
      in
      let scan_modules = scan_module_sources module_dirs in
      let startup_modules = scan_modules () in
      let modules =
        Option.map
          (fun (_, include_path) ->
            { Op.Nu.names = List.map fst startup_modules; include_path })
          module_path
      in
      let plugins =
        setup_plugins ~proc_mgr ~clock ~executable ~host_path
          profile.Op.Config.plugins
      in
      let plugin_names =
        match plugins with Some p -> p.Op.Nu.names | None -> []
      in
      let toolbox_dir = build_toolbox ~path:host_path ~externals in
      (* Toolbox first either way, so the script's own externals resolve
         through the vetted symlinks; Host appends the host PATH for the
         subprocess tree (make -> gcc), which allowlisting the build drivers
         already conceded. *)
      let child_path =
        match profile.Op.Config.path_mode with
        | Op.Config.Toolbox -> toolbox_dir
        | Op.Config.Host -> toolbox_dir ^ Op.Util.path_list_sep ^ host_path
      in
      let nu_env () =
        nu_child_env ~child_path ~host_path ~depth:(depth + 1)
          ~profile_name:profile.Op.Config.name ~externals
          ~module_names:(List.map fst (scan_modules ()))
          ~env_allow:profile.Op.Config.env_allow
      in
      let tool =
        Op.Nu.tool ~proc_mgr ~clock ?modules ?plugins ~env:nu_env ~executable
          ~externals ()
      in
      let ast = Op.Nu.ast ~proc_mgr ~clock ?modules ?plugins ~executable in
      Some (tool, { ast; externals; scan_modules; plugins = plugin_names })

(* Build the selected tool set. [nu] is constructed only when requested and
   additionally carries the gate context ([nu_ctx]) when built. *)
let build_tools ~env ~profile ~tool_names ~depth =
  let nu_tool, nu_ctx =
    if List.mem "nu" tool_names then
      match setup_nu ~env ~profile ~depth with
      | Some (tool, ctx) -> (Some tool, Some ctx)
      | None -> (None, None)
    else (None, None)
  in
  (Op.Tools.select ~nu:nu_tool tool_names, nu_ctx)

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
  (call, post)

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
let close_session ~(mode : Cli.output) session gate_log =
  Option.iter
    (fun s ->
      match (mode, Op.Session.leaf s) with
      | Cli.Human, _ -> ()
      | _, Some leaf ->
          Fmt.epr "resume: op --sessionId %s --parentId %s \"prompt\"\n%!"
            (Op.Session.id s) leaf
      | _, None -> Fmt.epr "session: %s\n%!" (Op.Session.id s))
    session;
  Option.iter
    (fun p -> Fmt.epr "gate log: %s\n%!" p)
    (Op.Gate_log.path gate_log);
  Op.Gate_log.close gate_log;
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
  let session_fields = Op.Run_summary.session_fields session in
  let envelope =
    match result with
    | Error e ->
        Op.Run_report.error_json ~typed ~msg:e ~session_fields ~model
          ~metadata:extra_metadata ()
    | Ok (history, outcome) ->
        let answer =
          Option.value ~default:"" (Op.Run_summary.last_assistant_text history)
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
            @ Op.Run_summary.run_metadata ~base history
            @ Op.Run_report.context_fields ~window history)
          ~outcome ()
  in
  match mode with
  | Cli.Human ->
      List.iter print_endline (Op.Render.footer (Lazy.force renderer) envelope)
  | Cli.Stream | Cli.Json -> print_endline (Yojson.Safe.to_string envelope)

(* The gate-judge audit log: lazy (nothing on disk until a judge call), rooted
   beside the main sessions under ~/.op_global/gate and linked to this run's
   session file via the header's parentSession. *)
let open_gate_log ~cwd ~session =
  Op.Gate_log.v ~cwd
    ~parent_session:(Option.map Op.Session.path session)
    ~warn:(fun msg -> Fmt.epr "warning: %s\n%!" msg)
    ()

(* Resolve the gate's LLM judge. A judge configured but unusable is a warned
   fallback, not a hard error: the gate then denies anything off its allowlist. *)
let resolve_judge ~config ~post ~gate_log =
  match Op.Gate_judge.resolve ~log:gate_log config ~post with
  | Op.Gate_judge.Ready j -> Some j
  | Not_configured -> None
  | Unusable reason ->
      Fmt.epr
        "warning: gate judge disabled — %s; commands off the allowlist will be \
         denied@."
        reason;
      None

(* Assemble the run's gate: the LLM judge (if any), the nu AST verdict when a nu
   tool was built, and the cwd-scoped path checks. *)
let build_gate ~config ~post ~cwd ~nu_ctx ~gate_log =
  let judge = resolve_judge ~config ~post ~gate_log in
  let nu =
    Option.map
      (fun ctx ->
        Op.Gate_nu.verdict ~ast:ctx.ast ~externals:ctx.externals
          ~scan_modules:ctx.scan_modules ~plugins:ctx.plugins ~judge)
      nu_ctx
  in
  Op.Gate.v ?nu ~cwd ()

(* Per-event sink: session logging, plus the one warning the session lines
   cannot carry (repaired tool-call ids) on stderr. *)
let build_emit ~session ~provider ~model =
  let log = log_event session provider model in
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
   stdout with the streamed entry lines), print the stderr trailers (resume
   hint, gate-log path), close the logs, and exit with the classified code. *)
let finish ~(mode : Cli.output) ~session ~gate_log ~base ~window ~model
    ~extra_metadata result =
  report_result ~mode ~session ~base ~window ~model ~extra_metadata result;
  close_session ~mode session gate_log;
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
    Op.Run_summary.branch_correction_metadata branch_correction
  in
  let emit = build_emit ~session ~provider ~model in
  let tools, nu_ctx = build_tools ~env ~profile ~tool_names ~depth in
  let call, post =
    setup_call ~env ~tls ~provider ~api_key ~model ~max_tokens ~tools
  in
  let gate_log = open_gate_log ~cwd ~session in
  let gate = build_gate ~config ~post ~cwd ~nu_ctx ~gate_log in
  Option.iter
    (fun s -> Op.Session.append_user s ~blocks:image_blocks ~text:prompt ())
    session;
  (* The prompt reflects the tools actually built (nu may have been dropped). *)
  let tool_names = List.map (fun (t : Op.Tool.t) -> t.name) tools in
  let messages =
    build_messages ~cwd ~prompt ~image_blocks ~prior_history ~tool_names
  in
  let result =
    Op.Agent.run ~call ~tools ~emit ~gate:(Op.Gate.check gate) ?max_steps
      ?over_context ~messages ()
  in
  finish ~mode ~session ~gate_log ~base:(List.length messages) ~window
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
  let call, _ =
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

let run (opts : Cli.opts) =
  match opts.set_kind with
  | Some Set_default -> List_cmd.set_default ~json:opts.json opts.prompt
  | None -> (
      match opts.list_kind with
      | Some kind ->
          if Option.is_some opts.prompt then
            List_cmd.list_fail ~text_prefix:"" ~json:opts.json
              "--list does not accept a PROMPT argument"
          else
            List_cmd.run_list ~json:opts.json ~limit:opts.limit
              ~session_id:opts.session_id kind
      | None ->
          Eio_main.run @@ fun env ->
          Mirage_crypto_rng_unix.use_default ();
          let mode = Cli.output opts in
          if opts.handoff then
            (* --handoff requires --sessionId; the CLI rejects it otherwise. *)
            handoff_cmd ~env ~mode
              ~session_id:(Option.get opts.session_id)
              ~parent_id:opts.parent_id ~model_spec:opts.model_spec
              ~prompt_file:opts.prompt_file ~prompt:opts.prompt
          else
            run_prompt ~env ~mode ~session_id:opts.session_id
              ~parent_id:opts.parent_id ~from_session:opts.from_session
              ~model_spec:opts.model_spec ~prompt_file:opts.prompt_file
              ~max_steps:opts.max_steps ~tools:opts.tools ~profile:opts.profile
              ~images:opts.images ~context:opts.context ~prompt:opts.prompt)

let () = exit (Cmdliner.Cmd.eval (Cli.cmd run))
