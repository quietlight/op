(* Require an executable regular file (following symlinks) so a directory named
   e.g. "nu" is never symlinked or executed. *)
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
  String.split_on_char Util.path_list_sep.[0] path
  |> List.find_map (fun dir ->
      if dir = "" then None
      else
        let candidate = Filename.concat dir name in
        if is_executable_file candidate then Some candidate else None)

let cleanup_toolbox dir =
  (try
     Sys.readdir dir
     |> Array.iter (fun name ->
         try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
   with Sys_error _ -> ());
  try Unix.rmdir dir with Unix.Unix_error _ -> ()

let build_toolbox ~warn ~path ~externals =
  let dir = Filename.temp_dir ~perms:0o700 "op-toolbox" "" in
  at_exit (fun () -> cleanup_toolbox dir);
  List.iter
    (fun name ->
      match which ~path name with
      | Some target -> (
          try Unix.symlink target (Filename.concat dir name)
          with Unix.Unix_error (e, _, _) ->
            Fmt.kstr warn "nu toolbox: cannot link %s: %s" name
              (Unix.error_message e))
      | None ->
          Fmt.kstr warn "nu toolbox: external %s not found on PATH; skipping"
            name)
    externals;
  dir

let expand_home path =
  if String.starts_with ~prefix:"~/" path then
    match Util.home_dir () with
    | Some home ->
        Filename.concat home (String.sub path 2 (String.length path - 2))
    | None -> path
  else path

(* Keep missing dirs in the include path so an agent can create them mid-run. *)
let module_include_path ~warn dirs =
  match List.map expand_home dirs with
  | [] -> None
  | dirs ->
      List.iter
        (fun d ->
          if not (Sys.file_exists d && Sys.is_directory d) then
            Fmt.kstr warn
              "nu modules: %s is not a directory yet; keeping it in the \
               include path"
              d)
        dirs;
      Some (dirs, String.concat Util.path_list_sep dirs)

let scan_module_sources ~warn dirs () =
  let read_dir dir =
    if not (Sys.file_exists dir && Sys.is_directory dir) then []
    else
      Sys.readdir dir |> Array.to_list |> List.sort String.compare
      |> List.filter_map (fun name ->
          if not (Filename.check_suffix name ".nu") then None
          else
            let path = Filename.concat dir name in
            (* Cap before reading: module dirs are run-writable, so oversized
               files or devices must be skipped before entering memory/AST. *)
            match Util.read_file_capped ~max:Gate_nu.max_script_chars path with
            | Ok source -> Some (name, source)
            | Error msg ->
                Fmt.kstr warn "nu modules: %s skipped: %s" path msg;
                None)
  in
  List.concat_map read_dir dirs
  |> List.fold_left
       (fun acc (name, src) ->
         if List.mem_assoc name acc then acc else (name, src) :: acc)
       []
  |> List.rev

let base_env_allow =
  [ "HOME"; "USER"; "LOGNAME"; "LANG"; "LC_ALL"; "TERM"; "TMPDIR"; "TZ" ]

(* Strict allowlist: reading [$env] in nu bypasses both gate layers, so only
   explicitly allowed host variables may cross into the child. *)
let child_env ~host_env ~child_path ~host_path ~depth ~profile_name ~externals
    ~module_names ~env_allow =
  let allowed = base_env_allow @ env_allow in
  let inherited =
    Array.to_list host_env
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

(* Truncate env/config stubs on every run: nu executes them ungated before each
   script when [--plugin-config] replaces [--no-config-file]. *)
let ensure_stub_dir agent_dir =
  let stub_dir = Filename.concat agent_dir "nu-stubs" in
  (try Unix.mkdir stub_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  List.iter
    (fun f -> Out_channel.with_open_bin (Filename.concat stub_dir f) ignore)
    [ "env.nu"; "config.nu" ];
  stub_dir

let plugin_catalog ~warn ~proc_mgr ~clock ~executable ~registry =
  match Nu.plugin_commands ~proc_mgr ~clock ~executable ~registry with
  | Ok commands -> commands
  | Error msg ->
      Fmt.kstr warn "nu plugins: %s" msg;
      []

(* One registry per profile; append a short hash when sanitizing characters so
   e.g. [data/dev] and [data_dev] stay distinct. *)
let registry_path ~agent_dir ~profile_name =
  let safe =
    String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c -> c
        | _ -> '_')
      profile_name
  in
  let stem =
    if safe = profile_name then safe
    else
      Fmt.str "%s-%s" safe
        (String.sub (Digest.to_hex (Digest.string profile_name)) 0 8)
  in
  Filename.concat agent_dir (Fmt.str "nu-plugin-%s.msgpackz" stem)

let setup_plugins ~warn ~proc_mgr ~clock ~executable ~host_path ~profile_name
    names =
  let mtime path =
    match Unix.stat path with
    | st -> Some st.Unix.st_mtime
    | exception Unix.Unix_error _ -> None
  in
  match (names, Util.home_dir ()) with
  | [], _ -> None
  | _, None ->
      Fmt.kstr warn "nu plugins: HOME not set; skipping";
      None
  | names, Some home -> (
      let agent_dir = Filename.concat home ".op_global/agent" in
      let stub_dir = ensure_stub_dir agent_dir in
      let registry = registry_path ~agent_dir ~profile_name in
      let resolved =
        List.filter_map
          (fun n ->
            let bin = "nu_plugin_" ^ n in
            match which ~path:host_path bin with
            | Some b -> Some (n, b)
            | None ->
                Fmt.kstr warn "nu plugins: %s not found on PATH; skipping" bin;
                None)
          names
      in
      (* Rebuild when [settings.json] or any plugin binary is newer than the
         cached registry. *)
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
                Nu.register_plugin ~proc_mgr ~clock ~executable ~registry
                  ~binary
              with
              | Ok () -> Some n
              | Error msg ->
                  Fmt.kstr warn "nu plugins: %s" msg;
                  None)
            resolved)
      in
      match registered with
      | [] -> None
      | names ->
          let commands =
            plugin_catalog ~warn ~proc_mgr ~clock ~executable ~registry
          in
          Some { Nu.names; registry; stub_dir; commands })

let setup ~proc_mgr ~clock ~warn ~(profile : Config.nu_profile) ~depth =
  let host_path = try Sys.getenv "PATH" with Not_found -> "" in
  match which ~path:host_path "nu" with
  | None ->
      warn "nu tool requested but nu is not on PATH; dropping it";
      None
  | Some executable ->
      let externals = profile.externals in
      let module_path = module_include_path ~warn profile.module_dirs in
      let module_dirs =
        match module_path with Some (dirs, _) -> dirs | None -> []
      in
      let scan_modules = scan_module_sources ~warn module_dirs in
      let startup_modules = scan_modules () in
      let modules =
        Option.map
          (fun (_, include_path) ->
            { Nu.names = List.map fst startup_modules; include_path })
          module_path
      in
      let plugins =
        setup_plugins ~warn ~proc_mgr ~clock ~executable ~host_path
          ~profile_name:profile.name profile.plugins
      in
      let plugin_names =
        match plugins with Some p -> p.Nu.names | None -> []
      in
      let toolbox_dir = build_toolbox ~warn ~path:host_path ~externals in
      (* Toolbox first in both modes; [Host] appends host PATH for build drivers. *)
      let child_path =
        match profile.path_mode with
        | Config.Toolbox -> toolbox_dir
        | Config.Host -> toolbox_dir ^ Util.path_list_sep ^ host_path
      in
      let env () =
        child_env ~host_env:(Unix.environment ()) ~child_path ~host_path
          ~depth:(depth + 1) ~profile_name:profile.name ~externals
          ~module_names:(List.map fst (scan_modules ()))
          ~env_allow:profile.env_allow
      in
      let tool =
        Nu.tool ~proc_mgr ~clock
          ?version:(Nu.version ~proc_mgr ~clock ~executable)
          ?modules ?plugins ~env ~executable ~externals ()
      in
      let ast = Nu.ast ~proc_mgr ~clock ?modules ?plugins ~executable in
      let gate =
        Gate_nu.verdict ~ast ~externals ~scan_modules ~plugins:plugin_names
      in
      Some (tool, gate)
