(* The nu tool's runtime: everything between "the profile says nu may run
   these" and a spawned nu child that can run only those. See nu_sandbox.mli. *)

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
  String.split_on_char Util.path_list_sep.[0] path
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
let build_toolbox ~warn ~path ~externals =
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

(* Expand every configured module dir for nu's include path. Missing dirs are
   warned once but retained: an agent may create the dir and a module mid-run. *)
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

(* Build a scanner that re-reads [*.nu] files on each call. Missing dirs are
   silently skipped after the startup warning; first dir wins name collisions,
   mirroring PATH. Dot-prefixed modules retain the existing behavior. *)
let scan_module_sources ~warn dirs () =
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
               in full on every gate call (memory), and fed to `nu ast`. Skip an
               oversized file before reading it; it becomes un-importable (a
               clean "module not found" deny) rather than an amplifier. *)
            let oversized =
              match Unix.stat path with
              | { st_size; _ } -> st_size > Gate_nu.max_script_chars
              | exception Unix.Unix_error _ -> false
            in
            if oversized then (
              Fmt.kstr warn
                "nu modules: %s skipped (larger than the %d-byte module cap)"
                path Gate_nu.max_script_chars;
              None)
            else
              match Util.read_file path with
              | Ok source -> Some (name, source)
              | Error msg ->
                  Fmt.kstr warn "nu modules: cannot read %s: %s" path msg;
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
let child_env ~host_env ~child_path ~host_path ~depth ~profile_name ~externals
    ~module_names ~env_allow =
  let allowed = base_env_allow @ env_allow in
  (* Split at '=' rather than matching a "NAME=" prefix, so HOME admits HOME
     alone and not HOMEBREW_PREFIX. The OP_* / PATH entries appended below need
     no exclusion here: an allowlist cannot admit them in the first place. *)
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

(* The plugins' own declared commands, for the tool description; when the
   registry cannot be read the description falls back to bare names. *)
let plugin_catalog ~warn ~proc_mgr ~clock ~executable ~registry =
  match Nu.plugin_commands ~proc_mgr ~clock ~executable ~registry with
  | Ok commands -> commands
  | Error msg ->
      Fmt.kstr warn "nu plugins: %s" msg;
      []

(* One registry per profile. A registry lists every plugin `plugin use` can
   load, so a shared file would let a profile load (and be told about) the
   plugins of whichever profile registered last. The profile name is a JSON
   key, so anything outside a conservative set is flattened to '_' — and a
   name that needed flattening also carries a digest of the original, so
   [data/dev] and [data_dev] do not land on the same file. *)
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

(* Resolve [nu_plugin_<name>] binaries and (re)build the profile's op-managed
   plugin registry when it is missing or older than a binary. [plugin add] executes
   the binary, so only profile-listed plugins ever run. Unresolvable or
   unregisterable plugins are warned and dropped, like missing externals. *)
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

(* Build the nu tool and its gate, or warn and return [None] when nu itself is
   not installed (the tool is then simply absent from the run). *)
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
      (* Toolbox first either way, so the script's own externals resolve
         through the vetted symlinks; Host appends the host PATH for the
         subprocess tree (make -> gcc), which allowlisting the build drivers
         already conceded. *)
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
