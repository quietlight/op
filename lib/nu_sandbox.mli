(** The [nu] tool's runtime: turning a toolbox profile into a nu child that can
    run only what the profile grants.

    {!setup} resolves nu, builds the per-run toolbox (a temp dir of symlinks to
    the profile's externals, which becomes the child's PATH), prepares the
    module include path and the profile's plugin registry, and returns the tool
    together with the static gate that vets each script. The pieces with a
    security job of their own are exposed so they can be tested directly:
    {!child_env} decides what the script can read from the environment, and
    {!which} decides what gets linked into the toolbox or executed as a plugin.
*)

val setup :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  warn:(string -> unit) ->
  profile:Config.nu_profile ->
  depth:int ->
  (Tool.t * Gate.nu_gate) option
(** [setup ~proc_mgr ~clock ~warn ~profile ~depth] builds the run's nu tool and
    its gate ({!Gate_nu.verdict}) for [profile], over the same parser state,
    externals, module scanner and plugin names, or is [None] (after a [warn])
    when nu is not on PATH. Missing externals, unreadable modules and
    unregisterable plugins are each warned and dropped rather than failing the
    run. [depth] is this op's [OP_DEPTH]; the child sees [depth + 1]. The
    toolbox dir is removed at process exit. *)

val which : path:string -> string -> string option
(** [which ~path name] is the first {e executable regular file} called [name] on
    the {!Util.path_list_sep}-separated [path], following symlinks. A directory
    or non-executable file of that name is not a hit. *)

val base_env_allow : string list
(** The environment names every profile's nu child inherits: locale, home and
    temp dir, and nothing that carries a credential. *)

val child_env :
  host_env:string array ->
  child_path:string ->
  host_path:string ->
  depth:int ->
  profile_name:string ->
  externals:string list ->
  module_names:string list ->
  env_allow:string list ->
  string array
(** [child_env ~host_env ~child_path ~host_path ~depth ~profile_name ~externals
     ~module_names ~env_allow] is the nu child's environment: the entries of
    [host_env] whose name is in {!base_env_allow} or [env_allow] — an allowlist,
    so an inherited API key never reaches a script — then [PATH] (set to
    [child_path]), the [OP_HOST_PATH] and [OP_DEPTH] handshake, and the
    [OP_NU_PROFILE] / [OP_NU_EXTERNALS] / [OP_NU_MODULES] introspection
    variables. *)

val scan_module_sources :
  warn:(string -> unit) -> string list -> unit -> (string * string) list
(** [scan_module_sources ~warn dirs ()] re-reads every [*.nu] file in [dirs] as
    [(file name, source)] pairs. The first dir wins a name collision, missing
    dirs are skipped, and a file larger than {!Gate_nu.max_script_chars} is
    warned and left out before it is read. *)

val registry_path : agent_dir:string -> profile_name:string -> string
(** [registry_path ~agent_dir ~profile_name] is the plugin registry file for one
    profile. Each profile has its own, so a run can only load the plugins its
    own profile lists. *)
