(** Runtime environment setup for the [nu] tool: per-run symlink toolbox,
    allowlisted child environment, module scanner, and per-profile plugin
    registry. *)

val setup :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  warn:(string -> unit) ->
  profile:Config.nu_profile ->
  depth:int ->
  (Tool.t * Gate.nu_gate) option
(** [setup ~proc_mgr ~clock ~warn ~profile ~depth] builds the [nu] tool and its
    {!Gate_nu.verdict} gate for [profile] (child sees [OP_DEPTH = depth + 1]),
    or returns [None] after calling [warn] if [nu] is not on [PATH]. The toolbox
    temp dir is removed at process exit. *)

val which : path:string -> string -> string option
(** [which ~path name] returns the first executable regular file (following
    symlinks) named [name] on {!Util.path_list_sep}-separated [path]. *)

val base_env_allow : string list
(** Non-credential env vars inherited by every [nu] child (locale, home, temp).
*)

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
     ~module_names ~env_allow] builds the allowlisted [nu] child environment
    ({!base_env_allow} + [env_allow], [PATH], [OP_HOST_PATH], [OP_DEPTH], and
    [OP_NU_*] introspection vars). *)

val scan_module_sources :
  warn:(string -> unit) -> string list -> unit -> (string * string) list
(** [scan_module_sources ~warn dirs ()] reads [*.nu] files across [dirs] (first
    dir wins collisions), skipping missing dirs and files exceeding
    {!Gate_nu.max_script_chars}. *)

val registry_path : agent_dir:string -> profile_name:string -> string
(** [registry_path ~agent_dir ~profile_name] returns the per-profile plugin
    registry path (isolating each profile's loadable plugins). *)
