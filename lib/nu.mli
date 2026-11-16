(** Nushell script execution for the [nu] tool.

    Runs a Nushell script (a tool parameter — never a shell-quoted string —
    written to a temp file and executed with [nu --no-config-file]) under a
    caller-supplied environment — the per-run toolbox PATH plus the OP_HOST_PATH
    handshake — and an explicit [executable] (nu's absolute path, resolved
    against the host PATH, because the toolbox PATH excludes nu itself). The
    static AST gate ({!Gate_nu}) vets the script before it is run.

    A toolbox profile may extend the run with vetted modules and plugins. Both
    apply to the run {e and} the AST extraction, so the parser state the gate
    inspects matches the one the script executes under:
    - [modules]: [--include-path] makes [use <name>] resolve module files at
      parse time (colon-separated dirs).
    - [plugins]: nu's [--no-config-file] hard-disables the plugin registry, so a
      plugin-bearing run swaps it for empty op-managed config stubs
      ([<stub_dir>/env.nu], [<stub_dir>/config.nu]) plus
      [--plugin-config <registry>]; a bare [plugin use <name>] then loads the
      plugin at parse time. *)

type modules = { names : string list; include_path : string }
(** [modules] The startup snapshot of importable names (for the tool
    description) and the colon-separated [--include-path]. The gate rescans
    those directories and vets imported sources on every call. *)

type plugin_command = { command : string; summary : string }
(** [plugin_command] One plugin command and its one-line description, as the
    plugin itself declares it (the standard nu plugin signature). *)

type plugins = {
  names : string list;
  registry : string;
  stub_dir : string;
  commands : (string * plugin_command list) list;
}
(** [plugins] The loadable plugin names, the op-managed registry file for
    [--plugin-config], the directory holding the empty [env.nu]/[config.nu]
    stubs that replace [--no-config-file], and each plugin's declared commands
    (for the tool description; may be empty when the registry could not be
    read). *)

type error = Spawn_error of string | Timeout of float * string

val pp_error : error -> string
(** [pp_error e] is a human-readable message for error [e]. *)

val run :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  ?timeout:float ->
  ?modules:modules ->
  ?plugins:plugins ->
  env:string array ->
  executable:string ->
  script:string ->
  unit ->
  (string * int, error) result
(** [run ~proc_mgr ~clock ?timeout ?modules ?plugins ~env ~executable ~script
     ()] writes [script] to a temp file, runs it with environment [env] and
    binary [executable], and returns the (possibly truncated) combined
    stdout+stderr and the exit code. Times out after [timeout] seconds (default
    120), killing the process. The temp file is always removed. *)

val ast :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  ?modules:modules ->
  ?plugins:plugins ->
  executable:string ->
  string ->
  (Yojson.Safe.t, string) result
(** [ast ~proc_mgr ~clock ?modules ?plugins ~executable script] returns the
    Nushell AST of [script] as JSON (via [nu ... ast ... --json --flatten]) for
    {!Gate_nu}'s static walk, or [Error msg] carrying nu's parse error (or a
    spawn/JSON failure). The script is passed through a temp file, never
    embedded in nu's [-c] string. Partially applied in main to build the [ast]
    closure {!Gate_nu.verdict} depends on. *)

val register_plugin :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  executable:string ->
  registry:string ->
  binary:string ->
  (unit, string) result
(** [register_plugin ~proc_mgr ~clock ~executable ~registry ~binary] registers
    the plugin [binary] into the op-managed [registry] file (creating it if
    absent) via nu's [plugin add], which executes the binary to collect its
    command signatures. Main calls this only for profile-listed plugins, and
    only when the registry is missing or older than the binary. *)

val plugin_commands :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  executable:string ->
  registry:string ->
  ((string * plugin_command list) list, string) result
(** [plugin_commands ~proc_mgr ~clock ~executable ~registry] reads each
    registered plugin's commands and descriptions from [registry] via nu's
    [plugin list]. It reads the registry file only; no plugin binary runs. *)

val version :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  executable:string ->
  string option
(** [version ~proc_mgr ~clock ~executable] is what [nu --version] prints
    (["0.117.1"]), or [None] when the probe fails. *)

val tool :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  ?version:string ->
  ?modules:modules ->
  ?plugins:plugins ->
  env:(unit -> string array) ->
  executable:string ->
  externals:string list ->
  unit ->
  Tool.t
(** [tool ~proc_mgr ~clock ?version ?modules ?plugins ~env ~executable
     ~externals ()] is the nu tool with its Eio capabilities, per-call child
    [env] closure, nu [executable], and toolbox [externals] captured. [version],
    [externals], module names, and plugin names are interpolated into the tool
    description so the model knows what is available. *)

(** Internal helpers exposed for testing. *)
module For_testing : sig
  val env_with : (string * string) list -> string array
  (** [env_with overrides] is the child environment op spawns nu with: the
      inherited environment with any entry whose name is in [overrides] removed,
      then [overrides] appended as [NAME=VALUE]. No name appears twice, so
      execve's unspecified duplicate-entry handling — nu takes the last, C
      getenv the first — never decides which value the child sees. *)

  val plugin_note : plugins option -> string
  (** [plugin_note plugins] is the tool-description paragraph naming each
      plugin, how to load it, its main command's description and its subcommands
      (a count plus a [help] pointer for large plugins). *)
end
