(** Nushell execution, AST extraction, and plugin helpers for the [nu] tool.

    Scripts are written to temp files and executed with an explicit [executable]
    (since the toolbox [PATH] excludes [nu]). Modules ([--include-path]) and
    plugins (empty config stubs + [--plugin-config], because [--no-config-file]
    disables plugins) are passed identically to both {!run} and {!ast} so the
    gated parser state matches execution. *)

type modules = { names : string list; include_path : string }
(** Startup module-name snapshot (for the tool description) and
    [--include-path]. *)

type plugin_command = { command : string; summary : string }
(** One plugin command name and summary from the plugin registry. *)

type plugins = {
  names : string list;
  registry : string;
  stub_dir : string;
  commands : (string * plugin_command list) list;
}
(** Configured plugin names, op-managed registry path, empty config stub dir,
    and declared commands per plugin. *)

type error = Spawn_error of string | Timeout of float * string

val pp_error : error -> string
(** [pp_error e] formats [e] for display. *)

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
     ()] executes [script] via a temp file with [timeout] (default 120s),
    returning tail-truncated combined output and exit code. *)

val ast :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  ?modules:modules ->
  ?plugins:plugins ->
  executable:string ->
  string ->
  (Yojson.Safe.t, string) result
(** [ast ~proc_mgr ~clock ?modules ?plugins ~executable script] runs
    [nu ... ast ... --json --flatten] via a temp file and returns the token
    stream JSON for {!Gate_nu}. *)

val register_plugin :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  executable:string ->
  registry:string ->
  binary:string ->
  (unit, string) result
(** [register_plugin ~proc_mgr ~clock ~executable ~registry ~binary] registers
    [binary] in [registry] via [plugin add] (executes [binary] to collect
    signatures). *)

val plugin_commands :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  executable:string ->
  registry:string ->
  ((string * plugin_command list) list, string) result
(** [plugin_commands ~proc_mgr ~clock ~executable ~registry] reads plugin
    commands from [registry] via [plugin list] without executing plugin
    binaries. *)

val version :
  proc_mgr:_ Eio.Process.mgr ->
  clock:_ Eio.Time.clock ->
  executable:string ->
  string option
(** [version ~proc_mgr ~clock ~executable] returns [nu --version], or [None] on
    failure. *)

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
     ~externals ()] builds the [nu] {!Tool.t} with profile metadata in its
    description. *)

(** Internal helpers for testing. *)
module For_testing : sig
  val env_with : (string * string) list -> string array
  (** [env_with overrides] builds the allowlisted helper environment with
      [overrides] replacing any matching inherited entries (no duplicates). *)

  val plugin_note : plugins option -> string
  (** [plugin_note plugins] formats the plugin section of the tool description.
  *)
end
