(** Configuration loading from [~/.op_global/agent]:
    - [models.json]: provider definitions ([baseUrl], optional [api] defaulting
      to ["openai-completions"], [apiKey] or [apiKeyEnv], and [models]).
    - [settings.json]: defaults, context limit, handoff model, and nu profiles.

    Setting both [apiKey] and [apiKeyEnv] is an error. An unset [apiKeyEnv]
    warns and leaves that provider keyless so other providers and [--list] still
    work. The [nu] child gets an allowlisted environment without API keys (see
    {!Nu_sandbox}). *)

module String_map : Map.S with type key = string

type api =
  | Openai_completions
  | Openai_responses
  | Anthropic_messages
  | Google_generative_ai
  | Other of string

(** Config load failures. *)
type error =
  | Missing_home
  | File_error of string * string
  | Json_error of string * string
  | Missing_field of string * string
  | Invalid_field of string * string
  | Missing_models_file of string  (** No [models.json] in config dir. *)

(** [PATH] exposed to the [nu] child. {!constructor-Toolbox} (default) exposes
    only the symlink toolbox; {!constructor-Host} prepends the toolbox to the
    host [PATH] so build drivers ([make], [opam], [go]) can find their children.
*)
type nu_path_mode = Toolbox | Host

type nu_profile = {
  name : string;
      (** Profile key in [nuProfiles] (["none"] when empty); passed as
          [$env.OP_NU_PROFILE]. *)
  externals : string list;  (** Allowed external commands in the toolbox. *)
  module_dirs : string list;
      (** Directories whose [*.nu] files scripts may [use]. *)
  plugins : string list;
      (** Plugin names (e.g. ["polars"] -> [nu_plugin_polars]) scripts may
          [plugin use]. *)
  path_mode : nu_path_mode;
      (** [pathMode]: ["host"] -> {!constructor-Host}, else
          {!constructor-Toolbox}. *)
  env_allow : string list;
      (** Extra host env var names passed through to the [nu] child. *)
  tools : string list option;
      (** Default tool set when [--tools] is absent: [None] means all tools,
          [Some []] means tool-less chat. Validated at run start. *)
}
(** Named capability profile from [nuProfiles] in [settings.json]. *)

type settings = {
  provider : string option;
  model : string option;
  handoff_model : string option;
      (** Optional ["provider/model"] for [--handoff]; [None] uses the run's
          model. *)
  context_limit : float option;
      (** Fraction in [(0, 1\]] of [contextWindow] at which a run stops between
          turns; [None] uses {!default_context_limit}. *)
  nu_profiles : (string * nu_profile) list;
      (** Named profiles in file order. *)
  nu_default_profile : string option;
      (** Profile used when [--profile] is omitted. *)
}
(** Parsed [settings.json]; omitted fields are [None]. *)

type model = {
  id : string;
  max_tokens : int option;
  context_window : int option;
}
(** Model entry from [models.json]. *)

type provider = {
  name : string;
  base_url : string;
  api : api;
  api_key : string option;
  models : model list;
}
(** Provider entry from [models.json]. *)

type t

val string_of_api : api -> string
(** [string_of_api a] is the wire-name string for [a]. *)

val pp_error : error -> string
(** [pp_error e] formats [e] for display. *)

val json_error : error -> string * string option
(** [json_error e] returns a short error message and optional hint for JSON
    envelopes. *)

val load_config : unit -> (t, error) result
(** [load_config ()] loads [models.json] (required) and [settings.json]
    (optional) from [$HOME/.op_global/agent]. *)

val default_provider : t -> string option
(** [default_provider c] is [defaultProvider] from [settings.json]. *)

val default_model : t -> string option
(** [default_model c] is [defaultModel] from [settings.json]. *)

val handoff_model : t -> string option
(** [handoff_model c] is [handoffModel] from [settings.json]. *)

val default_context_limit : float
(** Default [contextLimit] fraction ([0.85]). *)

val context_limit : t -> float
(** [context_limit c] is the configured [contextLimit] or
    {!default_context_limit}. *)

val resolve_nu_profile : t -> name:string option -> (nu_profile, string) result
(** [resolve_nu_profile c ~name] resolves [name] (or [nuDefaultProfile]) in
    [nuProfiles]. With neither set, returns the empty chat profile
    ([tools = Some []], no externals/modules/plugins). *)

val parse_model_spec : string -> string option * string
(** [parse_model_spec spec] splits on the first ['/'] into
    [(Some provider, model)], or [(None, spec)] if no ['/'] is present. *)

val provider : t -> string -> provider option
(** [provider c name] looks up [name] in [models.json]. *)

val resolve_default_spec :
  t ->
  string ->
  ( string * string,
    [ `Missing_provider of string
    | `Unknown_provider of string
    | `Unknown_model of string * string ] )
  result
(** [resolve_default_spec c spec] validates ["provider/model"] against [c],
    requiring an explicit known provider and a listed model. *)

val providers : t -> provider list
(** [providers c] returns all providers in name order. *)

val max_tokens_for : provider -> model:string -> int option
(** [max_tokens_for p ~model] returns the model's configured [maxTokens], if
    listed and set. *)

val context_window_for : provider -> model:string -> int option
(** [context_window_for p ~model] returns the model's configured
    [contextWindow], if listed and set. [None] disables context reporting and
    capping. *)

val save_default : provider:string -> model:string -> (unit, error) result
(** [save_default ~provider ~model] updates [defaultProvider] and [defaultModel]
    in [settings.json], preserving all other fields. *)

(** Internal hooks for parser tests. *)
module For_testing : sig
  val parse_settings : Yojson.Safe.t -> (settings, error) result
  (** [parse_settings json] parses [settings.json]. *)

  val parse_provider : name:string -> Yojson.Safe.t -> (provider, error) result
  (** [parse_provider ~name json] parses one provider entry. *)

  val parse_models : Yojson.Safe.t -> (provider String_map.t, error) result
  (** [parse_models json] parses the provider map of [models.json]. *)

  val update_settings_json :
    Yojson.Safe.t option -> provider:string -> model:string -> Yojson.Safe.t
  (** [update_settings_json json ~provider ~model] updates the default keys in
      [json] (or a fresh object), preserving other fields. *)

  val make_config :
    ?settings:settings -> ?providers:provider String_map.t -> unit -> t
  (** [make_config ?settings ?providers ()] constructs a config directly. *)
end
