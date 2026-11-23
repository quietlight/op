(** Configuration loading for op.

    Config layout under ~/.op_global/agent:
    - models.json : provider entries, each carrying baseUrl (required), api
      (optional, default "openai-completions"), apiKey, and models.
    - settings.json : defaultProvider / defaultModel

    models.json is the sole source of provider connection data: there are no
    built-in providers and no auth.json. The [apiKey] field in a provider entry
    is the API key used for that provider; [apiKeyEnv] is the alternative — it
    names an environment variable to read the key from, so the key need never be
    written to disk at all. Setting both is an error, since there is no sensible
    precedence. A variable that is unset warns and leaves the provider keyless
    rather than failing the load: one absent key must not take down
    [--list models] or a run against a different provider.

    The nu child cannot see either form: it gets an environment allowlist that
    admits no key names, so [apiKeyEnv] does not widen what a script can read
    (see bin/main's [nu_child_env]). *)

module String_map : Map.S with type key = string

type api =
  | Openai_completions
  | Openai_responses
  | Anthropic_messages
  | Google_generative_ai
  | Other of string

(** [error] Why a config directory failed to load. *)
type error =
  | Missing_home
  | File_error of string * string
  | Json_error of string * string
  | Missing_field of string * string
  | Invalid_field of string * string
  | Missing_models_file of string
      (** No [models.json] in the given config directory. *)

(** [nu_path_mode] What PATH the nu child (and its subprocess tree) sees.
    {!constructor-Toolbox} (the default) is the locked-down symlink dir alone.
    {!constructor-Host} prepends the toolbox to the host PATH so allowlisted
    build drivers ([make], [opam], [go]) can resolve their children ([gcc],
    [ld], [git]) — the gate's name-based allowlist on the {e script} is
    unchanged, and the subprocess tree was already conceded by allowlisting
    those drivers. *)
type nu_path_mode = Toolbox | Host

type nu_profile = {
  name : string;
      (** The profile's key in [nuProfiles] (["none"] for the empty profile),
          surfaced to the nu child as [$env.OP_NU_PROFILE]. *)
  externals : string list;
      (** The external commands the toolbox PATH may contain. *)
  module_dirs : string list;
      (** Directories whose [*.nu] files the gate lets scripts [use]. *)
  plugins : string list;
      (** Plugin names ([polars] -> [nu_plugin_polars]) op registers and the
          gate lets scripts [plugin use]. *)
  path_mode : nu_path_mode;
      (** [pathMode]: ["host"] -> {!constructor-Host}; anything else ->
          {!constructor-Toolbox}. *)
  env_allow : string list;
      (** [envAllow]: extra environment variable {i names} this profile passes
          through to the nu child, on top of the small locale/home base set. The
          child env is an allowlist, not the host environment, so a driver
          needing e.g. [OPAMROOT] names it here. *)
  tools : string list option;
      (** [tools]: the run's default tool set when [--tools] is not given.
          [None] (field absent) means every known tool; [Some []] (an explicit
          [[]] in settings.json) is a tool-less chat run. Names are validated
          against {!Tools.known} at run start, not at parse time. *)
}
(** [nu_profile] A named profile from [nuProfiles] in settings.json, selecting a
    run's capabilities: the default tool set, and what the nu tool may reach
    (externals, modules, plugins, PATH mode, child environment). *)

type settings = {
  provider : string option;
  model : string option;
  handoff_model : string option;
      (** [handoffModel]: an optional ["provider/model"] spec for [--handoff]'s
          summarising call. It writes the prompt that seeds the next thread, so
          it should be a strong model with a context window at least as large as
          the session's own. [None] falls back to the run's model. *)
  context_limit : float option;
      (** [contextLimit]: the fraction of the model's [contextWindow] at which a
          run stops with {!Agent.constructor-Context_limit_reached}. [None] uses
          {!default_context_limit}. Parsing rejects anything outside [(0, 1]].
          Checked between turns against the usage the provider last reported: a
          stopping threshold, not a cap on the size of the next request. *)
  nu_profiles : (string * nu_profile) list;
      (** [nuProfiles]: named toolbox profiles, in file order — the sole source
          of the nu toolbox allowlist. *)
  nu_default_profile : string option;
      (** [nuDefaultProfile]: the profile used when [--profile] is not given. *)
}
(** [settings] The configured defaults from [settings.json]; a field is [None]
    when the file is absent or omits it. *)

type model = {
  id : string;
  max_tokens : int option;
  context_window : int option;
}
(** [model] A model entry from models.json; {!field-max_tokens} is the optional
    [maxTokens] field capping the response length for that model, and
    {!field-context_window} the optional [contextWindow] — the total prompt
    budget, against which a run's context usage is reported and capped. *)

type provider = {
  name : string;
  base_url : string;
  api : api;
      (** e.g. {!constructor-Openai_completions},
          {!constructor-Anthropic_messages}. *)
  api_key : string option;  (** The API key for this provider, if any. *)
  models : model list;
}
(** [provider] A provider definition from models.json. *)

type t

val string_of_api : api -> string
(** [string_of_api a] Return the string representation of [a]. *)

val pp_error : error -> string
(** [pp_error e] Return a human-readable description of [e]. *)

val json_error : error -> string * string option
(** [json_error e] returns a short error string plus an optional hint suitable
    for JSON envelopes. *)

val load_config : unit -> (t, error) result
(** [load_config ()] Load and parse [models.json] and [settings.json] from
    [$HOME/.op_global/agent]. [models.json] is required
    ({!constructor-Missing_models_file} otherwise); a missing [settings.json]
    yields empty defaults. *)

val default_provider : t -> string option
(** [default_provider c] Return the configured default provider for [c], or
    [None] when [settings.json] is absent or omits it. *)

val default_model : t -> string option
(** [default_model c] Return the configured default model for [c], or [None]
    when [settings.json] is absent or omits it. *)

val handoff_model : t -> string option
(** [handoff_model c] Return the configured [handoffModel] spec used by
    [--handoff], or [None] to fall back to the run's model. *)

val default_context_limit : float
(** [default_context_limit] The fraction of the context window a run may use
    when [contextLimit] is unset: [0.85]. *)

val context_limit : t -> float
(** [context_limit c] The configured [contextLimit], or
    {!default_context_limit}. *)

val resolve_nu_profile : t -> name:string option -> (nu_profile, string) result
(** [resolve_nu_profile c ~name] Return the nu toolbox profile for a run: [name]
    (from [--profile]) or, failing that, [nuDefaultProfile] must exist in
    [nuProfiles]; with neither set, the empty profile: no tools (a chat run)
    and, if [--tools] adds nu, no externals, modules or plugins. The error names
    the available profiles. *)

val parse_model_spec : string -> string option * string
(** [parse_model_spec spec] Split a [--model] spec into an optional provider and
    a model. With a '/', the part before the first '/' is the provider and the
    rest is the model; otherwise the whole spec is the model and the provider is
    [None]. *)

val provider : t -> string -> provider option
(** [provider c name] Resolve a provider by name from models.json. *)

val resolve_default_spec :
  t ->
  string ->
  ( string * string,
    [ `Missing_provider of string
    | `Unknown_provider of string
    | `Unknown_model of string * string ] )
  result
(** [resolve_default_spec c spec] Validate a ["provider/model"] spec for use as
    the persisted default and return the [(provider, model)] pair to save. The
    provider must be explicit ([`Missing_provider] otherwise) and known to [c]
    ([`Unknown_provider] otherwise), and the model must be one the provider
    lists ([`Unknown_model (provider, model)] otherwise). *)

val providers : t -> provider list
(** [providers c] All provider definitions from models.json, in name order. *)

val max_tokens_for : provider -> model:string -> int option
(** [max_tokens_for p ~model] Return the configured [maxTokens] for [model], if
    the provider lists the model and the entry sets one. Adapters fall back to
    their own default otherwise. *)

val context_window_for : provider -> model:string -> int option
(** [context_window_for p ~model] Return the configured [contextWindow] for
    [model], or [None] when the provider does not list the model (a [--model]
    override may name an unlisted one) or the entry omits it. [None] disables
    context reporting and the context cap for the run. *)

val save_default : provider:string -> model:string -> (unit, error) result
(** [save_default ~provider ~model] Persist [provider]/[model] as the new
    default, writing [defaultProvider] and [defaultModel] back to
    [settings.json] while preserving all other fields already present in the
    file. *)

(** Internal hooks for parser-focused tests. Production callers should prefer
    {!val-load_config} and [find_provider]. *)
module For_testing : sig
  val parse_settings : Yojson.Safe.t -> (settings, error) result
  (** [parse_settings json] parses the contents of a [settings.json]. *)

  val parse_provider : name:string -> Yojson.Safe.t -> (provider, error) result
  (** [parse_provider ~name json] parses a single provider entry from
      [models.json]. *)

  val parse_models : Yojson.Safe.t -> (provider String_map.t, error) result
  (** [parse_models json] parses the provider map of a [models.json]. *)

  val update_settings_json :
    Yojson.Safe.t option -> provider:string -> model:string -> Yojson.Safe.t
  (** [update_settings_json json ~provider ~model] rewrites the default
      provider/model keys in [json] (or a fresh object), preserving any other
      fields. *)

  val make_config :
    ?settings:settings -> ?providers:provider String_map.t -> unit -> t
  (** [make_config ?settings ?providers ()] builds a config value directly,
      bypassing the filesystem. *)
end
