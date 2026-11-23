(* Configuration loading for op. See config.mli for the layout. *)

module String_map = Map.Make (String)

type api =
  | Openai_completions
  | Openai_responses
  | Anthropic_messages
  | Google_generative_ai
  | Other of string

type error =
  | Missing_home
  | File_error of string * string
  | Json_error of string * string
  | Missing_field of string * string
  | Invalid_field of string * string
  | Missing_models_file of string

type nu_path_mode = Toolbox | Host

type nu_profile = {
  name : string;
  externals : string list;
  module_dirs : string list;
  plugins : string list;
  path_mode : nu_path_mode;
  env_allow : string list;
  tools : string list option;
}

type settings = {
  provider : string option;
  model : string option;
  handoff_model : string option;
  context_limit : float option;
  nu_profiles : (string * nu_profile) list;
  nu_default_profile : string option;
}

type model = {
  id : string;
  max_tokens : int option;
  context_window : int option;
}

type provider = {
  name : string;
  base_url : string;
  api : api;
  api_key : string option;
  models : model list;
}

type t = { settings : settings; providers : provider String_map.t }

let string_of_api = function
  | Openai_completions -> "openai-completions"
  | Openai_responses -> "openai-responses"
  | Anthropic_messages -> "anthropic-messages"
  | Google_generative_ai -> "google-generative-ai"
  | Other api -> api

let api_of_string = function
  | "openai-completions" -> Openai_completions
  | "openai-responses" -> Openai_responses
  | "anthropic-messages" -> Anthropic_messages
  | "google-generative-ai" -> Google_generative_ai
  | api -> Other api

let models_json_example =
  {|{
  "providers": {
    "deepseek": {
      "baseUrl": "https://api.deepseek.com",
      "api": "openai-completions",
      "apiKey": "sk-...",
      "models": [ { "id": "deepseek-v4-flash" } ]
    }
  }
}|}

let missing_models_msg dir = Fmt.str "no models.json found in %s" dir

let missing_models_hint dir =
  Fmt.str "Create %s/models.json to define a provider, e.g.:\n\n%s" dir
    models_json_example

let pp_error = function
  | Missing_home -> "HOME environment variable not set"
  | File_error (path, msg) -> Fmt.str "%s: %s" path msg
  | Json_error (path, msg) -> Fmt.str "%s: invalid JSON: %s" path msg
  | Missing_field (ctx, field) -> Fmt.str "%s: missing field %s" ctx field
  | Invalid_field (ctx, field) -> Fmt.str "%s: invalid field %s" ctx field
  | Missing_models_file dir ->
      Fmt.str "%s\n\n%s" (missing_models_msg dir) (missing_models_hint dir)

let json_error = function
  | Missing_models_file dir ->
      (missing_models_msg dir, Some (missing_models_hint dir))
  | e -> (pp_error e, None)

let ( let* ) = Result.bind

let op_dir () =
  match Util.home_dir () with
  | Some home -> Ok (Filename.concat home ".op_global/agent")
  | None -> Error Missing_home

let read_json_file path =
  match Util.read_file path with
  | Error msg -> Error (File_error (path, msg))
  | Ok content -> (
      match Yojson.Safe.from_string content with
      | json -> Ok json
      | exception Yojson.Json_error msg -> Error (Json_error (path, msg)))

let read_json_file_opt path =
  if Sys.file_exists path then read_json_file path |> Result.map Option.some
  else Ok None

let string_field ctx field json =
  match Yojson.Safe.Util.member field json with
  | `String s -> Ok s
  | `Null -> Error (Missing_field (ctx, field))
  | _ -> Error (Invalid_field (ctx, field))

let optional_string_field field json =
  match Yojson.Safe.Util.member field json with
  | `String s -> Some s
  | _ -> None

(* An optional array-of-strings field; absent or non-array -> []. Non-string
   elements are dropped so a partially malformed list degrades to its valid
   entries rather than failing the whole load. *)
let string_list_field field json =
  match Yojson.Safe.Util.member field json with
  | `List items ->
      List.filter_map (function `String s -> Some s | _ -> None) items
  | _ -> []

(* Like [string_list_field], but absence is meaningful: [None] when the field
   is not an array at all, so a caller can tell "not set" from an explicit
   [[]]. The [tools] field needs the distinction — absent means every tool,
   [[]] means none. *)
let optional_string_list_field field json =
  match Yojson.Safe.Util.member field json with
  | `List items ->
      Some (List.filter_map (function `String s -> Some s | _ -> None) items)
  | _ -> None

(* A named profile from [nuProfiles]. Each field degrades gracefully: absent or
   malformed -> [] (or [None] for [tools], where absence means "all"). *)
let parse_nu_profile ~name json =
  {
    name;
    externals = string_list_field "externals" json;
    module_dirs = string_list_field "moduleDirs" json;
    plugins = string_list_field "plugins" json;
    path_mode =
      (match optional_string_field "pathMode" json with
      | Some "host" -> Host
      | _ -> Toolbox);
    env_allow = string_list_field "envAllow" json;
    tools = optional_string_list_field "tools" json;
  }

let parse_nu_profiles json =
  match Yojson.Safe.Util.member "nuProfiles" json with
  | `Assoc entries ->
      List.map (fun (name, v) -> (name, parse_nu_profile ~name v)) entries
  | _ -> []

(* [contextLimit] is the fraction of a model's contextWindow at which a run
   stops; only (0, 1] is meaningful, so anything else is a hard error rather
   than a silently ignored typo that would let a run overrun the window. *)
let parse_context_limit json =
  match Yojson.Safe.Util.member "contextLimit" json with
  | `Null -> Ok None
  | (`Int _ | `Float _) as v ->
      let f = Yojson.Safe.Util.to_number v in
      if f > 0. && f <= 1. then Ok (Some f)
      else Error (Invalid_field ("settings.json", "contextLimit"))
  | _ -> Error (Invalid_field ("settings.json", "contextLimit"))

(* Both defaults are optional: a settings.json that omits them (or is absent
   entirely) just means op has no default and a --model spec is required. *)
let parse_settings json =
  let* context_limit = parse_context_limit json in
  Ok
    {
      provider = optional_string_field "defaultProvider" json;
      model = optional_string_field "defaultModel" json;
      handoff_model = optional_string_field "handoffModel" json;
      context_limit;
      nu_profiles = parse_nu_profiles json;
      nu_default_profile = optional_string_field "nuDefaultProfile" json;
    }

(* A provider baseUrl carries the API key in an Authorization header on every
   request, so plaintext http would put that key on the wire. Require https,
   except for a loopback host, where there is no network to sniff and local
   mock/proxy servers (ollama, llama.cpp) routinely speak http. *)
let is_loopback uri =
  match Uri.host uri with
  | Some ("localhost" | "127.0.0.1" | "::1" | "[::1]") -> true
  | _ -> false

let validate_base_url ctx s =
  let uri = Uri.of_string s in
  match Uri.scheme uri with
  | Some "https" -> Ok s
  | Some "http" when is_loopback uri -> Ok s
  | _ ->
      Error
        (Invalid_field
           (ctx, "baseUrl (must be https, or http on localhost) — got " ^ s))

(* A provider entry from models.json. [baseUrl] is required; [api] is optional
   and defaults to [Openai_completions]. *)
let parse_provider ~name json =
  let ctx = Fmt.str "models.json provider '%s'" name in
  let* base_url =
    match Yojson.Safe.Util.member "baseUrl" json with
    | `String s -> validate_base_url ctx s
    | `Null -> Error (Missing_field (ctx, "baseUrl"))
    | _ -> Error (Invalid_field (ctx, "baseUrl"))
  in
  let api =
    match optional_string_field "api" json with
    | Some s -> api_of_string s
    | None -> Openai_completions
  in
  (* [apiKeyEnv] names a variable to read the key from instead of writing it
     into models.json, so the key need never touch disk. Safe against the nu
     child, which sees an env allowlist that admits no such name (see
     bin/main's [nu_child_env]) — this reads op's own environment.

     An unset variable warns rather than failing the load: models.json holds
     every provider, and one absent key must not take down [--list models] or a
     run against a different provider. Both fields at once is a plain mistake
     with no sensible precedence, so it is an error. *)
  let* api_key =
    match
      ( optional_string_field "apiKey" json,
        optional_string_field "apiKeyEnv" json )
    with
    | Some _, Some _ ->
        Error
          (Invalid_field (ctx, "apiKey and apiKeyEnv are both set — pick one"))
    | Some k, None -> Ok (Some k)
    | None, Some var -> (
        match Sys.getenv_opt var with
        | Some v when String.trim v <> "" -> Ok (Some (String.trim v))
        | _ ->
            Fmt.epr
              "warning: %s: apiKeyEnv names %s, which is unset or empty; \
               requests to this provider will carry no key@."
              ctx var;
            Ok None)
    | None, None -> Ok None
  in
  let* models =
    match Yojson.Safe.Util.member "models" json with
    | `Null -> Ok []
    | `List models ->
        let model_ctx = ctx ^ " models[]" in
        List.fold_left
          (fun acc m ->
            let* acc = acc in
            let* id = string_field model_ctx "id" m in
            let* max_tokens =
              match Yojson.Safe.Util.member "maxTokens" m with
              | `Int n when n > 0 -> Ok (Some n)
              | `Null -> Ok None
              | _ -> Error (Invalid_field (model_ctx, "maxTokens"))
            in
            let* context_window =
              match Yojson.Safe.Util.member "contextWindow" m with
              | `Int n when n > 0 -> Ok (Some n)
              | `Null -> Ok None
              | _ -> Error (Invalid_field (model_ctx, "contextWindow"))
            in
            Ok ({ id; max_tokens; context_window } :: acc))
          (Ok []) models
        |> Result.map List.rev
    | _ -> Error (Invalid_field (ctx, "models"))
  in
  Ok { name; base_url; api; api_key; models }

let parse_models json =
  let open Yojson.Safe.Util in
  match member "providers" json with
  | `Assoc providers ->
      List.fold_left
        (fun acc (name, v) ->
          let* acc = acc in
          let* provider = parse_provider ~name v in
          Ok (String_map.add name provider acc))
        (Ok String_map.empty) providers
  | `Null -> Error (Missing_field ("models.json", "providers"))
  | _ -> Error (Invalid_field ("models.json", "providers"))

(* A missing settings.json is not an error: it leaves op with no default,
   which is fine as long as the run supplies a --model spec. *)
let load_settings dir =
  let* opt = Filename.concat dir "settings.json" |> read_json_file_opt in
  match opt with
  | Some json -> parse_settings json
  | None ->
      Ok
        {
          provider = None;
          model = None;
          handoff_model = None;
          context_limit = None;
          nu_profiles = [];
          nu_default_profile = None;
        }

(* models.json is the sole source of providers, so its absence is fatal — but
   reported with an example schema rather than a bare file-not-found. *)
let load_models dir =
  let* opt = Filename.concat dir "models.json" |> read_json_file_opt in
  match opt with
  | Some json -> parse_models json
  | None -> Error (Missing_models_file dir)

(* models.json holds every provider's apiKey, so it is a secret — op's own gate
   agrees, listing .op_global/agent/models in [Gate.sensitive_path_re]. Nothing
   stops it being world-readable, which on a shared host is a plaintext key for
   every other account.

   Checked on the "other" bits only (0o007), not group (0o077): a umask of 002
   is common and makes every new file group-accessible, so warning on group
   access would fire on essentially every run and be tuned out within a day.
   World access is the bit that actually means "anyone with a login here can
   read your keys".

   Warn rather than chmod: silently rewriting the modes of a file the user owns
   is its own surprise, and stderr is where op's warnings already go.

   A config that is a *symlink* is deliberately not refused: pointing
   ~/.op_global/agent at a dotfiles repo is ordinary practice, and [Unix.stat]
   follows the link, so the mode checked is the real file's. Ownership is worth
   a word of its own — a models.json owned by someone else is a key store and a
   set of endpoints under their control. *)
let warn_config_permissions dir =
  let euid = Unix.geteuid () in
  List.iter
    (fun name ->
      let path = Filename.concat dir name in
      match Unix.stat path with
      | st ->
          if st.Unix.st_perm land 0o007 <> 0 then
            Fmt.epr
              "warning: %s is mode %04o — readable by any account on this \
               host, and it holds API keys. Fix with: chmod o-rwx %s@."
              path st.Unix.st_perm path;
          if st.Unix.st_uid <> euid then
            Fmt.epr
              "warning: %s is owned by uid %d, not you (uid %d) — its keys and \
               endpoints are under that account's control@."
              path st.Unix.st_uid euid
      | exception Unix.Unix_error _ -> ())
    [ "models.json"; "settings.json" ]

let load_config () =
  let* dir = op_dir () in
  let* settings = load_settings dir in
  let* providers = load_models dir in
  warn_config_permissions dir;
  Ok { settings; providers }

let default_provider config = config.settings.provider
let default_model config = config.settings.model
let handoff_model config = config.settings.handoff_model

(* The gist's recommendation (85-90%), earlier than Claude Code's 95%: leave
   enough headroom that the stopped run's final turn still fits. *)
let default_context_limit = 0.85

let context_limit config =
  Option.value config.settings.context_limit ~default:default_context_limit

(* Resolve the nu toolbox profile for a run. An explicit or configured-default
   name must exist in [nuProfiles]; no name at all resolves to the empty
   profile: no tools at all (a chat run), and should [--tools nu] add the nu
   tool, no externals, modules or plugins. The toolbox allowlist has exactly
   one source of truth, [nuProfiles], and an unconfigured run fails closed
   rather than falling back to a second list. *)
let resolve_nu_profile config ~name =
  let err_unknown n =
    Fmt.kstr
      (fun s -> Error s)
      "unknown nu profile '%s' (available: %s)" n
      (match config.settings.nu_profiles with
      | [] -> "none"
      | ps -> String.concat ", " (List.map fst ps))
  in
  let named n =
    match List.assoc_opt n config.settings.nu_profiles with
    | Some p -> Ok p
    | None -> err_unknown n
  in
  match (name, config.settings.nu_default_profile) with
  | Some n, _ | None, Some n -> named n
  | None, None ->
      Ok
        {
          name = "none";
          externals = [];
          module_dirs = [];
          plugins = [];
          path_mode = Toolbox;
          env_allow = [];
          tools = Some [];
        }

(* Replace the two default keys in place (preserving field order) and append
   any that were absent. *)
let update_settings_json json ~provider ~model =
  let replace = function
    | "defaultProvider", _ -> ("defaultProvider", `String provider)
    | "defaultModel", _ -> ("defaultModel", `String model)
    | field -> field
  in
  let ensure key value fields =
    if List.mem_assoc key fields then fields else fields @ [ (key, value) ]
  in
  let fields = match json with Some (`Assoc f) -> f | _ -> [] in
  `Assoc
    (List.map replace fields
    |> ensure "defaultProvider" (`String provider)
    |> ensure "defaultModel" (`String model))

let save_default ~provider ~model =
  let* dir = op_dir () in
  let settings_path = Filename.concat dir "settings.json" in
  let* json_opt = read_json_file_opt settings_path in
  let json = update_settings_json json_opt ~provider ~model in
  let s = Yojson.Safe.pretty_to_string json ^ "\n" in
  try
    (* 0600 to match models.json's expectation: settings.json holds no keys
       today, but it sits beside the key store and [warn_config_permissions]
       flags world access on it, so create it private rather than at the
       umask-dependent default. *)
    let ch =
      open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o600 settings_path
    in
    Fun.protect ~finally:(fun () -> close_out ch) (fun () -> output_string ch s);
    Ok ()
  with Sys_error msg -> Error (File_error (settings_path, msg))

(* Split on the first '/' only: provider names never contain '/', but model
   ids may. No '/' means the whole spec is the model under the default
   provider. *)
let parse_model_spec spec =
  match String.index_opt spec '/' with
  | Some i ->
      let provider = String.sub spec 0 i in
      let model = String.sub spec (i + 1) (String.length spec - i - 1) in
      (Some provider, model)
  | None -> (None, spec)

let provider config name = String_map.find_opt name config.providers
let providers config = String_map.bindings config.providers |> List.map snd

(* Resolve a "provider/model" spec into the pair to persist as the default. For
   a stored default the provider must be explicit (a bare model is ambiguous)
   and known to [config], and the model must be one the provider lists, so every
   later run resolves to a real model rather than 404ing at call time. *)
let resolve_default_spec config spec =
  match parse_model_spec spec with
  | None, _ -> Error (`Missing_provider spec)
  | Some name, model -> (
      match provider config name with
      | None -> Error (`Unknown_provider name)
      | Some prov ->
          if List.exists (fun (m : model) -> m.id = model) prov.models then
            Ok (name, model)
          else Error (`Unknown_model (name, model)))

(* Per-model maxTokens from models.json, if the model is listed and sets it. *)
let max_tokens_for provider ~model =
  List.find_opt (fun m -> m.id = model) provider.models
  |> Option.map (fun m -> m.max_tokens)
  |> Option.join

(* Per-model contextWindow. [None] for a model the provider does not list (a
   --model override may name one), which simply disables context reporting. *)
let context_window_for provider ~model =
  List.find_opt (fun m -> m.id = model) provider.models
  |> Option.map (fun m -> m.context_window)
  |> Option.join

module For_testing = struct
  let parse_settings = parse_settings
  let parse_provider = parse_provider
  let parse_models = parse_models
  let update_settings_json = update_settings_json

  let make_config
      ?(settings =
        {
          provider = None;
          model = None;
          handoff_model = None;
          context_limit = None;
          nu_profiles = [];
          nu_default_profile = None;
        }) ?(providers = String_map.empty) () =
    { settings; providers }
end
