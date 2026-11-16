(* Tests for Op.Config *)

module Config_testing = Op.Config.For_testing

let test_parse_settings () =
  let json =
    Yojson.Safe.from_string
      {|{"defaultProvider": "deepseek", "defaultModel": "deepseek-chat", "nuProfiles": {"data": {"externals": ["skraak", "op"]}}, "defaultThinkingLevel": "off"}|}
  in
  match Config_testing.parse_settings json with
  | Ok settings ->
      Alcotest.(check (option string))
        "default provider" (Some "deepseek") settings.provider;
      Alcotest.(check (option string))
        "default model" (Some "deepseek-chat") settings.model;
      let profile =
        match settings.nu_profiles with
        | [ ("data", p) ] -> p
        | _ -> Alcotest.fail "expected exactly the 'data' profile"
      in
      Alcotest.(check (list string))
        "profile externals" [ "skraak"; "op" ] profile.externals
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

(* Omitted defaults parse as None rather than erroring, so a run with --model
   can still proceed (and create settings.json). *)
let test_parse_settings_omitted () =
  let json = Yojson.Safe.from_string {|{}|} in
  match Config_testing.parse_settings json with
  | Ok settings ->
      Alcotest.(check (option string)) "default provider" None settings.provider;
      Alcotest.(check (option string)) "default model" None settings.model;
      Alcotest.(check int) "nu profiles" 0 (List.length settings.nu_profiles)
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

(* With no --profile and no nuDefaultProfile, the run gets the empty profile:
   the toolbox allowlist has exactly one source of truth (nuProfiles), so an
   unconfigured run fails closed to a builtins-only Nushell. *)
let test_resolve_profile_unconfigured_is_empty () =
  let config = Config_testing.make_config () in
  match Op.Config.resolve_nu_profile config ~name:None with
  | Ok p ->
      Alcotest.(check (list string)) "externals" [] p.Op.Config.externals;
      Alcotest.(check (list string)) "module dirs" [] p.module_dirs;
      Alcotest.(check (list string)) "plugins" [] p.plugins
  | Error e -> Alcotest.failf "expected the empty profile, got error %S" e

let path_mode =
  Alcotest.testable
    (fun ppf -> function
      | Op.Config.Toolbox -> Fmt.string ppf "toolbox"
      | Op.Config.Host -> Fmt.string ppf "host")
    ( = )

(* Parses a "dev" profile's pathMode from the given fragment (e.g.
   {|, "pathMode": "host"|}, or "" to omit the field entirely). *)
let profile_path_mode path_mode_json =
  let json =
    Fmt.kstr Yojson.Safe.from_string
      {|{"nuProfiles": {"dev": {"externals": ["op"]%s}}}|} path_mode_json
  in
  match Config_testing.parse_settings json with
  | Ok settings -> (
      match settings.nu_profiles with
      | [ ("dev", p) ] -> p.Op.Config.path_mode
      | _ -> Alcotest.fail "expected exactly the 'dev' profile")
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_parse_path_mode_host () =
  Alcotest.check path_mode "host" Op.Config.Host
    (profile_path_mode {|, "pathMode": "host"|})

let test_parse_path_mode_absent_is_toolbox () =
  Alcotest.check path_mode "default" Op.Config.Toolbox (profile_path_mode "")

(* Any value other than the literal "host" degrades to Toolbox rather than
   erroring, matching every other profile field's graceful-degrade rule
   (config.ml:141-144) — a typo fails closed instead of failing the load. *)
let test_parse_path_mode_unrecognized_is_toolbox () =
  Alcotest.check path_mode "wrong case" Op.Config.Toolbox
    (profile_path_mode {|, "pathMode": "Host"|});
  Alcotest.check path_mode "unrelated string" Op.Config.Toolbox
    (profile_path_mode {|, "pathMode": "toolbox"|})

(* Parses a "dev" profile's envAllow from the given fragment, same shape as
   [profile_path_mode]. *)
let profile_env_allow env_allow_json =
  let json =
    Fmt.kstr Yojson.Safe.from_string
      {|{"nuProfiles": {"dev": {"externals": ["op"]%s}}}|} env_allow_json
  in
  match Config_testing.parse_settings json with
  | Ok settings -> (
      match settings.nu_profiles with
      | [ ("dev", p) ] -> p.Op.Config.env_allow
      | _ -> Alcotest.fail "expected exactly the 'dev' profile")
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_parse_env_allow () =
  Alcotest.(check (list string))
    "names"
    [ "OPAMROOT"; "CARGO_HOME" ]
    (profile_env_allow {|, "envAllow": ["OPAMROOT", "CARGO_HOME"]|})

(* Absent envAllow means the base allowlist only — the nu child never inherits
   the host environment wholesale, so the safe default is the empty extension. *)
let test_parse_env_allow_absent_is_empty () =
  Alcotest.(check (list string)) "default" [] (profile_env_allow "")

(* Parses a "dev" profile's tools from the given fragment, same shape as
   [profile_path_mode]. *)
let profile_tools tools_json =
  let json =
    Fmt.kstr Yojson.Safe.from_string
      {|{"nuProfiles": {"dev": {"externals": ["op"]%s}}}|} tools_json
  in
  match Config_testing.parse_settings json with
  | Ok settings -> (
      match settings.nu_profiles with
      | [ ("dev", p) ] -> p.Op.Config.tools
      | _ -> Alcotest.fail "expected exactly the 'dev' profile")
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

(* Unlike the other list fields, absence and [] mean different things: absent
   is "every known tool", an explicit [] is a tool-less chat profile. *)
let test_parse_profile_tools () =
  Alcotest.(check (option (list string)))
    "named subset"
    (Some [ "read"; "glob" ])
    (profile_tools {|, "tools": ["read", "glob"]|});
  Alcotest.(check (option (list string)))
    "explicit empty is a chat profile" (Some [])
    (profile_tools {|, "tools": []|});
  Alcotest.(check (option (list string)))
    "absent means all" None (profile_tools "")

(* A baseUrl carries the apiKey in an Authorization header on every request, so
   plaintext http would put the key on the wire. https is required; http is
   allowed only for a loopback host, where local model servers speak it and
   there is no network to sniff. *)
let parse_base_url url =
  let json =
    Fmt.kstr Yojson.Safe.from_string
      {|{"providers": {"p": {"baseUrl": "%s", "models": [{"id": "m"}]}}}|} url
  in
  Config_testing.parse_models json

let base_url_ok url =
  match parse_base_url url with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "expected %s to be accepted, got %s" url
        (Op.Config.pp_error e)

let base_url_rejected url =
  match parse_base_url url with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "expected %s to be rejected" url

let test_base_url_requires_https () =
  base_url_ok "https://api.example.com/v1";
  base_url_rejected "http://api.example.com/v1";
  base_url_rejected "file:///etc/passwd";
  base_url_rejected "api.example.com"

(* Loopback is the exception: ollama and llama.cpp serve plain http locally. *)
let test_base_url_allows_loopback_http () =
  base_url_ok "http://localhost:11434/v1";
  base_url_ok "http://127.0.0.1:8080/v1"

(* [apiKeyEnv] keeps the key out of models.json entirely: the file names a
   variable and op reads it from its own environment at load. *)
let parse_key_fields fields =
  let json =
    Fmt.kstr Yojson.Safe.from_string
      {|{"providers": {"p": {"baseUrl": "https://x.test", %s "models": []}}}|}
      fields
  in
  Config_testing.parse_models json

let provider_key result =
  match result with
  | Ok providers -> (
      match Op.Config.String_map.find_opt "p" providers with
      | Some (p : Op.Config.provider) -> p.api_key
      | None -> Alcotest.fail "provider 'p' missing")
  | Error e -> Alcotest.failf "expected a parse, got %s" (Op.Config.pp_error e)

let test_api_key_env_read_from_environment () =
  Unix.putenv "OP_TEST_KEY_ENV" "sk-from-env";
  Alcotest.(check (option string))
    "key read from the named variable" (Some "sk-from-env")
    (provider_key (parse_key_fields {|"apiKeyEnv": "OP_TEST_KEY_ENV",|}))

(* An unset variable warns rather than failing the load: one absent key must not
   take down --list models or a run against a different provider. *)
let test_api_key_env_unset_is_none () =
  Alcotest.(check (option string))
    "unset variable yields no key" None
    (provider_key
       (parse_key_fields {|"apiKeyEnv": "OP_TEST_KEY_DEFINITELY_UNSET",|}))

let test_api_key_literal_still_works () =
  Alcotest.(check (option string))
    "literal apiKey unchanged" (Some "sk-literal")
    (provider_key (parse_key_fields {|"apiKey": "sk-literal",|}))

(* Both fields at once has no sensible precedence, so it is a config error. *)
let test_api_key_both_fields_rejected () =
  match
    parse_key_fields {|"apiKey": "sk-x", "apiKeyEnv": "OP_TEST_KEY_ENV",|}
  with
  | Error e ->
      Alcotest.(check bool)
        "names both fields" true
        (Test_support.contains (Op.Config.pp_error e) "apiKeyEnv")
  | Ok _ -> Alcotest.fail "expected apiKey + apiKeyEnv to be rejected"

(* The missing-models-file error names the directory and shows a schema the
   user can copy. *)
let test_missing_models_file_message () =
  let msg =
    Op.Config.pp_error (Op.Config.Missing_models_file "/tmp/.op_global/agent")
  in
  let contains hay needle =
    let nl = String.length needle and hl = String.length hay in
    let rec go i =
      i + nl <= hl && (String.sub hay i nl = needle || go (i + 1))
    in
    go 0
  in
  Alcotest.(check bool) "names dir" true (contains msg "/tmp/.op_global/agent");
  Alcotest.(check bool) "shows providers schema" true (contains msg "providers");
  Alcotest.(check bool) "shows baseUrl field" true (contains msg "baseUrl")

let test_update_settings_preserves_unrelated () =
  let json =
    Some
      (Yojson.Safe.from_string
         {|{"defaultProvider": "deepseek", "defaultModel": "deepseek-chat", "gateModel": "anthropic/haiku", "lastChangelogVersion": "0.3.0", "defaultThinkingLevel": "off"}|})
  in
  let result =
    Config_testing.update_settings_json json ~provider:"openai" ~model:"gpt-4o"
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "provider" "openai"
    (member "defaultProvider" result |> to_string);
  Alcotest.(check string)
    "model" "gpt-4o"
    (member "defaultModel" result |> to_string);
  (* a key op does not know (here a retired one) survives a --set default
     round-trip untouched *)
  Alcotest.(check string)
    "gate model" "anthropic/haiku"
    (member "gateModel" result |> to_string);
  Alcotest.(check string)
    "changelog" "0.3.0"
    (member "lastChangelogVersion" result |> to_string);
  Alcotest.(check string)
    "thinking" "off"
    (member "defaultThinkingLevel" result |> to_string)

let test_update_settings_none_input () =
  let result =
    Config_testing.update_settings_json None ~provider:"anthropic"
      ~model:"claude-3"
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "provider" "anthropic"
    (member "defaultProvider" result |> to_string);
  Alcotest.(check string)
    "model" "claude-3"
    (member "defaultModel" result |> to_string);
  (* exactly two fields *)
  match result with
  | `Assoc fields -> Alcotest.(check int) "field count" 2 (List.length fields)
  | _ -> Alcotest.fail "expected Assoc"

let test_update_settings_absent_keys () =
  let json =
    Some
      (Yojson.Safe.from_string
         {|{"lastChangelogVersion": "0.3.0", "defaultThinkingLevel": "off"}|})
  in
  let result =
    Config_testing.update_settings_json json ~provider:"groq" ~model:"llama3"
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string)
    "provider" "groq"
    (member "defaultProvider" result |> to_string);
  Alcotest.(check string)
    "model" "llama3"
    (member "defaultModel" result |> to_string);
  Alcotest.(check string)
    "changelog" "0.3.0"
    (member "lastChangelogVersion" result |> to_string);
  Alcotest.(check string)
    "thinking" "off"
    (member "defaultThinkingLevel" result |> to_string)

let test_parse_provider () =
  let json =
    Yojson.Safe.from_string
      {|{
        "baseUrl": "https://api.deepseek.com/v1",
        "api": "openai-completions",
        "apiKey": "sk-test-key",
        "models": [{"id": "deepseek-chat"}, {"id": "deepseek-reasoner"}]
      }|}
  in
  match Config_testing.parse_provider ~name:"deepseek" json with
  | Ok provider ->
      Alcotest.(check string)
        "base url" "https://api.deepseek.com/v1" provider.base_url;
      Alcotest.(check string)
        "api" "openai-completions"
        (Op.Config.string_of_api provider.api);
      Alcotest.(check (option string))
        "api key" (Some "sk-test-key") provider.api_key;
      Alcotest.(check (list string))
        "models"
        [ "deepseek-chat"; "deepseek-reasoner" ]
        (List.map (fun (m : Op.Config.model) -> m.id) provider.models)
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_parse_models () =
  let json =
    Yojson.Safe.from_string
      {|{
        "providers": {
          "deepseek": {
            "baseUrl": "https://api.deepseek.com/v1",
            "api": "openai-completions",
            "apiKey": "sk-key-1",
            "models": [{"id": "deepseek-chat"}]
          },
          "moonshot": {
            "baseUrl": "https://api.moonshot.ai/v1",
            "api": "openai-completions",
            "apiKey": "sk-key-2",
            "models": [{"id": "kimi-k2.5"}, {"id": "kimi-k2.6"}]
          }
        }
      }|}
  in
  match Config_testing.parse_models json with
  | Ok providers ->
      let names = Op.Config.String_map.bindings providers |> List.map fst in
      Alcotest.(check (list string))
        "provider names" [ "deepseek"; "moonshot" ] names
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let config ?(providers = Op.Config.String_map.empty) () =
  Config_testing.make_config ~providers ()

let deepseek_provider =
  {
    Op.Config.name = "deepseek";
    base_url = "https://api.deepseek.com/v1";
    api = Op.Config.Openai_completions;
    api_key = Some "sk-test";
    models =
      [
        {
          Op.Config.id = "deepseek-chat";
          max_tokens = None;
          context_window = None;
        };
      ];
  }

let test_find_provider_found () =
  let providers = Op.Config.String_map.singleton "deepseek" deepseek_provider in
  let cfg = config ~providers () in
  match Op.Config.provider cfg "deepseek" with
  | Some p ->
      Alcotest.(check string)
        "found provider" "https://api.deepseek.com/v1" p.base_url
  | None -> Alcotest.fail "expected to find provider"

let test_providers_lists_all () =
  (* [providers] enumerates every provider, in name order. *)
  let groq =
    {
      Op.Config.name = "groq";
      base_url = "https://api.groq.com";
      api = Op.Config.Openai_completions;
      api_key = Some "sk-groq";
      models = [];
    }
  in
  let providers =
    Op.Config.String_map.empty
    |> Op.Config.String_map.add "deepseek" deepseek_provider
    |> Op.Config.String_map.add "groq" groq
  in
  let cfg = config ~providers () in
  let names = Op.Config.providers cfg |> List.map (fun p -> p.Op.Config.name) in
  Alcotest.(check (list string))
    "all providers, sorted" [ "deepseek"; "groq" ] names

let test_find_provider_not_found () =
  (* A name not present in models.json resolves to None: there are no built-in
     providers. *)
  let providers = Op.Config.String_map.singleton "deepseek" deepseek_provider in
  let cfg = config ~providers () in
  match Op.Config.provider cfg "groq" with
  | Some _ -> Alcotest.fail "expected not to find provider"
  | None -> ()

let test_provider_missing_base_url () =
  (* baseUrl is required: a provider entry without one fails. *)
  let json =
    Yojson.Safe.from_string
      {|{ "api": "openai-completions", "apiKey": "sk-x" }|}
  in
  match Config_testing.parse_provider ~name:"deepseek" json with
  | Error (Op.Config.Missing_field (_, "baseUrl")) -> ()
  | Error err -> Alcotest.fail (Op.Config.pp_error err)
  | Ok _ -> Alcotest.fail "expected error for missing baseUrl"

let test_parse_provider_default_api () =
  (* api is optional and defaults to openai-completions. *)
  let json =
    Yojson.Safe.from_string
      {|{ "baseUrl": "https://api.example.com/v1", "apiKey": "sk-x" }|}
  in
  match Config_testing.parse_provider ~name:"example" json with
  | Ok provider ->
      Alcotest.(check string)
        "default api" "openai-completions"
        (Op.Config.string_of_api provider.api)
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_parse_provider_max_tokens () =
  let json =
    Yojson.Safe.from_string
      {|{
        "baseUrl": "https://api.example.com/v1",
        "api": "openai-completions",
        "models": [{"id": "big", "maxTokens": 8192}, {"id": "small"}]
      }|}
  in
  match Config_testing.parse_provider ~name:"example" json with
  | Ok provider ->
      Alcotest.(check (option int))
        "big maxTokens" (Some 8192)
        (Op.Config.max_tokens_for provider ~model:"big");
      Alcotest.(check (option int))
        "small maxTokens" None
        (Op.Config.max_tokens_for provider ~model:"small");
      Alcotest.(check (option int))
        "unlisted model" None
        (Op.Config.max_tokens_for provider ~model:"other")
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_parse_provider_context_window () =
  let json =
    Yojson.Safe.from_string
      {|{
        "baseUrl": "https://api.example.com/v1",
        "models": [{"id": "wide", "contextWindow": 262144}, {"id": "small"}]
      }|}
  in
  match Config_testing.parse_provider ~name:"example" json with
  | Ok provider ->
      Alcotest.(check (option int))
        "wide contextWindow" (Some 262144)
        (Op.Config.context_window_for provider ~model:"wide");
      Alcotest.(check (option int))
        "omitted contextWindow" None
        (Op.Config.context_window_for provider ~model:"small");
      (* A --model override may name a model the provider does not list; that
         must disable the gauge, not crash the run. *)
      Alcotest.(check (option int))
        "unlisted model" None
        (Op.Config.context_window_for provider ~model:"other")
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_provider_invalid_context_window () =
  let json =
    Yojson.Safe.from_string
      {|{
        "baseUrl": "https://api.example.com/v1",
        "models": [{"id": "m", "contextWindow": 0}]
      }|}
  in
  match Config_testing.parse_provider ~name:"example" json with
  | Ok _ -> Alcotest.fail "expected a non-positive contextWindow to be rejected"
  | Error (Op.Config.Invalid_field (_, field)) ->
      Alcotest.(check string) "names the field" "contextWindow" field
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

let test_parse_settings_handoff () =
  let json =
    Yojson.Safe.from_string
      {|{"handoffModel": "anthropic/claude-x", "contextLimit": 0.7}|}
  in
  match Config_testing.parse_settings json with
  | Ok settings ->
      Alcotest.(check (option string))
        "handoff model" (Some "anthropic/claude-x") settings.handoff_model;
      Alcotest.(check (option (float 0.0001)))
        "context limit" (Some 0.7) settings.context_limit
  | Error err -> Alcotest.fail (Op.Config.pp_error err)

(* A fraction outside (0, 1] is a typo that would silently disable the cap (or
   stop every run at once), so it fails the load rather than being ignored. *)
let test_parse_settings_bad_context_limit () =
  let check spec =
    match Config_testing.parse_settings (Yojson.Safe.from_string spec) with
    | Ok _ -> Alcotest.fail ("expected " ^ spec ^ " to be rejected")
    | Error _ -> ()
  in
  check {|{"contextLimit": 0}|};
  check {|{"contextLimit": 1.5}|};
  check {|{"contextLimit": "most of it"}|}

let test_provider_invalid_max_tokens () =
  let json =
    Yojson.Safe.from_string
      {|{
        "baseUrl": "https://api.example.com/v1",
        "models": [{"id": "m", "maxTokens": "lots"}]
      }|}
  in
  match Config_testing.parse_provider ~name:"example" json with
  | Error (Op.Config.Invalid_field (_, "maxTokens")) -> ()
  | Error err -> Alcotest.fail (Op.Config.pp_error err)
  | Ok _ -> Alcotest.fail "expected error for non-int maxTokens"

let test_parse_model_spec () =
  let check spec expected =
    let provider, model = Op.Config.parse_model_spec spec in
    Alcotest.(check (pair (option string) string))
      spec expected (provider, model)
  in
  check "anthropic/claude-x" (Some "anthropic", "claude-x");
  check "claude-x" (None, "claude-x");
  check "zai/glm/4.6" (Some "zai", "glm/4.6")

let resolve_result =
  let pp ppf = function
    | Ok (p, m) -> Fmt.pf ppf "ok(%s,%s)" p m
    | Error (`Missing_provider s) -> Fmt.pf ppf "missing(%s)" s
    | Error (`Unknown_provider s) -> Fmt.pf ppf "unknown-provider(%s)" s
    | Error (`Unknown_model (p, m)) -> Fmt.pf ppf "unknown-model(%s,%s)" p m
  in
  Alcotest.testable pp ( = )

let test_resolve_default_spec () =
  let providers = Op.Config.String_map.singleton "deepseek" deepseek_provider in
  let cfg = config ~providers () in
  let check spec expected =
    Alcotest.check resolve_result spec expected
      (Op.Config.resolve_default_spec cfg spec)
  in
  (* A known provider with a listed model resolves to the pair. *)
  check "deepseek/deepseek-chat" (Ok ("deepseek", "deepseek-chat"));
  (* A model the provider does not list is rejected, so the saved default never
     points at a model that 404s at call time. *)
  check "deepseek/some-future-model"
    (Error (`Unknown_model ("deepseek", "some-future-model")));
  (* A bare model (no provider) is rejected: a default needs both halves. *)
  check "deepseek-chat" (Error (`Missing_provider "deepseek-chat"));
  (* An unknown provider is rejected before anything is saved. *)
  check "groq/llama" (Error (`Unknown_provider "groq"))

let suite =
  ( "config",
    [
      ("parse settings", `Quick, test_parse_settings);
      ("parse settings omitted", `Quick, test_parse_settings_omitted);
      ( "resolve profile unconfigured is empty",
        `Quick,
        test_resolve_profile_unconfigured_is_empty );
      ("parse path mode host", `Quick, test_parse_path_mode_host);
      ( "parse path mode absent is toolbox",
        `Quick,
        test_parse_path_mode_absent_is_toolbox );
      ( "parse path mode unrecognized is toolbox",
        `Quick,
        test_parse_path_mode_unrecognized_is_toolbox );
      ("parse env allow", `Quick, test_parse_env_allow);
      ( "parse env allow absent is empty",
        `Quick,
        test_parse_env_allow_absent_is_empty );
      ("parse profile tools", `Quick, test_parse_profile_tools);
      ("base url requires https", `Quick, test_base_url_requires_https);
      ("api key env read", `Quick, test_api_key_env_read_from_environment);
      ("api key env unset is none", `Quick, test_api_key_env_unset_is_none);
      ("api key literal still works", `Quick, test_api_key_literal_still_works);
      ("api key both fields rejected", `Quick, test_api_key_both_fields_rejected);
      ( "base url allows loopback http",
        `Quick,
        test_base_url_allows_loopback_http );
      ("missing models file message", `Quick, test_missing_models_file_message);
      ( "update settings preserves unrelated",
        `Quick,
        test_update_settings_preserves_unrelated );
      ("update settings none input", `Quick, test_update_settings_none_input);
      ( "update settings replaces absent keys",
        `Quick,
        test_update_settings_absent_keys );
      ("parse model spec", `Quick, test_parse_model_spec);
      ("resolve default spec", `Quick, test_resolve_default_spec);
      ("parse provider", `Quick, test_parse_provider);
      ("parse provider maxTokens", `Quick, test_parse_provider_max_tokens);
      ( "parse provider contextWindow",
        `Quick,
        test_parse_provider_context_window );
      ( "parse provider rejects bad contextWindow",
        `Quick,
        test_provider_invalid_context_window );
      ("parse settings handoff", `Quick, test_parse_settings_handoff);
      ( "parse settings rejects bad contextLimit",
        `Quick,
        test_parse_settings_bad_context_limit );
      ( "parse provider rejects bad maxTokens",
        `Quick,
        test_provider_invalid_max_tokens );
      ("parse models", `Quick, test_parse_models);
      ("parse provider missing baseUrl", `Quick, test_provider_missing_base_url);
      ("parse provider default api", `Quick, test_parse_provider_default_api);
      ("find provider found", `Quick, test_find_provider_found);
      ("find provider not found", `Quick, test_find_provider_not_found);
      ("providers lists all", `Quick, test_providers_lists_all);
    ] )
