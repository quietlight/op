(* Tests for Op.Gate_judge: response parsing (which must never raise and always
   falls back to Deny) and [of_config] resolution. No network is involved —
   [of_config] only builds the closure; a dummy post is never invoked. *)

module Parse = Op.Gate_judge.For_testing

let tag = function
  | Op.Gate.Allow -> "allow"
  | Op.Gate.Warn _ -> "warn"
  | Op.Gate.Deny _ -> "deny"
  | Op.Gate.Fault _ -> "fault"
  | Op.Gate.Code_red _ -> "code_red"

let reason = function
  | Op.Gate.Allow -> ""
  | Op.Gate.Warn s | Op.Gate.Deny s | Op.Gate.Fault s | Op.Gate.Code_red s -> s

let test_parse_clean_json () =
  let v =
    Parse.parse_verdict {|{"verdict": "warn", "reason": "looks risky"}|}
  in
  Alcotest.(check string) "verdict" "warn" (tag v);
  Alcotest.(check string) "reason" "looks risky" (reason v)

let test_parse_each_verdict () =
  let one s =
    Fmt.kstr
      (fun v -> tag (Parse.parse_verdict v))
      {|{"verdict":"%s","reason":"r"}|} s
  in
  Alcotest.(check string) "allow" "allow" (one "allow");
  Alcotest.(check string) "deny" "deny" (one "deny");
  Alcotest.(check string) "code_red" "code_red" (one "code_red")

let test_parse_fenced_json () =
  let text =
    "Here is my ruling:\n\
     ```json\n\
     {\"verdict\": \"deny\", \"reason\": \"nope\"}\n\
     ```\n"
  in
  let v = Parse.parse_verdict text in
  Alcotest.(check string) "verdict through the fence" "deny" (tag v);
  Alcotest.(check string) "reason through the fence" "nope" (reason v)

(* Garbage and off-schema replies block, but as a fault: the judge failed to
   rule, which is not the same event as the judge ruling against the command,
   and Gate.combine counts the two separately. *)
let test_parse_garbage_faults () =
  Alcotest.(check string)
    "not json -> fault" "fault"
    (tag (Parse.parse_verdict "I cannot comply with that."))

let test_parse_unknown_verdict_faults () =
  Alcotest.(check string)
    "unknown verdict -> fault" "fault"
    (tag (Parse.parse_verdict {|{"verdict": "maybe", "reason": "r"}|}))

(* The case seen in the wild: the judge reasons the command through, decides to
   allow it, and emits an object with no verdict key at all. *)
let test_parse_missing_verdict_faults () =
  let v = Parse.parse_verdict {|{"reason": "ordinary development work"}|} in
  Alcotest.(check string) "missing verdict -> fault" "fault" (tag v);
  Alcotest.(check string)
    "and says which failure it was" "gate judge omitted the verdict field"
    (reason v)

(* --- of_config --- *)

let dummy_post ~url:_ ~headers:_ ~body:_ = Ok ""

let provider =
  {
    Op.Config.name = "anthropic";
    base_url = "https://example.test";
    api = Op.Config.Anthropic_messages;
    api_key = Some "sk-test";
    models =
      [ { Op.Config.id = "haiku"; max_tokens = None; context_window = None } ];
  }

let config_with ?gate_model () =
  let providers = Op.Config.String_map.singleton "anthropic" provider in
  let settings =
    {
      Op.Config.provider = None;
      model = None;
      gate_model;
      handoff_model = None;
      context_limit = None;
      nu_profiles = [];
      nu_default_profile = None;
    }
  in
  Op.Config.For_testing.make_config ~settings ~providers ()

let test_of_config_none_when_unset () =
  let config = config_with () in
  Alcotest.(check bool)
    "no gateModel -> None" true
    (Op.Gate_judge.of_config config ~post:dummy_post = None)

let test_of_config_some_when_resolvable () =
  let config = config_with ~gate_model:"anthropic/haiku" () in
  Alcotest.(check bool)
    "resolvable gateModel -> Some" true
    (Option.is_some (Op.Gate_judge.of_config config ~post:dummy_post))

let test_of_config_none_on_unknown_provider () =
  let config = config_with ~gate_model:"nope/haiku" () in
  Alcotest.(check bool)
    "unknown provider -> None" true
    (Op.Gate_judge.of_config config ~post:dummy_post = None)

let test_of_config_none_when_provider_implicit () =
  (* a bare model with no explicit provider cannot be resolved *)
  let config = config_with ~gate_model:"haiku" () in
  Alcotest.(check bool)
    "implicit provider -> None" true
    (Op.Gate_judge.of_config config ~post:dummy_post = None)

(* --- resolve: distinguishes unset from misconfigured so main can warn --- *)

let resolution_tag = function
  | Op.Gate_judge.Not_configured -> "not_configured"
  | Op.Gate_judge.Unusable _ -> "unusable"
  | Op.Gate_judge.Ready _ -> "ready"

let test_resolve_not_configured () =
  let config = config_with () in
  Alcotest.(check string)
    "no gateModel -> Not_configured" "not_configured"
    (resolution_tag (Op.Gate_judge.resolve config ~post:dummy_post))

let test_resolve_ready () =
  let config = config_with ~gate_model:"anthropic/haiku" () in
  Alcotest.(check string)
    "resolvable -> Ready" "ready"
    (resolution_tag (Op.Gate_judge.resolve config ~post:dummy_post))

let test_resolve_unusable_carries_reason () =
  (* every misconfiguration is Unusable with a non-empty reason, not a silent
     None that looks identical to "no gateModel set" *)
  let unusable spec =
    match
      Op.Gate_judge.resolve (config_with ~gate_model:spec ()) ~post:dummy_post
    with
    | Op.Gate_judge.Unusable reason -> reason <> ""
    | _ -> false
  in
  Alcotest.(check bool) "unknown provider" true (unusable "nope/haiku");
  Alcotest.(check bool) "implicit provider" true (unusable "haiku")

let suite =
  ( "gate_judge",
    [
      ("parse clean json", `Quick, test_parse_clean_json);
      ("parse each verdict", `Quick, test_parse_each_verdict);
      ("parse fenced json", `Quick, test_parse_fenced_json);
      ("parse garbage faults", `Quick, test_parse_garbage_faults);
      ("parse unknown verdict faults", `Quick, test_parse_unknown_verdict_faults);
      ("parse missing verdict faults", `Quick, test_parse_missing_verdict_faults);
      ("of_config none when unset", `Quick, test_of_config_none_when_unset);
      ( "of_config some when resolvable",
        `Quick,
        test_of_config_some_when_resolvable );
      ( "of_config none on unknown provider",
        `Quick,
        test_of_config_none_on_unknown_provider );
      ( "of_config none when provider implicit",
        `Quick,
        test_of_config_none_when_provider_implicit );
      ("resolve not_configured when unset", `Quick, test_resolve_not_configured);
      ("resolve ready when resolvable", `Quick, test_resolve_ready);
      ( "resolve unusable carries reason",
        `Quick,
        test_resolve_unusable_carries_reason );
    ] )
