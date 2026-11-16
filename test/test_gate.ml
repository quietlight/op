(* Tests for Op.Gate: pure path rules, the shared sensitive-path pattern,
   and the sliding-window combination. Everything is exercised through [check]
   on a fresh gate (except the window tests, which reuse one gate to observe
   state across calls). *)

let cwd = "/home/u/proj"

let tc ?(args = `Assoc []) name : Op.Llm.tool_call =
  { id = "1"; name; arguments = args }

let path_tc name p = tc ~args:(`Assoc [ ("path", `String p) ]) name

let reason = function
  | Op.Gate.Allow -> ""
  | Op.Gate.Warn s | Op.Gate.Deny s | Op.Gate.Unparsed s | Op.Gate.Code_red s ->
      s

let tag = function
  | Op.Gate.Allow -> "allow"
  | Op.Gate.Warn _ -> "warn"
  | Op.Gate.Deny _ -> "deny"
  | Op.Gate.Unparsed _ -> "unparsed"
  | Op.Gate.Code_red _ -> "code_red"

let check_tag msg expected tcall =
  let gate = Op.Gate.v ~cwd () in
  Alcotest.(check string) msg expected (tag (Op.Gate.check gate tcall))

(* --- paths --- *)

let test_paths () =
  let deny name = check_tag name "deny" in
  let allow name = check_tag name "allow" in
  deny "relative dotfile" (path_tc "write" ".env");
  deny "leading ./ dotfile" (path_tc "write" "./.env");
  deny ".. resolves into dotfile" (path_tc "write" "foo/../.env");
  deny "absolute .ssh" (path_tc "read" "/abs/.ssh/key");
  deny "read is gated too" (path_tc "read" ".env");
  deny "~/.op_global outside cwd"
    (path_tc "read" "/home/u/.op_global/agent/models.json");
  allow "dot inside a component is fine" (path_tc "write" "a/b.c/d");
  deny "backslash-separated dotfile" (path_tc "write" "foo\\.env");
  deny "drive-letter absolute .ssh"
    (path_tc "read" "C:\\Users\\me\\.ssh\\id_rsa");
  deny "mixed separators .. into dotfile" (path_tc "write" "foo\\../.env");
  allow "drive letter is not a dot segment"
    (path_tc "read" "C:\\Users\\me\\a.txt");
  allow "ordinary relative file" (path_tc "write" "notes.txt");
  allow "top-level skills is an ordinary path"
    (path_tc "write" (cwd ^ "/skills/x.md"));
  deny "cwd/.op is no longer allowlisted"
    (path_tc "write" (cwd ^ "/.op/agent/skills/x.md"))

(* --- glob root gating --- *)

let test_glob_paths () =
  let glob p =
    tc ~args:(`Assoc [ ("pattern", `String "*"); ("path", `String p) ]) "glob"
  in
  check_tag "hidden root denied" "deny" (glob "/home/u/.ssh");
  check_tag "cwd/.op root denied" "deny" (glob (cwd ^ "/.op/agent"));
  check_tag "cwd/skills root allowed" "allow" (glob (cwd ^ "/skills"));
  check_tag "ordinary root allowed" "allow" (glob (cwd ^ "/src"));
  (* no path -> defaults to cwd, allowed *)
  check_tag "no path allowed" "allow"
    (tc ~args:(`Assoc [ ("pattern", `String "**/*.ml") ]) "glob")

(* --- sensitive_path_re: the secret-path pattern shared with Gate_nu --- *)

let test_sensitive_paths_match () =
  let hits name s =
    Alcotest.(check bool) name true (Re.execp Op.Gate.sensitive_path_re s)
  in
  hits "ssh key" "open ~/.ssh/id_ed25519";
  hits "aws credentials" "open ~/.aws/credentials";
  hits "dotenv" "open .env";
  hits "op's models.json" "open ~/.op_global/agent/models.json";
  hits "npm auth config" "open ~/.npmrc";
  hits "stored git credentials" "open ~/.git-credentials";
  hits "gh token store" "open ~/.config/gh/hosts.yml";
  hits "himalaya mail config" "open ~/.config/himalaya/config.toml";
  hits "typesafe api key (nu_plugin_jev)" "open ~/.config/typesafe/api_key";
  hits "zsh history" "open ~/.zsh_history";
  hits "bash history" "open ~/.bash_history";
  hits "psql history" "open ~/.psql_history"

let test_sensitive_path_near_misses () =
  let misses name s =
    Alcotest.(check bool) name false (Re.execp Op.Gate.sensitive_path_re s)
  in
  misses "gitignore is not a secret" "open .gitignore";
  misses "environment is not .env" "open environment.md";
  misses "ordinary read" "open README.md";
  misses "ghostty config is not gh" "open ~/.config/ghostty/config";
  misses "himalayan is not himalaya" "open ~/.config/himalayan/notes";
  misses "typesafety is not typesafe" "open ~/.config/typesafety/x";
  misses "history.md is not a history file" "open docs/history.md";
  misses "undotted npmrc mention" "grep npmrc README.md"

(* --- window: one-shot flagged-call reminder --- *)

let test_warn_fires_once_then_rearms () =
  let gate = Op.Gate.v ~cwd () in
  (* a denied call flags the window *)
  Alcotest.(check string)
    "flagging call" "deny"
    (tag (Op.Gate.check gate (path_tc "write" ".env")));
  (* the first allowed call carries the reminder... *)
  let allowed () = Op.Gate.check gate (path_tc "read" "notes.txt") in
  let v = allowed () in
  Alcotest.(check string) "first allowed warns" "warn" (tag v);
  Alcotest.(check bool)
    "reminder cites the flag" true
    (Test_support.contains (reason v) "reminder");
  (* ...and only the first: subsequent clean calls are plain Allow *)
  Alcotest.(check string) "second allowed is clean" "allow" (tag (allowed ()));
  Alcotest.(check string) "third allowed is clean" "allow" (tag (allowed ()));
  (* a fresh flagged call re-arms the reminder *)
  Alcotest.(check string)
    "re-flagging call" "deny"
    (tag (Op.Gate.check gate (path_tc "write" ".env")));
  Alcotest.(check string) "reminder fires again" "warn" (tag (allowed ()));
  Alcotest.(check string) "then clean again" "allow" (tag (allowed ()))

(* --- window: escalation --- *)

let test_repeated_denies_escalate () =
  let gate = Op.Gate.v ~cwd () in
  let deny () = Op.Gate.check gate (path_tc "write" ".env") in
  Alcotest.(check string) "first deny" "deny" (tag (deny ()));
  Alcotest.(check string) "second deny" "deny" (tag (deny ()));
  Alcotest.(check string)
    "third denied call escalates" "code_red"
    (tag (deny ()))

(* A script that would not parse is blocked but is not a strike. Syntax slips
   are routine (about half the runs escalation ever ended had one among their
   three), so they must neither count toward it nor raise the flagged-call
   reminder on the next clean call. *)
let test_unparsed_is_not_a_strike () =
  let next = ref Op.Gate.Allow in
  let nu ~script:_ = !next in
  let gate = Op.Gate.v ~nu ~cwd () in
  let call verdict =
    next := verdict;
    tag
      (Op.Gate.check gate (tc ~args:(`Assoc [ ("script", `String "x") ]) "nu"))
  in
  Alcotest.(check string) "deny" "deny" (call (Op.Gate.Deny "curl"));
  Alcotest.(check string) "typo" "unparsed" (call (Op.Gate.Unparsed "typo"));
  Alcotest.(check string) "typo" "unparsed" (call (Op.Gate.Unparsed "typo"));
  Alcotest.(check string)
    "a second real deny is still just a deny" "deny"
    (call (Op.Gate.Deny "curl"));
  let gate = Op.Gate.v ~nu ~cwd () in
  let call verdict =
    next := verdict;
    tag
      (Op.Gate.check gate (tc ~args:(`Assoc [ ("script", `String "x") ]) "nu"))
  in
  Alcotest.(check string) "typo" "unparsed" (call (Op.Gate.Unparsed "typo"));
  Alcotest.(check string)
    "no reminder after a typo" "allow" (call Op.Gate.Allow)

(* --- nu dispatch, over the injected [?nu] closure --- *)

let nu_tc s = tc ~args:(`Assoc [ ("script", `String s) ]) "nu"

let test_nu_no_gate_fails_closed () =
  (* No [?nu] closure -> a nu call is denied: nothing executable is allowed by
     default. *)
  let gate = Op.Gate.v ~cwd () in
  Alcotest.(check string)
    "no nu gate -> deny" "deny"
    (tag (Op.Gate.check gate (nu_tc "ls | length")))

let test_nu_gate_allows () =
  let nu ~script:_ = Op.Gate.Allow in
  let gate = Op.Gate.v ~nu ~cwd () in
  Alcotest.(check string)
    "nu gate allow" "allow"
    (tag (Op.Gate.check gate (nu_tc "ls | length")))

let test_nu_gate_denies_and_names () =
  let nu ~script:_ = Op.Gate.Deny "curl not in toolbox" in
  let gate = Op.Gate.v ~nu ~cwd () in
  let v = Op.Gate.check gate (nu_tc "curl x") in
  Alcotest.(check string) "deny" "deny" (tag v);
  Alcotest.(check bool)
    "carries the reason" true
    (Test_support.contains (reason v) "curl")

let test_nu_receives_script () =
  let seen = ref None in
  let nu ~script =
    seen := Some script;
    Op.Gate.Allow
  in
  let gate = Op.Gate.v ~nu ~cwd () in
  ignore (Op.Gate.check gate (nu_tc "skraak sql"));
  Alcotest.(check (option string)) "script forwarded" (Some "skraak sql") !seen

(* Nu verdicts depend on module files, so an identical script is re-gated. *)
let test_nu_verdict_not_cached () =
  let calls = ref 0 in
  let nu ~script:_ =
    incr calls;
    if !calls = 1 then Op.Gate.Allow else Op.Gate.Deny "module changed"
  in
  let gate = Op.Gate.v ~nu ~cwd () in
  let first = Op.Gate.check gate (nu_tc "use m.nu") in
  let second = Op.Gate.check gate (nu_tc "use m.nu") in
  Alcotest.(check string) "first allow" "allow" (tag first);
  Alcotest.(check string) "second deny" "deny" (tag second);
  Alcotest.(check int) "nu gate consulted twice" 2 !calls

let suite =
  ( "gate",
    [
      ("path rules", `Quick, test_paths);
      ("glob root gating", `Quick, test_glob_paths);
      ("sensitive paths match", `Quick, test_sensitive_paths_match);
      ("sensitive path near misses", `Quick, test_sensitive_path_near_misses);
      ("warn fires once then rearms", `Quick, test_warn_fires_once_then_rearms);
      ("repeated denies escalate", `Quick, test_repeated_denies_escalate);
      ("unparsed is not a strike", `Quick, test_unparsed_is_not_a_strike);
      ("nu without gate fails closed", `Quick, test_nu_no_gate_fails_closed);
      ("nu gate allows", `Quick, test_nu_gate_allows);
      ("nu gate denies and names", `Quick, test_nu_gate_denies_and_names);
      ("nu gate sees the script", `Quick, test_nu_receives_script);
      ("nu verdict not cached", `Quick, test_nu_verdict_not_cached);
    ] )
