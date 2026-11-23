(* Tests for Op.Gate_nu: the static AST walk (layer 1) against the committed
   real-nu-0.114 fixtures under fixtures/nu, and the full [verdict] closure
   (length cap, AST-failure Deny, and the layer-2 sensitive-path routing) driven
   by canned [ast] closures — no nu process is spawned here. *)

let externals = [ "skraak"; "op" ]

(* Load a committed AST fixture. Dune copies fixtures/ into the test sandbox, so
   the path is relative to the test's run directory. *)
let fixture name =
  Fmt.kstr Test_support.read_file "fixtures/nu/%s.json" name
  |> Yojson.Safe.from_string

let analyze ?(modules = []) ?(plugins = []) name =
  Op.Gate_nu.analyze_ast ~externals ~modules ~plugins (fixture name)

let ok ?modules ?plugins name =
  match analyze ?modules ?plugins name with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: expected Ok, got Error %S" name e

let denied ?modules ?plugins ~naming name =
  match analyze ?modules ?plugins name with
  | Ok () -> Alcotest.failf "%s: expected Error, got Ok" name
  | Error e ->
      Alcotest.(check bool)
        (Fmt.str "%s denial names %S" name naming)
        true
        (Test_support.contains e naming)

(* --- layer 1: AST walk against real fixtures --- *)

let test_clean_pipeline_ok () =
  (* skraak + op externals, from json / get builtins: all within the toolbox *)
  ok "clean"

let test_use_std_ok () = ok "use_std"
let test_external_not_in_toolbox () = denied ~naming:"curl" "external_curl"

(* The Unix text-tool reflex (grep above all) gets its nu translation in the
   deny, not the network-delegation hint — the deny is where a model actually
   reads what to do instead. *)
let test_external_grep_hint () =
  denied ~naming:"where item =~" "external_grep";
  denied ~naming:"practical-nu.md" "external_grep"

let test_path_dispatch_denied () =
  denied ~naming:"/usr/bin/whoami" "path_dispatch"

(* Path dispatch whose basename is an allowlisted external (the "run my
   just-built binary" reflex): still denied, but the message must steer to the
   bare toolbox name. *)
let test_path_dispatch_toolbox_hint () =
  denied ~naming:"call it bare (`^op`)" "path_dispatch_toolbox"

let test_http_builtin_denied () = denied ~naming:"http" "http_get"
let test_run_external_denied () = denied ~naming:"run-external" "run_external"
let test_exec_denied () = denied ~naming:"exec" "exec_cmd"
let test_source_denied () = denied ~naming:"source" "source_file"
let test_overlay_use_denied () = denied ~naming:"overlay use" "overlay_use"
let test_source_env_denied () = denied ~naming:"source-env" "source_env"

(* [use std] is allowed but a std-prefixed *file* is not, and a std/ submodule
   is fine — the boundary between the builtin stdlib and file imports. *)
let test_use_file_denied () = denied ~naming:"use" "use_file"
let test_use_std_submodule_ok () = ok "use_std_sub"

(* Closure/block nesting: the load-bearing case flatten exists to catch. A
   denied builtin or out-of-toolbox external hidden in [each { }], [do { }],
   [if { }], [def f [] { }] or a [$"(...)"] interpolation must still be denied —
   [get block] would have serialized these bodies as opaque block-ids and let
   them through. *)
let test_closure_http_denied () = denied ~naming:"http" "closure_http"

(* `job` is allowed (spawn/recv give gated concurrency; jobs die at script
   exit), but a denied builtin inside a job closure is still caught — flatten
   emits the closure body as ordinary tokens. *)
let test_job_spawn_ok () = ok "job_spawn"
let test_job_http_denied () = denied ~naming:"http" "job_http"
let test_closure_external_denied () = denied ~naming:"curl" "closure_curl"
let test_do_http_denied () = denied ~naming:"http" "do_http"
let test_if_http_denied () = denied ~naming:"http" "if_http"
let test_def_http_denied () = denied ~naming:"http" "def_http"
let test_interp_http_denied () = denied ~naming:"http" "interp_http"

(* `extern` is the one construct that runs a program outside the toolbox with
   nothing for the walk to catch: the declaration makes the call site flatten to
   a bare shape_internalcall and no shape_external token is emitted anywhere, so
   [check_external] never sees the name while nu still executes it
   (`extern id []; id -u` really does run /usr/bin/id). The declaration itself
   is therefore denied, in both its bare and `export` spellings — the latter
   matters because [first_word "export extern"] is "export", not "extern". *)
let test_extern_denied () = denied ~naming:"extern" "extern_call"
let test_export_extern_denied () = denied ~naming:"extern" "export_extern"

(* The other [export] spellings flatten the same way, so each must be judged
   as the command after the prefix: [export use ./m.nu] ran an unread file,
   while [export use std] stays fine. *)
let test_export_use_file_denied () = denied ~naming:"use" "export_use_file"
let test_export_use_std_ok () = ok "export_use_std"

(* [module ./m.nu] loads an unread file (and could shadow a vetted module's
   name); an inline [module m { ... }] is ordinary walked code. *)
let test_module_file_denied () = denied ~naming:"module" "module_file"

let test_export_module_file_denied () =
  denied ~naming:"module" "export_module_file"

let test_module_inline_ok () = ok "module_inline"

(* [history] reads shell history with no path for Layer 2 to see. *)
let test_history_denied () = denied ~naming:"history" "history"

(* [--plugin-config] on either side of an allowed plugin name would load a
   registry op did not write. *)
let test_plugin_use_config_denied () =
  denied ~plugins:[ "polars" ] ~naming:"plugin use" "plugin_use_config";
  denied ~plugins:[ "polars" ] ~naming:"plugin use" "plugin_use_config_first"

(* A computed external head (`^$cmd`) flattens to garbage and is denied. *)
let test_computed_external_denied () =
  denied ~naming:"computed" "computed_external"

(* A made-up subcommand of a command group ([polars scan-parquet]) is still
   blocked, but the reason names the group and how to list its real
   subcommands instead of the generic syntax hint. Fixture from real nu 0.117
   with the polars plugin loaded. *)
let test_unknown_subcommand_hint () =
  denied ~plugins:[ "polars" ] ~naming:"`polars scan-parquet` is not a command"
    "unknown_subcommand";
  denied ~plugins:[ "polars" ]
    ~naming:"help commands | where name starts-with 'polars '"
    "unknown_subcommand"

(* Protected env vars (OP_DEPTH, OP_HOST_PATH, PATH) may be read as
   [$env.NAME] and nothing else. Setting op's handshake vars hands a nested op
   a forged value (resetting its recursion guard); setting PATH lets a copied
   binary run under an allowed external's name. Every write spelling is
   denied: record and list keys, [$env.NAME =] (quoted, member and [++=]
   forms), [hide-env], [insert]. Comparisons and pipelines off [$env.NAME] are
   reads. *)
let test_handshake_write_record_denied () =
  denied ~naming:"OP_DEPTH" "handshake_write_record"

let test_handshake_write_list_denied () =
  denied ~naming:"OP_DEPTH" "handshake_write_list"

let test_handshake_read_ok () = ok "handshake_read"

let test_env_writes_denied () =
  List.iter
    (fun (name, fixture) -> denied ~naming:name fixture)
    [
      ("OP_DEPTH", "env_depth_assign");
      ("OP_DEPTH", "env_depth_quoted_assign");
      ("OP_DEPTH", "env_hide_depth");
      ("OP_DEPTH", "env_insert_depth");
      ("PATH", "env_path_assign");
      ("PATH", "env_path_append");
      ("PATH", "env_path_member_assign");
      ("PATH", "env_path_record");
    ]

let test_env_path_reads_ok () =
  ok "env_path_read";
  ok "env_path_compare"

(* Guard against a false positive: an external with an interpolated argument
   followed by a flag (`op $"(...)" --json`) is a legitimate toolbox call, not a
   computed head — it must pass layer 1. *)
let test_op_interp_ok () = ok "op_interp"

(* open/save read/write files with no external call, so the AST walk passes them
   — layer 2 (text check) is what denies them. *)
let test_sensitive_open_passes_layer1 () = ok "sensitive_open"
let test_save_dotfile_passes_layer1 () = ok "save_dotfile"

(* --- profile modules and plugins in the walk --- *)

let profile_modules = [ "gpx.nu"; "bad.nu" ]
let test_use_module_ok () = ok ~modules:profile_modules "use_module"
let test_use_module_unknown_denied () = denied ~naming:"use" "use_module"

let test_use_module_pathform_denied () =
  denied ~modules:profile_modules ~naming:"use" "use_module_pathform"

let test_plugin_use_ok () = ok ~plugins:[ "polars" ] "plugin_use"
let test_plugin_use_unknown_denied () = denied ~naming:"plugin use" "plugin_use"

let test_plugin_add_denied () =
  denied ~plugins:[ "polars" ] ~naming:"plugin" "plugin_add"

let test_plugin_list_denied () =
  denied ~plugins:[ "polars" ] ~naming:"plugin" "plugin_list"

(* A *declared*-variable computed head (`let c = ...; ^$c`) flattens to inert
   [shape_variable], so the walk passes it — the source-text check in [verdict]
   is what denies it (below). The committed fixture documents the walk gap. *)
let test_computed_declared_passes_layer1 () = ok "computed_declared"

(* An empty toolbox denies even the otherwise-clean pipeline's externals. *)
let test_empty_toolbox_denies_external () =
  match
    Op.Gate_nu.analyze_ast ~externals:[] ~modules:[] ~plugins:[]
      (fixture "clean")
  with
  | Error e ->
      Alcotest.(check bool)
        "names skraak" true
        (Test_support.contains e "skraak")
  | Ok () -> Alcotest.fail "empty toolbox should deny the skraak external"

(* --- layer 2 + verdict wiring, with canned ast --- *)

let tag = function
  | Op.Gate.Allow -> "allow"
  | Op.Gate.Warn _ -> "warn"
  | Op.Gate.Deny _ -> "deny"
  | Op.Gate.Unparsed _ -> "unparsed"
  | Op.Gate.Code_red _ -> "code_red"

let reason = function
  | Op.Gate.Allow -> ""
  | Op.Gate.Warn s | Op.Gate.Deny s | Op.Gate.Unparsed s | Op.Gate.Code_red s ->
      s

(* An [ast] closure serving a fixture regardless of the script text, so the
   verdict path can be exercised without nu. *)
let ast_of name _script = Ok (fixture name)
let ast_fails _script = Error "parse error at 1:5"

let verdict ?(modules = []) ?(plugins = []) ~ast ~script () =
  Op.Gate_nu.verdict ~ast ~externals
    ~scan_modules:(fun () -> modules)
    ~plugins ~script

let test_verdict_clean_allows () =
  let v = verdict ~ast:(ast_of "clean") ~script:"skraak sql ..." () in
  Alcotest.(check string) "clean -> allow" "allow" (tag v)

let test_verdict_external_denies () =
  let v = verdict ~ast:(ast_of "external_curl") ~script:"curl x" () in
  Alcotest.(check string) "curl -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "names curl" true
    (Test_support.contains (reason v) "curl")

(* A script nu cannot parse is blocked as Unparsed, not Deny: it never runs,
   but a syntax slip is not a strike toward escalation. A garbage token — nu's
   flatten of anything it could not classify — is the same case. *)
let test_verdict_ast_failure_denies () =
  let garbage = verdict ~ast:(ast_of "computed_external") ~script:"x" () in
  Alcotest.(check string) "garbage token -> unparsed" "unparsed" (tag garbage);
  let v = verdict ~ast:ast_fails ~script:"bad (syntax" () in
  Alcotest.(check string) "ast failure -> unparsed" "unparsed" (tag v);
  Alcotest.(check bool)
    "carries nu's error" true
    (Test_support.contains (reason v) "parse error")

let test_verdict_too_long_denies () =
  let script = String.make 40_000 'x' in
  let v = verdict ~ast:(ast_of "clean") ~script () in
  Alcotest.(check string) "over cap -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "explains the cap" true
    (Test_support.contains (reason v) "too long")

(* Layer 2: a clean AST but a script that names a secret path is denied, and
   the deny quotes what matched so a false positive is visible as one. *)
let test_verdict_sensitive_path_denies () =
  let v =
    verdict ~ast:(ast_of "sensitive_open") ~script:"open ~/.ssh/id_ed25519" ()
  in
  Alcotest.(check string) "secret path -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "quotes the match" true
    (Test_support.contains (reason v) "`.ssh`")

let test_verdict_save_dotfile_denies () =
  let v =
    verdict ~ast:(ast_of "save_dotfile") ~script:"echo hi | save .env" ()
  in
  Alcotest.(check string) "save .env -> deny" "deny" (tag v)

(* The declared-variable computed head passes the walk, so the deny must come
   from the verdict's source-text check. *)
let test_verdict_computed_head_denies () =
  let v =
    verdict ~ast:(ast_of "clean") ~script:"let c = \"curl\"\n^$c example.com" ()
  in
  Alcotest.(check string) "computed head -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "names computed" true
    (Test_support.contains (reason v) "computed")

(* A computed head straight after a closure's brace has no whitespace before
   its caret ([do {^$c}]) and ran any program by absolute path. *)
let test_verdict_computed_head_in_closure_denies () =
  List.iter
    (fun script ->
      let v = verdict ~ast:(ast_of "clean") ~script () in
      Alcotest.(check string) script "deny" (tag v))
    [ "let c = \"/bin/x\"; do {^$c}"; "let c = \"x\"; [(^$c)]"; "[^$c]" ]

(* A regex anchor's `^(` sits after a quote, not at a command position — it
   must not trip the computed-head check. *)
let test_verdict_regex_anchor_allowed () =
  let v =
    verdict ~ast:(ast_of "clean") ~script:{|"x1" | parse --regex '^(\d)x'|} ()
  in
  Alcotest.(check string) "regex anchor -> allow" "allow" (tag v)

(* --- module vetting: the verdict reads each used module's source through the
   same ast closure and walks it with the same rules --- *)

let module_sources = [ ("gpx.nu", "GPX_SRC"); ("bad.nu", "BAD_SRC") ]

(* An [ast] closure routing module sources to canned fixtures and any other
   script (the top-level one) to [top]. *)
let ast_routing ~top ~gpx script =
  if script = "GPX_SRC" then Ok (fixture gpx)
  else if script = "BAD_SRC" then Ok (fixture "http_get")
  else Ok (fixture top)

let test_verdict_module_vet_pass () =
  let ast = ast_routing ~top:"use_module" ~gpx:"clean" in
  let v = verdict ~modules:module_sources ~ast ~script:"use gpx.nu" () in
  Alcotest.(check string) "clean module -> allow" "allow" (tag v)

(* [export use] of a profile module must be vetted exactly like [use]. *)
let test_verdict_export_use_module_vetted () =
  let ast = ast_routing ~top:"export_use_module_bad" ~gpx:"clean" in
  let v = verdict ~modules:module_sources ~ast ~script:"export use bad.nu" () in
  Alcotest.(check string) "http in exported module -> deny" "deny" (tag v)

let test_verdict_module_vet_fail () =
  let ast = ast_routing ~top:"use_module_bad" ~gpx:"clean" in
  let v = verdict ~modules:module_sources ~ast ~script:"use bad.nu" () in
  Alcotest.(check string) "http in module -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "names the module" true
    (Test_support.contains (reason v) "bad.nu");
  Alcotest.(check bool)
    "names the offender" true
    (Test_support.contains (reason v) "http")

(* bad.nu imports gpx.nu whose body is denied: the recursion must surface the
   inner module's failure. *)
let test_verdict_module_vet_recursive () =
  let ast script =
    if script = "BAD_SRC" then Ok (fixture "use_module")
    else if script = "GPX_SRC" then Ok (fixture "http_get")
    else Ok (fixture "use_module_bad")
  in
  let v = verdict ~modules:module_sources ~ast ~script:"use bad.nu" () in
  Alcotest.(check string) "recursive vet -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "names the inner module" true
    (Test_support.contains (reason v) "gpx.nu")

(* Layer 2 over module sources. The module dirs are writable by the nu tool
   itself ([save] to a non-dot path), so a sensitive read hidden in a module and
   called from an innocuous top-level script must be denied exactly as the
   same line written inline is — otherwise the verdict depends on where the
   line was written rather than on what it does. *)
let secret_module = [ ("gpx.nu", "export def peek [] { open ~/.ssh/id_rsa }") ]

let ast_module_clean script =
  if String.length script > 0 && script.[0] = 'e' then Ok (fixture "clean")
  else Ok (fixture "use_module")

let test_verdict_sensitive_in_module_denies () =
  let v =
    verdict ~modules:secret_module ~ast:ast_module_clean
      ~script:"use gpx.nu *; peek" ()
  in
  Alcotest.(check string) "module secret -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "names the module" true
    (Test_support.contains (reason v) "gpx.nu")

(* The anti-runaway cap applies to module sources too: module dirs are
   run-writable, so an oversized module would otherwise feed `nu ast` past the
   bound the top-level script obeys. The module is
   rejected on size before its own AST is needed. *)
let test_verdict_module_too_long_denies () =
  let big = String.make (Op.Gate_nu.max_script_chars + 1) 'x' in
  let modules = [ ("gpx.nu", big) ] in
  let v = verdict ~modules ~ast:(ast_of "use_module") ~script:"use gpx.nu" () in
  Alcotest.(check string) "oversized module -> deny" "deny" (tag v);
  Alcotest.(check bool)
    "names the module" true
    (Test_support.contains (reason v) "gpx.nu");
  Alcotest.(check bool)
    "explains the cap" true
    (Test_support.contains (reason v) "too long")

let test_verdict_module_appears_between_calls () =
  let modules = ref [] in
  let v =
    Op.Gate_nu.verdict
      ~ast:(ast_routing ~top:"use_module" ~gpx:"clean")
      ~externals
      ~scan_modules:(fun () -> !modules)
      ~plugins:[]
  in
  let first = v ~script:"use gpx.nu" in
  modules := module_sources;
  let second = v ~script:"use gpx.nu" in
  Alcotest.(check string) "missing module denied" "deny" (tag first);
  Alcotest.(check string) "new module allowed" "allow" (tag second)

let test_verdict_module_mutates_between_calls () =
  let source = ref "GPX_SRC" in
  let ast script =
    if script = "GPX_SRC" then Ok (fixture "clean")
    else if script = "BAD_SRC" then Ok (fixture "http_get")
    else Ok (fixture "use_module")
  in
  let v =
    Op.Gate_nu.verdict ~ast ~externals
      ~scan_modules:(fun () -> [ ("gpx.nu", !source) ])
      ~plugins:[]
  in
  let first = v ~script:"use gpx.nu" in
  source := "BAD_SRC";
  let second = v ~script:"use gpx.nu" in
  Alcotest.(check string) "clean module allowed" "allow" (tag first);
  Alcotest.(check string) "mutated module denied" "deny" (tag second);
  Alcotest.(check bool)
    "mutation names http" true
    (Test_support.contains (reason second) "http")

(* Within one call the cache remains the recursion guard. A imports B and B
   imports A; each source is analyzed once and the call terminates. *)
let test_verdict_module_cycle_guard () =
  let a_calls = ref 0 and b_calls = ref 0 in
  let ast script =
    if script = "A_SRC" then (
      incr a_calls;
      Ok (fixture "use_module_bad"))
    else if script = "B_SRC" then (
      incr b_calls;
      Ok (fixture "use_module"))
    else Ok (fixture "use_module")
  in
  let modules = [ ("gpx.nu", "A_SRC"); ("bad.nu", "B_SRC") ] in
  let v = verdict ~modules ~ast ~script:"use gpx.nu" () in
  Alcotest.(check string) "cycle allows" "allow" (tag v);
  Alcotest.(check int) "A analyzed once" 1 !a_calls;
  Alcotest.(check int) "B analyzed once" 1 !b_calls

let suite =
  ( "gate_nu",
    [
      ("clean pipeline ok", `Quick, test_clean_pipeline_ok);
      ("use std ok", `Quick, test_use_std_ok);
      ("external not in toolbox", `Quick, test_external_not_in_toolbox);
      ("external grep gets nu hint", `Quick, test_external_grep_hint);
      ("path dispatch denied", `Quick, test_path_dispatch_denied);
      ( "path dispatch to toolbox basename hints bare name",
        `Quick,
        test_path_dispatch_toolbox_hint );
      ("http builtin denied", `Quick, test_http_builtin_denied);
      ("run-external denied", `Quick, test_run_external_denied);
      ("exec denied", `Quick, test_exec_denied);
      ("source denied", `Quick, test_source_denied);
      ("overlay use denied", `Quick, test_overlay_use_denied);
      ("source-env denied", `Quick, test_source_env_denied);
      ("use of a file denied", `Quick, test_use_file_denied);
      ("use std submodule ok", `Quick, test_use_std_submodule_ok);
      ("closure http denied", `Quick, test_closure_http_denied);
      ("job spawn ok", `Quick, test_job_spawn_ok);
      ("job closure http denied", `Quick, test_job_http_denied);
      ("closure external denied", `Quick, test_closure_external_denied);
      ("do http denied", `Quick, test_do_http_denied);
      ("if http denied", `Quick, test_if_http_denied);
      ("def http denied", `Quick, test_def_http_denied);
      ("interp http denied", `Quick, test_interp_http_denied);
      ("extern denied", `Quick, test_extern_denied);
      ("export extern denied", `Quick, test_export_extern_denied);
      ("export use file denied", `Quick, test_export_use_file_denied);
      ("export use std ok", `Quick, test_export_use_std_ok);
      ("module file denied", `Quick, test_module_file_denied);
      ("export module file denied", `Quick, test_export_module_file_denied);
      ("module inline ok", `Quick, test_module_inline_ok);
      ("history denied", `Quick, test_history_denied);
      ("plugin use config denied", `Quick, test_plugin_use_config_denied);
      ("computed external denied", `Quick, test_computed_external_denied);
      ("unknown subcommand hint", `Quick, test_unknown_subcommand_hint);
      ( "handshake write record denied",
        `Quick,
        test_handshake_write_record_denied );
      ("handshake write list denied", `Quick, test_handshake_write_list_denied);
      ("handshake read ok", `Quick, test_handshake_read_ok);
      ("env writes denied", `Quick, test_env_writes_denied);
      ("env path reads ok", `Quick, test_env_path_reads_ok);
      ("op interp arg ok", `Quick, test_op_interp_ok);
      ( "sensitive open passes layer 1",
        `Quick,
        test_sensitive_open_passes_layer1 );
      ("save dotfile passes layer 1", `Quick, test_save_dotfile_passes_layer1);
      ("use module ok", `Quick, test_use_module_ok);
      ("use module unknown denied", `Quick, test_use_module_unknown_denied);
      ("use module pathform denied", `Quick, test_use_module_pathform_denied);
      ("plugin use ok", `Quick, test_plugin_use_ok);
      ("plugin use unknown denied", `Quick, test_plugin_use_unknown_denied);
      ("plugin add denied", `Quick, test_plugin_add_denied);
      ("plugin list denied", `Quick, test_plugin_list_denied);
      ( "computed declared passes layer 1",
        `Quick,
        test_computed_declared_passes_layer1 );
      ( "empty toolbox denies external",
        `Quick,
        test_empty_toolbox_denies_external );
      ("verdict clean allows", `Quick, test_verdict_clean_allows);
      ("verdict external denies", `Quick, test_verdict_external_denies);
      ( "verdict ast failure is unparsed",
        `Quick,
        test_verdict_ast_failure_denies );
      ("verdict too long denies", `Quick, test_verdict_too_long_denies);
      ( "verdict sensitive path denies",
        `Quick,
        test_verdict_sensitive_path_denies );
      ("verdict save dotfile denies", `Quick, test_verdict_save_dotfile_denies);
      ("verdict computed head denies", `Quick, test_verdict_computed_head_denies);
      ("verdict regex anchor allowed", `Quick, test_verdict_regex_anchor_allowed);
      ( "verdict computed head in closure denies",
        `Quick,
        test_verdict_computed_head_in_closure_denies );
      ( "verdict export use module vetted",
        `Quick,
        test_verdict_export_use_module_vetted );
      ("verdict module vet pass", `Quick, test_verdict_module_vet_pass);
      ("verdict module vet fail", `Quick, test_verdict_module_vet_fail);
      ("verdict module vet recursive", `Quick, test_verdict_module_vet_recursive);
      ( "verdict module too long denies",
        `Quick,
        test_verdict_module_too_long_denies );
      ( "verdict sensitive in module denies",
        `Quick,
        test_verdict_sensitive_in_module_denies );
      ( "verdict module appears between calls",
        `Quick,
        test_verdict_module_appears_between_calls );
      ( "verdict module mutates between calls",
        `Quick,
        test_verdict_module_mutates_between_calls );
      ("verdict module cycle guard", `Quick, test_verdict_module_cycle_guard);
    ] )
