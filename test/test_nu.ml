open Test_support

(* Resolve nu the same way main's toolbox walk will: first hit on PATH. The nu
   tool is the whole point of this build, so — like the bash tests assume bash —
   these assume nu 0.114 is installed. *)
let nu_exe =
  let path = try Sys.getenv "PATH" with Not_found -> "" in
  String.split_on_char Op.Util.path_list_sep.[0] path
  |> List.find_map (fun dir ->
      let candidate = Filename.concat dir "nu" in
      if Sys.file_exists candidate then Some candidate else None)

let env = Unix.environment ()

let run ?timeout ?plugins script =
  match nu_exe with
  | None -> Alcotest.fail "nu not found on PATH"
  | Some executable ->
      Eio_main.run @@ fun eenv ->
      Op.Nu.run
        ~proc_mgr:(Eio.Stdenv.process_mgr eenv)
        ~clock:(Eio.Stdenv.clock eenv) ?timeout ?plugins ~env ~executable
        ~script ()

let test_nu_print () =
  match run {|print "hello"|} with
  | Ok (out, 0) -> Alcotest.(check bool) "output" true (contains out "hello")
  | Ok (_, n) -> Alcotest.failf "expected exit 0, got %d" n
  | Error e -> Alcotest.fail (Op.Nu.pp_error e)

(* Output well past the cap keeps its tail and reports the true total, which
   only the streaming tail sink can know once it has dropped the head. *)
let test_nu_output_tail () =
  match
    run {|1..20000 | each {|i| $"line ($i)"} | str join "\n" | print $in|}
  with
  | Ok (out, 0) ->
      Alcotest.(check bool) "keeps the tail" true (contains out "line 20000");
      Alcotest.(check bool) "drops the head" false (contains out "line 1\n");
      Alcotest.(check bool)
        "notes the cut" true
        (contains out "output truncated, showing last 30000 of");
      Alcotest.(check bool) "bounded" true (String.length out < 30_000 + 200)
  | Ok (_, n) -> Alcotest.failf "expected exit 0, got %d" n
  | Error e -> Alcotest.fail (Op.Nu.pp_error e)

let test_nu_pipeline () =
  (* A structured pipeline: the substrate the persona actually uses. *)
  match run {|[1 2 3] | math sum|} with
  | Ok (out, 0) -> Alcotest.(check bool) "sums" true (contains out "6")
  | Ok (_, n) -> Alcotest.failf "expected exit 0, got %d" n
  | Error e -> Alcotest.fail (Op.Nu.pp_error e)

let check_borderless_table out =
  Alcotest.(check bool) "keeps header" true (contains out "alpha");
  Alcotest.(check bool) "keeps value" true (contains out "42");
  Alcotest.(check bool) "no rounded border" false (contains out "╭")

let test_nu_table_is_borderless () =
  match run {|[{alpha: 42}]|} with
  | Ok (out, 0) -> check_borderless_table out
  | Ok (_, n) -> Alcotest.failf "expected exit 0, got %d" n
  | Error e -> Alcotest.fail (Op.Nu.pp_error e)

let test_nu_plugin_config_table_is_borderless () =
  with_temp_dir (fun stub_dir ->
      write_file (Filename.concat stub_dir "env.nu") "";
      write_file (Filename.concat stub_dir "config.nu") "";
      let plugins : Op.Nu.plugins =
        {
          names = [ "test" ];
          registry = Filename.concat stub_dir "plugins.msgpackz";
          stub_dir;
          commands = [];
        }
      in
      match run ~plugins {|[{alpha: 42}]|} with
      | Ok (out, 0) -> check_borderless_table out
      | Ok (_, n) -> Alcotest.failf "expected exit 0, got %d" n
      | Error e -> Alcotest.fail (Op.Nu.pp_error e))

let test_nu_exit_code () =
  match run {|exit 3|} with
  | Ok (_, 3) -> ()
  | Ok (_, n) -> Alcotest.failf "expected exit 3, got %d" n
  | Error e -> Alcotest.fail (Op.Nu.pp_error e)

let test_nu_timeout () =
  match run ~timeout:0.2 {|sleep 5sec|} with
  | Error (Op.Nu.Timeout _) -> ()
  | Error e -> Alcotest.fail (Op.Nu.pp_error e)
  | Ok (_, n) -> Alcotest.failf "expected timeout, exited %d" n

let tool_run script =
  match nu_exe with
  | None -> Alcotest.fail "nu not found on PATH"
  | Some executable ->
      Eio_main.run @@ fun eenv ->
      let tool =
        Op.Nu.tool
          ~proc_mgr:(Eio.Stdenv.process_mgr eenv)
          ~clock:(Eio.Stdenv.clock eenv)
          ~env:(fun () -> env)
          ~executable ~externals:[ "skraak"; "op" ] ()
      in
      tool.Op.Tool.run (`Assoc [ ("script", `String script) ])

let test_nu_tool_nonzero_is_error () =
  match tool_run {|print "boom"; exit 1|} with
  | Error msg ->
      Alcotest.(check bool) "keeps output" true (contains msg "boom");
      Alcotest.(check bool) "keeps exit code" true (contains msg "exit code: 1")
  | Ok _ -> Alcotest.fail "expected a non-zero exit to be a tool error"

let test_nu_tool_zero_is_ok () =
  match tool_run {|print "hi"|} with
  | Ok { Op.Tool.content = msg; _ } ->
      Alcotest.(check bool) "keeps output" true (contains msg "hi");
      Alcotest.(check bool)
        "exit code 0 note" true
        (contains msg "exit code: 0")
  | Error e -> Alcotest.fail e

(* The description carries the allowed externals so the model learns the
   contract; a missing name there is a silent capability regression. *)
let test_nu_tool_description_lists_externals () =
  match nu_exe with
  | None -> Alcotest.fail "nu not found on PATH"
  | Some executable ->
      Eio_main.run @@ fun eenv ->
      let tool =
        Op.Nu.tool
          ~proc_mgr:(Eio.Stdenv.process_mgr eenv)
          ~clock:(Eio.Stdenv.clock eenv)
          ~env:(fun () -> env)
          ~executable ~externals:[ "skraak"; "op" ] ()
      in
      Alcotest.(check bool)
        "lists skraak" true
        (contains tool.Op.Tool.description "skraak")

(* The ast runner underpins Gate_nu: it must return real JSON for a valid
   script (so the walk has something to inspect) and an Error for a broken one. *)
let ast script =
  match nu_exe with
  | None -> Alcotest.fail "nu not found on PATH"
  | Some executable ->
      Eio_main.run @@ fun eenv ->
      Op.Nu.ast
        ~proc_mgr:(Eio.Stdenv.process_mgr eenv)
        ~clock:(Eio.Stdenv.clock eenv) ~executable script

let test_nu_ast_valid_json () =
  match ast {|curl https://example.com|} with
  | Ok json ->
      (* The AST of an external call must reach Gate_nu's walk unchanged. *)
      Alcotest.(check string)
        "walk denies the external" "deny"
        (match
           Op.Gate_nu.analyze_ast ~externals:[ "op" ] ~modules:[] ~plugins:[]
             json
         with
        | Ok () -> "allow"
        | Error _ -> "deny")
  | Error e -> Alcotest.failf "expected AST JSON, got error: %s" e

(* End-to-end through a real nu subprocess: a denied builtin hidden in a closure
   must still be denied. This is the load-bearing case the flatten switch exists
   to close — with the old `get block` runner the closure body was an opaque
   block-id and this script sailed through. *)
let test_nu_ast_denies_closure_http () =
  match ast {|[1 2] | each { http get https://example.com }|} with
  | Ok json ->
      Alcotest.(check string)
        "walk denies http nested in each { }" "deny"
        (match
           Op.Gate_nu.analyze_ast ~externals:[ "op" ] ~modules:[] ~plugins:[]
             json
         with
        | Ok () -> "allow"
        | Error _ -> "deny")
  | Error e -> Alcotest.failf "expected AST JSON, got error: %s" e

(* [pathMode] lives entirely in bin/main.ml's [child_path] construction
   (toolbox_dir, or toolbox_dir ^ ":" ^ host_path), which has no test seam of
   its own. Reproduce it here against a real nu subprocess via [Op.Nu.run] —
   the same primitive main.ml calls — so the test exercises the actual
   PATH-resolution behavior the setting exists for, not just a string. *)
let run_with_path ~path script =
  match nu_exe with
  | None -> Alcotest.fail "nu not found on PATH"
  | Some executable ->
      let env =
        Unix.environment () |> Array.to_list
        |> List.filter (fun kv -> not (String.starts_with ~prefix:"PATH=" kv))
        |> List.cons ("PATH=" ^ path)
        |> Array.of_list
      in
      Eio_main.run @@ fun eenv ->
      Op.Nu.run
        ~proc_mgr:(Eio.Stdenv.process_mgr eenv)
        ~clock:(Eio.Stdenv.clock eenv) ~env ~executable ~script ()

let test_nu_path_mode_toolbox_only_hides_host_command () =
  (* A "dev"-style probe binary that lives only on the host PATH, never in the
     toolbox dir — standing in for `gcc` being invisible to a `make` symlinked
     into an empty toolbox. *)
  with_temp_dir (fun toolbox_dir ->
      with_temp_dir (fun host_dir ->
          let probe = Filename.concat host_dir "op-test-probe" in
          write_file probe "#!/bin/sh\nprintf 'PROBE-OK'\n";
          Unix.chmod probe 0o755;
          match run_with_path ~path:toolbox_dir "^op-test-probe" with
          | Ok (out, code) ->
              Alcotest.(check bool)
                "toolbox-only PATH can't reach the probe" false
                (contains out "PROBE-OK");
              Alcotest.(check bool) "nonzero exit" true (code <> 0)
          | Error e -> Alcotest.fail (Op.Nu.pp_error e)))

let test_nu_path_mode_host_appends_host_path () =
  with_temp_dir (fun toolbox_dir ->
      with_temp_dir (fun host_dir ->
          let probe = Filename.concat host_dir "op-test-probe" in
          write_file probe "#!/bin/sh\nprintf 'PROBE-OK'\n";
          Unix.chmod probe 0o755;
          (* toolbox_dir ^ ":" ^ host_dir, exactly as bin/main.ml builds
             [child_path] for a [pathMode: host] profile. *)
          match
            run_with_path
              ~path:(toolbox_dir ^ Op.Util.path_list_sep ^ host_dir)
              "^op-test-probe"
          with
          | Ok (out, 0) ->
              Alcotest.(check bool)
                "toolbox:host PATH reaches the probe" true
                (contains out "PROBE-OK")
          | Ok (_, n) -> Alcotest.failf "expected exit 0, got %d" n
          | Error e -> Alcotest.fail (Op.Nu.pp_error e)))

(* The child env is built by replacing same-named entries, never appending a
   duplicate: execve leaves duplicate NAME= handling to the consumer (nu takes
   the last, C getenv the first), so a stray inherited OP_AST_FILE must not
   survive alongside the intended one. *)
let test_env_with_replaces_not_appends () =
  Unix.putenv "OP_AST_FILE" "/tmp/STALE";
  let env = Op.Nu.For_testing.env_with [ ("OP_AST_FILE", "/tmp/INTENDED") ] in
  let entries =
    Array.to_list env |> List.filter (String.starts_with ~prefix:"OP_AST_FILE=")
  in
  Alcotest.(check (list string))
    "exactly one entry, the intended value"
    [ "OP_AST_FILE=/tmp/INTENDED" ]
    entries

let contains_sub s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

let test_plugin_note_describes_commands () =
  let cmd command summary = { Op.Nu.command; summary } in
  let many =
    List.init 12 (fun i -> cmd (Fmt.str "big c%d" i) "a big command")
  in
  let plugins : Op.Nu.plugins =
    {
      names = [ "jev"; "big"; "bare" ];
      registry = "unused";
      stub_dir = "unused";
      commands =
        [
          ( "jev",
            [
              cmd "jev" "Ask Jev typed questions.";
              cmd "jev noul" "Probability of yes.";
              cmd "jev choice" "Pick one.";
            ] );
          ("big", cmd "big" "Lots of commands." :: many);
        ];
    }
  in
  let note = Op.Nu.For_testing.plugin_note (Some plugins) in
  let has name sub = Alcotest.(check bool) name true (contains_sub note sub) in
  has "how to load" "bare `plugin use <name>`";
  has "points at help" "`help <command>`";
  has "main description" "- jev: Ask Jev typed questions.";
  has "subcommand with summary" "\n  - `jev noul`: Probability of yes.";
  has "every small-plugin subcommand" "\n  - `jev choice`: Pick one.";
  has "large plugin counted" "- big: Lots of commands. 12 commands; `help big`";
  has "plugin with no catalog still named" "- bare:";
  Alcotest.(check string)
    "no plugins, no note" ""
    (Op.Nu.For_testing.plugin_note None)

let suite =
  ( "nu",
    [
      ( "nu plugin note describes commands",
        `Quick,
        test_plugin_note_describes_commands );
      ("nu print", `Quick, test_nu_print);
      ("nu output tail", `Quick, test_nu_output_tail);
      ( "nu env_with replaces not appends",
        `Quick,
        test_env_with_replaces_not_appends );
      ("nu ast valid json", `Quick, test_nu_ast_valid_json);
      ("nu ast denies closure http", `Quick, test_nu_ast_denies_closure_http);
      ("nu pipeline", `Quick, test_nu_pipeline);
      ("nu table is borderless", `Quick, test_nu_table_is_borderless);
      ( "nu plugin config table is borderless",
        `Quick,
        test_nu_plugin_config_table_is_borderless );
      ("nu exit code", `Quick, test_nu_exit_code);
      ("nu timeout", `Quick, test_nu_timeout);
      ("nu tool nonzero is error", `Quick, test_nu_tool_nonzero_is_error);
      ("nu tool zero is ok", `Quick, test_nu_tool_zero_is_ok);
      ( "nu tool description lists externals",
        `Quick,
        test_nu_tool_description_lists_externals );
      ( "nu path mode toolbox only hides host command",
        `Quick,
        test_nu_path_mode_toolbox_only_hides_host_command );
      ( "nu path mode host appends host path",
        `Quick,
        test_nu_path_mode_host_appends_host_path );
    ] )
