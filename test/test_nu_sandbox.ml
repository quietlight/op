(* Tests for Op.Nu_sandbox: the pieces of the nu runtime with a security job
   of their own — the child environment allowlist, the [which] walk that
   decides what gets linked or executed, the module scanner, and the
   per-profile plugin registry path. None of them spawns nu. *)

module S = Op.Nu_sandbox

let child_env ?(env_allow = []) host_env =
  S.child_env ~host_env ~child_path:"/tmp/toolbox" ~host_path:"/usr/bin"
    ~depth:2 ~profile_name:"data" ~externals:[ "git"; "rg" ]
    ~module_names:[ "a.nu" ] ~env_allow
  |> Array.to_list

let has env entry = List.mem entry env

let names env =
  List.filter_map
    (fun kv ->
      Option.map (fun i -> String.sub kv 0 i) (String.index_opt kv '='))
    env

(* The reason the env is an allowlist: a credential the host happens to hold
   must not be one `$env` read away from the tool result. *)
let test_child_env_drops_credentials () =
  let env =
    child_env
      [|
        "HOME=/home/u";
        "ANTHROPIC_API_KEY=sk-secret";
        "SSH_AUTH_SOCK=/run/agent";
        "PATH=/host/bin";
        "LANG=en_NZ.UTF-8";
      |]
  in
  Alcotest.(check bool) "HOME kept" true (has env "HOME=/home/u");
  Alcotest.(check bool) "LANG kept" true (has env "LANG=en_NZ.UTF-8");
  Alcotest.(check bool)
    "API key dropped" false
    (List.mem "ANTHROPIC_API_KEY" (names env));
  Alcotest.(check bool)
    "agent socket dropped" false
    (List.mem "SSH_AUTH_SOCK" (names env));
  Alcotest.(check bool) "host PATH dropped" false (has env "PATH=/host/bin");
  Alcotest.(check bool) "PATH is the toolbox" true (has env "PATH=/tmp/toolbox")

(* Names match whole, not by prefix: HOME must not admit HOMEBREW_PREFIX. *)
let test_child_env_matches_whole_names () =
  let env =
    child_env [| "HOMEBREW_PREFIX=/opt"; "TERMINFO=/x"; "malformed" |]
  in
  Alcotest.(check bool)
    "prefix sibling dropped" false
    (List.mem "HOMEBREW_PREFIX" (names env));
  Alcotest.(check bool)
    "TERMINFO dropped" false
    (List.mem "TERMINFO" (names env));
  Alcotest.(check bool) "entry without = dropped" false (has env "malformed")

let test_child_env_profile_allow () =
  let host = [| "SKRAAK_DB=/data/db"; "OTHER=1" |] in
  Alcotest.(check bool)
    "not inherited by default" false
    (has (child_env host) "SKRAAK_DB=/data/db");
  let env = child_env ~env_allow:[ "SKRAAK_DB" ] host in
  Alcotest.(check bool) "envAllow admits it" true (has env "SKRAAK_DB=/data/db");
  Alcotest.(check bool) "and nothing else" false (has env "OTHER=1")

let test_child_env_handshake () =
  let env = child_env [| "OP_DEPTH=9"; "OP_NU_PROFILE=dev" |] in
  Alcotest.(check bool) "depth is op's" true (has env "OP_DEPTH=2");
  Alcotest.(check bool) "inherited depth dropped" false (has env "OP_DEPTH=9");
  Alcotest.(check bool) "host path" true (has env "OP_HOST_PATH=/usr/bin");
  Alcotest.(check bool) "profile" true (has env "OP_NU_PROFILE=data");
  Alcotest.(check bool) "externals" true (has env "OP_NU_EXTERNALS=git,rg");
  Alcotest.(check bool) "modules" true (has env "OP_NU_MODULES=a.nu");
  let ns = names env in
  Alcotest.(check int)
    "no name appears twice"
    (List.length (List.sort_uniq String.compare ns))
    (List.length ns)

(* [which] must return something safe to symlink into the toolbox or execute
   as a plugin: a directory or a non-executable file of the right name is not
   the tool. *)
let test_which () =
  Test_support.with_temp_dir @@ fun root ->
  let dir name =
    let d = Filename.concat root name in
    Unix.mkdir d 0o700;
    d
  in
  let a = dir "a" and b = dir "b" and c = dir "c" in
  Unix.mkdir (Filename.concat a "nu") 0o700;
  Test_support.write_file (Filename.concat b "nu") "";
  Unix.chmod (Filename.concat b "nu") 0o600;
  Test_support.write_file (Filename.concat c "nu") "";
  Unix.chmod (Filename.concat c "nu") 0o700;
  let path = String.concat Op.Util.path_list_sep [ a; ""; b; c ] in
  Alcotest.(check (option string))
    "skips the directory and the non-executable file"
    (Some (Filename.concat c "nu"))
    (S.which ~path "nu");
  Alcotest.(check (option string)) "absent" None (S.which ~path "nope")

let test_scan_module_sources () =
  Test_support.with_temp_dir @@ fun root ->
  let dir name =
    let d = Filename.concat root name in
    Unix.mkdir d 0o700;
    d
  in
  let first = dir "first" and second = dir "second" in
  Test_support.write_file (Filename.concat first "a.nu") "FIRST";
  Test_support.write_file (Filename.concat second "a.nu") "SECOND";
  Test_support.write_file (Filename.concat second "b.nu") "B";
  Test_support.write_file (Filename.concat second "notes.md") "not a module";
  Test_support.write_file
    (Filename.concat second "huge.nu")
    (String.make (Op.Gate_nu.max_script_chars + 1) 'x');
  let warnings = ref [] in
  let warn m = warnings := m :: !warnings in
  let scan =
    S.scan_module_sources ~warn
      [ first; Filename.concat root "missing"; second ]
  in
  Alcotest.(check (list (pair string string)))
    "first dir wins; oversized and non-nu files left out"
    [ ("a.nu", "FIRST"); ("b.nu", "B") ]
    (scan ());
  Alcotest.(check bool)
    "the oversized module is warned about" true
    (List.exists (fun m -> Test_support.contains m "huge.nu") !warnings);
  (* Rescanned on every call: a module written mid-run is seen next time. *)
  Test_support.write_file (Filename.concat first "c.nu") "C";
  Alcotest.(check bool)
    "new module appears" true
    (List.mem_assoc "c.nu" (scan ()))

let test_registry_path () =
  let p name = S.registry_path ~agent_dir:"/agent" ~profile_name:name in
  Alcotest.(check bool) "one per profile" true (p "data" <> p "dev");
  Alcotest.(check string)
    "named for it" "/agent/nu-plugin-data.msgpackz" (p "data");
  (* A name that needs flattening stays inside the agent dir, and does not
     collide with the profile whose name is the flattened form. *)
  let hostile = p "../../etc/x" in
  Alcotest.(check string)
    "stays in the agent dir" "/agent" (Filename.dirname hostile);
  Alcotest.(check bool)
    "no path separators survive" false
    (String.contains (Filename.basename hostile) '/');
  Alcotest.(check bool)
    "data/dev is not data_dev" true
    (p "data/dev" <> p "data_dev")

let suite =
  ( "nu_sandbox",
    [
      ("child env drops credentials", `Quick, test_child_env_drops_credentials);
      ( "child env matches whole names",
        `Quick,
        test_child_env_matches_whole_names );
      ("child env honours envAllow", `Quick, test_child_env_profile_allow);
      ("child env handshake vars", `Quick, test_child_env_handshake);
      ("which wants an executable file", `Quick, test_which);
      ("module scan", `Quick, test_scan_module_sources);
      ("plugin registry is per profile", `Quick, test_registry_path);
    ] )
