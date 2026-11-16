open Test_support

let test_glob_star () =
  with_temp_dir @@ fun dir ->
  let open Out_channel in
  with_open_bin (Filename.concat dir "a.ml") (fun oc ->
      output_string oc "let a = 1");
  with_open_bin (Filename.concat dir "b.ml") (fun oc ->
      output_string oc "let b = 2");
  with_open_bin (Filename.concat dir "c.txt") (fun oc ->
      output_string oc "hello");
  let result = Op.Glob.run ~pattern:"*.ml" ~path:dir () in
  match result with
  | Ok out ->
      Alcotest.(check bool) "contains a.ml" true (contains out "a.ml");
      Alcotest.(check bool) "contains b.ml" true (contains out "b.ml");
      Alcotest.(check bool) "no c.txt" false (contains out "c.txt")
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_sorted () =
  with_temp_dir @@ fun dir ->
  let open Out_channel in
  with_open_bin (Filename.concat dir "zebra.ml") (fun oc -> output_string oc "");
  with_open_bin (Filename.concat dir "alpha.ml") (fun oc -> output_string oc "");
  with_open_bin (Filename.concat dir "mid.ml") (fun oc -> output_string oc "");
  let result = Op.Glob.run ~pattern:"*.ml" ~path:dir () in
  match result with
  | Ok out ->
      let lines =
        String.split_on_char '\n' out |> List.filter (fun s -> s <> "")
      in
      Alcotest.(check (list string))
        "sorted"
        [ "alpha.ml"; "mid.ml"; "zebra.ml" ]
        (List.map Filename.basename lines)
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_no_match () =
  with_temp_dir @@ fun dir ->
  let open Out_channel in
  with_open_bin (Filename.concat dir "readme.txt") (fun oc ->
      output_string oc "");
  let result = Op.Glob.run ~pattern:"*.ml" ~path:dir () in
  match result with
  | Ok out -> Alcotest.(check string) "empty" "" out
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_bad_dir () =
  let result = Op.Glob.run ~pattern:"*.ml" ~path:"/no/such/directory" () in
  match result with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for nonexistent directory"

let test_glob_invalid_pattern () =
  (* A pattern with an unbalanced brace should parse-error *)
  let result = Op.Glob.run ~pattern:"{unclosed" () in
  match result with
  | Error e ->
      Alcotest.(check bool)
        "mentions invalid" true
        (contains (Op.Glob.pp_error e) "invalid")
  | Ok _ -> Alcotest.fail "expected error for invalid pattern"

let test_glob_double_star () =
  (* ** should match across directories *)
  with_temp_dir @@ fun dir ->
  let sub = Filename.concat dir "sub" in
  Sys.mkdir sub 0o755;
  let open Out_channel in
  with_open_bin (Filename.concat dir "top.ml") (fun oc -> output_string oc "");
  with_open_bin (Filename.concat sub "nested.ml") (fun oc ->
      output_string oc "");
  let result = Op.Glob.run ~pattern:"**/*.ml" ~path:dir () in
  match result with
  | Ok out ->
      Alcotest.(check bool) "contains top.ml" true (contains out "top.ml");
      Alcotest.(check bool) "contains nested.ml" true (contains out "nested.ml")
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_star_no_recurse () =
  (* * does NOT cross directory boundaries in pathname mode *)
  with_temp_dir @@ fun dir ->
  let sub = Filename.concat dir "sub" in
  Sys.mkdir sub 0o755;
  let open Out_channel in
  with_open_bin (Filename.concat dir "top.ml") (fun oc -> output_string oc "");
  with_open_bin (Filename.concat sub "nested.ml") (fun oc ->
      output_string oc "");
  let result = Op.Glob.run ~pattern:"*.ml" ~path:dir () in
  match result with
  | Ok out ->
      Alcotest.(check bool) "contains top.ml" true (contains out "top.ml");
      Alcotest.(check bool) "no nested.ml" false (contains out "nested.ml")
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_path_prefix () =
  (* Results should be prefixed with the path argument *)
  with_temp_dir @@ fun dir ->
  let sub = Filename.concat dir "lib" in
  Sys.mkdir sub 0o755;
  let open Out_channel in
  with_open_bin (Filename.concat sub "util.ml") (fun oc -> output_string oc "");
  let result = Op.Glob.run ~pattern:"**/*.ml" ~path:sub () in
  match result with
  | Ok out ->
      (* Result should contain the full path with the lib/ prefix *)
      Alcotest.(check bool)
        "contains lib/util.ml" true
        (contains out (Filename.concat sub "util.ml"))
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_skips_hidden () =
  (* Hidden directories should be skipped *)
  with_temp_dir @@ fun dir ->
  let hidden = Filename.concat dir ".hidden" in
  Sys.mkdir hidden 0o755;
  let open Out_channel in
  with_open_bin (Filename.concat hidden "secret.ml") (fun oc ->
      output_string oc "");
  with_open_bin (Filename.concat dir "visible.ml") (fun oc ->
      output_string oc "");
  let result = Op.Glob.run ~pattern:"**/*.ml" ~path:dir () in
  match result with
  | Ok out ->
      Alcotest.(check bool)
        "contains visible.ml" true
        (contains out "visible.ml");
      Alcotest.(check bool) "no secret.ml" false (contains out "secret.ml")
  | Error e -> Alcotest.fail (Op.Glob.pp_error e)

let test_glob_too_many_globstars () =
  (* A pattern with an absurd number of '**/' segments is refused up front so
     the variant expansion stays bounded. *)
  let pattern = String.concat "" (List.init 12 (fun _ -> "**/")) ^ "*.ml" in
  match Op.Glob.run ~pattern ~path:"." () with
  | Error e ->
      Alcotest.(check bool)
        "mentions the segment cap" true
        (contains (Op.Glob.pp_error e) "**/")
  | Ok _ -> Alcotest.fail "expected error for too many '**/' segments"

let suite =
  ( "glob",
    [
      ("glob star", `Quick, test_glob_star);
      ("glob sorted", `Quick, test_glob_sorted);
      ("glob no match", `Quick, test_glob_no_match);
      ("glob bad dir", `Quick, test_glob_bad_dir);
      ("glob invalid pattern", `Quick, test_glob_invalid_pattern);
      ("glob double star", `Quick, test_glob_double_star);
      ("glob star no recurse", `Quick, test_glob_star_no_recurse);
      ("glob path prefix", `Quick, test_glob_path_prefix);
      ("glob skips hidden", `Quick, test_glob_skips_hidden);
      ("glob too many globstars", `Quick, test_glob_too_many_globstars);
    ] )
