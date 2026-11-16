open Test_support

let test_edit_unique () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "foo bar baz";
  match Op.Edit.run ~path ~old_string:"bar" ~new_string:"QUX" () with
  | Ok 1 -> Alcotest.(check string) "content" "foo QUX baz" (read_file path)
  | Ok n -> Alcotest.failf "expected 1 replacement, got %d" n
  | Error e -> Alcotest.fail (Op.Edit.pp_error e)

let test_edit_replace_all () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "a a a";
  match
    Op.Edit.run ~path ~old_string:"a" ~new_string:"b" ~replace_all:true ()
  with
  | Ok 3 -> Alcotest.(check string) "content" "b b b" (read_file path)
  | Ok n -> Alcotest.failf "expected 3 replacements, got %d" n
  | Error e -> Alcotest.fail (Op.Edit.pp_error e)

let test_edit_not_found () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "hello";
  match Op.Edit.run ~path ~old_string:"zzz" ~new_string:"x" () with
  | Error Op.Edit.Substring_not_found -> ()
  | Error e -> Alcotest.fail (Op.Edit.pp_error e)
  | Ok _ -> Alcotest.fail "expected Substring_not_found"

let test_edit_not_unique () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "a a";
  match Op.Edit.run ~path ~old_string:"a" ~new_string:"b" () with
  | Error (Op.Edit.Not_unique 2) -> ()
  | Error e -> Alcotest.fail (Op.Edit.pp_error e)
  | Ok _ -> Alcotest.fail "expected Not_unique"

let test_edit_no_change () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "xax";
  match Op.Edit.run ~path ~old_string:"a" ~new_string:"a" () with
  | Error Op.Edit.No_change -> ()
  | Error e -> Alcotest.fail (Op.Edit.pp_error e)
  | Ok _ -> Alcotest.fail "expected No_change"

let suite =
  ( "edit",
    [
      ("edit unique", `Quick, test_edit_unique);
      ("edit replace_all", `Quick, test_edit_replace_all);
      ("edit not found", `Quick, test_edit_not_found);
      ("edit not unique", `Quick, test_edit_not_unique);
      ("edit no change", `Quick, test_edit_no_change);
    ] )
