open Test_support

let test_write_creates_dirs () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "a/b/c.txt" in
  match Op.Write.run ~path ~content:"hello" with
  | Ok () ->
      Alcotest.(check bool) "exists" true (Sys.file_exists path);
      Alcotest.(check string) "content" "hello" (read_file path)
  | Error e -> Alcotest.fail (Op.Write.pp_error e)

let test_write_overwrites () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "old";
  match Op.Write.run ~path ~content:"new" with
  | Ok () -> Alcotest.(check string) "content" "new" (read_file path)
  | Error e -> Alcotest.fail (Op.Write.pp_error e)

let suite =
  ( "write",
    [
      ("write creates dirs", `Quick, test_write_creates_dirs);
      ("write overwrites", `Quick, test_write_overwrites);
    ] )
