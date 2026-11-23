open Test_support

let test_read_basic () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "alpha\nbeta\ngamma\n";
  match Op.Read.run ~path () with
  | Ok out ->
      Alcotest.(check bool) "line 1" true (contains out "     1\talpha");
      Alcotest.(check bool) "line 3" true (contains out "     3\tgamma")
  | Error e -> Alcotest.fail (Op.Read.pp_error e)

let test_read_offset_limit () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "l1\nl2\nl3\nl4\nl5\n";
  match Op.Read.run ~path ~offset:2 ~limit:2 () with
  | Ok out ->
      Alcotest.(check bool) "has l2" true (contains out "     2\tl2");
      Alcotest.(check bool) "has l3" true (contains out "     3\tl3");
      Alcotest.(check bool) "omits l1" false (contains out "\tl1");
      Alcotest.(check bool) "omits l4" false (contains out "\tl4");
      Alcotest.(check bool)
        "says where to continue" true
        (contains out "more lines from offset 4")
  | Error e -> Alcotest.fail (Op.Read.pp_error e)

(* The last line of the file ends the slice with no continuation note, and an
   offset past the end reports how many lines there are. *)
let test_read_tail_and_past_end () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f.txt" in
  write_file path "l1\nl2\nl3";
  (match Op.Read.run ~path ~offset:2 () with
  | Ok out ->
      Alcotest.(check bool) "unterminated last line" true (contains out "\tl3");
      Alcotest.(check bool) "no note" false (contains out "more lines")
  | Error e -> Alcotest.fail (Op.Read.pp_error e));
  match Op.Read.run ~path ~offset:9 () with
  | Ok out ->
      Alcotest.(check string)
        "past end" "(file has 3 lines; offset 9 is past end)" out
  | Error e -> Alcotest.fail (Op.Read.pp_error e)

(* A device never ends; reading one must be refused, not attempted. *)
let test_read_device_refused () =
  match Op.Read.run ~path:"/dev/zero" ~limit:1 () with
  | Error (Op.Read.Read_error msg) ->
      Alcotest.(check bool) "says why" true (contains msg "not a regular file")
  | Error e -> Alcotest.fail (Op.Read.pp_error e)
  | Ok _ -> Alcotest.fail "expected a refusal"

let test_read_missing () =
  with_temp_dir @@ fun dir ->
  match Op.Read.run ~path:(Filename.concat dir "nope") () with
  | Error (Op.Read.Not_found _) -> ()
  | Error e -> Alcotest.fail (Op.Read.pp_error e)
  | Ok _ -> Alcotest.fail "expected Not_found"

let test_read_directory () =
  with_temp_dir @@ fun dir ->
  match Op.Read.run ~path:dir () with
  | Error (Op.Read.Is_directory _) -> ()
  | Error e -> Alcotest.fail (Op.Read.pp_error e)
  | Ok _ -> Alcotest.fail "expected Is_directory"

let test_read_byte_cap () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "big.txt" in
  (* Many long lines: well over the 256 KB result budget within the default
     line window, so the read stops early and notes the remainder. *)
  let line = String.make 1000 'x' in
  let content = String.concat "\n" (List.init 1000 (fun _ -> line)) ^ "\n" in
  write_file path content;
  match Op.Read.run ~path () with
  | Ok out ->
      Alcotest.(check bool)
        "result stays within budget (plus one line + note)" true
        (String.length out <= (256 * 1024) + 4096);
      Alcotest.(check bool)
        "notes truncation" true
        (contains out "output truncated")
  | Error e -> Alcotest.fail (Op.Read.pp_error e)

let suite =
  ( "read",
    [
      ("read basic", `Quick, test_read_basic);
      ("read byte cap", `Quick, test_read_byte_cap);
      ("read offset/limit", `Quick, test_read_offset_limit);
      ("read missing", `Quick, test_read_missing);
      ("read directory", `Quick, test_read_directory);
      ("read tail and past end", `Quick, test_read_tail_and_past_end);
      ("read device refused", `Quick, test_read_device_refused);
    ] )
