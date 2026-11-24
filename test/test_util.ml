let test_truncate_short () =
  Alcotest.(check string)
    "short string unchanged" "hello"
    (Op.Util.truncate ~max_bytes:10 "hello")

let test_truncate_exact () =
  Alcotest.(check string)
    "exact fit unchanged" "12345"
    (Op.Util.truncate ~max_bytes:5 "12345")

let test_truncate_long () =
  let result = Op.Util.truncate ~max_bytes:5 "hello world" in
  Alcotest.(check bool)
    "starts with head" true
    (String.starts_with ~prefix:"hello" result);
  Alcotest.(check bool) "has ellipsis" true (String.contains result '.')

let test_truncate_breaks_at_newline () =
  let result = Op.Util.truncate ~max_bytes:12 "line1\nline2\nline3\nline4" in
  Alcotest.(check bool)
    "ends with ellipsis" true
    (String.ends_with ~suffix:"..." result);
  Alcotest.(check bool) "contains line1" true (String.contains result '1')

let test_truncate_empty () =
  Alcotest.(check string)
    "empty unchanged" ""
    (Op.Util.truncate ~max_bytes:5 "")

(* "é" is 2 bytes; max_bytes:5 splits it, so the partial char is dropped. *)
let test_truncate_utf8_boundary () =
  let result = Op.Util.truncate ~max_bytes:5 "aaaaé!" in
  Alcotest.(check string) "drops the split character" "aaaa..." result

let test_truncate_keeps_whole_utf8 () =
  let result = Op.Util.truncate ~max_bytes:6 "aaaaé!" in
  Alcotest.(check string) "keeps the whole character" "aaaaé..." result

let test_drop_partial_utf8_tail_noop () =
  Alcotest.(check string)
    "complete text unchanged" "héllo"
    (Op.Util.drop_partial_utf8_tail "héllo")

let test_escape_closing_tag () =
  Alcotest.(check string)
    "closing tag neutralised" "before &lt;/context&gt; after"
    (Op.Util.escape_closing_tag ~tag:"context" "before </context> after")

let test_escape_closing_tag_all_occurrences () =
  Alcotest.(check string)
    "every occurrence" "&lt;/skill&gt;x&lt;/skill&gt;"
    (Op.Util.escape_closing_tag ~tag:"skill" "</skill>x</skill>")

let test_escape_closing_tag_leaves_other_markup () =
  Alcotest.(check string)
    "opening tags and other tags untouched" "<context a=1></skill>"
    (Op.Util.escape_closing_tag ~tag:"context" "<context a=1></skill>")

let test_count_lines_empty () =
  Alcotest.(check int) "empty is 0 lines" 0 (Op.Util.count_lines "")

let test_count_lines_single () =
  Alcotest.(check int) "one line, no newline" 1 (Op.Util.count_lines "hello")

let test_count_lines_trailing_newline () =
  Alcotest.(check int)
    "trailing newline does not add a line" 1
    (Op.Util.count_lines "hello\n")

let test_count_lines_multi () =
  Alcotest.(check int) "three lines" 3 (Op.Util.count_lines "a\nb\nc");
  Alcotest.(check int)
    "three lines with trailing newline" 3
    (Op.Util.count_lines "a\nb\nc\n")

let test_count_lines_blank_lines () =
  Alcotest.(check int)
    "internal blank lines count" 3
    (Op.Util.count_lines "a\n\nb")

let is_valid_utf8 s =
  let n = String.length s in
  let rec loop i =
    i >= n
    ||
    let d = String.get_utf_8_uchar s i in
    Uchar.utf_decode_is_valid d && loop (i + Uchar.utf_decode_length d)
  in
  loop 0

let test_utf8_valid_unchanged () =
  Alcotest.(check string)
    "ascii unchanged" "hello"
    (Op.Util.to_valid_utf8 "hello");
  let s = "caf\xc3\xa9 \xe2\x9c\x93 \xf0\x9f\x8c\xb2" in
  Alcotest.(check bool) "fixture is valid" true (is_valid_utf8 s);
  Alcotest.(check string) "multibyte unchanged" s (Op.Util.to_valid_utf8 s)

let test_utf8_split_multibyte () =
  (* "ok" + first 2 bytes of U+2713 *)
  let bad = "ok\xe2\x9c" in
  let r = Op.Util.to_valid_utf8 bad in
  Alcotest.(check bool) "result is valid utf-8" true (is_valid_utf8 r);
  Alcotest.(check bool)
    "head preserved" true
    (String.starts_with ~prefix:"ok" r);
  Alcotest.(check bool)
    "contains replacement char" true
    (Op.Util.substring ~start:0 ~needle:"\xef\xbf\xbd" r <> None)

let test_utf8_stray_bytes () =
  let r = Op.Util.to_valid_utf8 "a\xffb\x80c" in
  Alcotest.(check bool) "result is valid utf-8" true (is_valid_utf8 r);
  Alcotest.(check bool) "keeps ascii a" true (String.contains r 'a');
  Alcotest.(check bool) "keeps ascii c" true (String.contains r 'c')

let test_utf8_idempotent () =
  let bad = "x\xe2\x9cy\xff" in
  let once = Op.Util.to_valid_utf8 bad in
  Alcotest.(check string)
    "scrubbing twice is stable" once
    (Op.Util.to_valid_utf8 once)

let test_utf8_empty () =
  Alcotest.(check string) "empty unchanged" "" (Op.Util.to_valid_utf8 "")

let test_read_file_capped () =
  Test_support.with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "f" in
  Test_support.write_file path "12345";
  Alcotest.(check (result string string))
    "fits" (Ok "12345")
    (Op.Util.read_file_capped ~max:5 path);
  (match Op.Util.read_file_capped ~max:4 path with
  | Error e ->
      Alcotest.(check bool) "over cap" true (Test_support.contains e "cap")
  | Ok _ -> Alcotest.fail "expected the cap to refuse");
  match Op.Util.read_file_capped ~max:4 "/dev/zero" with
  | Error e ->
      Alcotest.(check bool)
        "device" true
        (Test_support.contains e "not a regular file")
  | Ok _ -> Alcotest.fail "expected a device to be refused"

let suite =
  ( "util",
    [
      ("read file capped", `Quick, test_read_file_capped);
      ("utf8 valid unchanged", `Quick, test_utf8_valid_unchanged);
      ("utf8 split multibyte", `Quick, test_utf8_split_multibyte);
      ("utf8 stray bytes", `Quick, test_utf8_stray_bytes);
      ("utf8 idempotent", `Quick, test_utf8_idempotent);
      ("utf8 empty", `Quick, test_utf8_empty);
      ("truncate short", `Quick, test_truncate_short);
      ("truncate exact", `Quick, test_truncate_exact);
      ("truncate long", `Quick, test_truncate_long);
      ("truncate breaks at newline", `Quick, test_truncate_breaks_at_newline);
      ("truncate empty", `Quick, test_truncate_empty);
      ("truncate utf8 boundary", `Quick, test_truncate_utf8_boundary);
      ("truncate keeps whole utf8", `Quick, test_truncate_keeps_whole_utf8);
      ("drop partial utf8 tail noop", `Quick, test_drop_partial_utf8_tail_noop);
      ("escape closing tag", `Quick, test_escape_closing_tag);
      ( "escape closing tag all occurrences",
        `Quick,
        test_escape_closing_tag_all_occurrences );
      ( "escape closing tag leaves other markup",
        `Quick,
        test_escape_closing_tag_leaves_other_markup );
      ("count_lines empty", `Quick, test_count_lines_empty);
      ("count_lines single", `Quick, test_count_lines_single);
      ("count_lines trailing newline", `Quick, test_count_lines_trailing_newline);
      ("count_lines multi", `Quick, test_count_lines_multi);
      ("count_lines blank lines", `Quick, test_count_lines_blank_lines);
    ] )
