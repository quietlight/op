(* Tests for Op.Util *)

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
  (* Should break at the last newline within 12 bytes, keeping line1\nline2 *)
  Alcotest.(check bool)
    "ends with ellipsis" true
    (String.ends_with ~suffix:"..." result);
  Alcotest.(check bool) "contains line1" true (String.contains result '1')

let test_truncate_empty () =
  Alcotest.(check string)
    "empty unchanged" ""
    (Op.Util.truncate ~max_bytes:5 "")

(* A byte-offset cut lands mid-character on any multibyte text. "é" is 2 bytes,
   so max_bytes:5 over "aaaaé" cuts between them; the result must drop the
   partial character rather than emit a lone lead byte. Not every truncated
   string reaches a request body through [to_valid_utf8] (gate summaries and
   session previews do not), so the cut itself has to be clean. *)
let test_truncate_utf8_boundary () =
  let result = Op.Util.truncate ~max_bytes:5 "aaaaé!" in
  Alcotest.(check string) "drops the split character" "aaaa..." result

let test_truncate_keeps_whole_utf8 () =
  (* max_bytes:6 fits "aaaa" + both bytes of "é". *)
  let result = Op.Util.truncate ~max_bytes:6 "aaaaé!" in
  Alcotest.(check string) "keeps the whole character" "aaaaé..." result

let test_drop_partial_utf8_tail_noop () =
  Alcotest.(check string)
    "complete text unchanged" "héllo"
    (Op.Util.drop_partial_utf8_tail "héllo")

(* Content wrapped in <tag>...</tag> must not be able to close its own block:
   a context file holding a literal closing tag would otherwise have everything
   after it read as instructions rather than data. *)
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

(* True when every byte of [s] is part of a well-formed UTF-8 sequence. *)
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
  (* ASCII and well-formed multibyte input round-trip byte-for-byte. *)
  Alcotest.(check string)
    "ascii unchanged" "hello"
    (Op.Util.to_valid_utf8 "hello");
  let s = "caf\xc3\xa9 \xe2\x9c\x93 \xf0\x9f\x8c\xb2" in
  (* é, ✓, 🌲 *)
  Alcotest.(check bool) "fixture is valid" true (is_valid_utf8 s);
  Alcotest.(check string) "multibyte unchanged" s (Op.Util.to_valid_utf8 s)

let test_utf8_split_multibyte () =
  (* A multibyte char cut mid-sequence (as byte truncation does) is repaired. *)
  let bad =
    "ok\xe2\x9c"
    (* "ok" + first 2 bytes of ✓ *)
  in
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

let suite =
  ( "util",
    [
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
