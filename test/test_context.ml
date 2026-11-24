let render = Op.Context.render

let test_no_files_is_identity () =
  Alcotest.(check string)
    "prompt unchanged" "fix the bug"
    (render ~files:[] ~prompt:"fix the bug")

let test_single_file_block () =
  Alcotest.(check string)
    "one block then prompt"
    "<context file=\"CLAUDE.md\">\nBuild with make.\n</context>\n\nfix the bug"
    (render
       ~files:[ ("CLAUDE.md", "Build with make.\n") ]
       ~prompt:"fix the bug")

let test_multiple_files_preserve_order () =
  Alcotest.(check string)
    "blocks in flag order"
    "<context file=\"CLAUDE.md\">\n\
     a\n\
     </context>\n\n\
     <context file=\"docs/data.md\">\n\
     b\n\
     </context>\n\n\
     go"
    (render ~files:[ ("CLAUDE.md", "a"); ("docs/data.md", "b") ] ~prompt:"go")

let test_contents_trimmed_only_at_edges () =
  Alcotest.(check string)
    "inner whitespace kept, outer trimmed"
    "<context file=\"f\">\nline one\n\nline two\n</context>\n\ngo"
    (render ~files:[ ("f", "\n\nline one\n\nline two\n\n") ] ~prompt:"go")

let test_path_with_quote_is_escaped () =
  Alcotest.(check string)
    "quoted path stays one attribute"
    "<context file=\"a\\\"b.md\">\nx\n</context>\n\ngo"
    (render ~files:[ ("a\"b.md", "x") ] ~prompt:"go")

let suite =
  ( "context",
    [
      Alcotest.test_case "no files is identity" `Quick test_no_files_is_identity;
      Alcotest.test_case "single file block" `Quick test_single_file_block;
      Alcotest.test_case "multiple files preserve order" `Quick
        test_multiple_files_preserve_order;
      Alcotest.test_case "contents trimmed only at edges" `Quick
        test_contents_trimmed_only_at_edges;
      Alcotest.test_case "path with quote is escaped" `Quick
        test_path_with_quote_is_escaped;
    ] )
