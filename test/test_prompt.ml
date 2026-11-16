(* Tests for Op.Prompt — pure prompt-source resolution. *)

open Op.Prompt

(* True if [needle] occurs in [hay]. *)
let contains ~needle hay = Op.Util.substring ~start:0 ~needle hay <> None

(* The returned prompt when [Ok], else [None]. *)
let ok_of = function Ok s -> Some s | Error _ -> None

(* A short label for an error variant, so error cases assert on the specific
   constructor without exposing internal equality. *)
let err_label = function
  | No_source -> "no_source"
  | Empty File -> "empty_file"
  | Empty Arg -> "empty_arg"
  | Empty Stdin -> "empty_stdin"
  | Ambiguous -> "ambiguous"

let result_label = function Ok _ -> "ok" | Error e -> err_label e

(* --- resolve: --prompt FILE wins --- *)

let test_file_wins_over_arg_and_stdin () =
  Alcotest.(check (option string))
    "file overrides arg and stdin" (Some "from file")
    (ok_of
       (resolve ~file:(Some "from file") ~arg:(Some "from arg")
          ~stdin:(Some "from stdin")))

let test_file_wins_even_with_nonempty_arg () =
  Alcotest.(check (option string))
    "file used even with a non-empty arg" (Some "f")
    (ok_of (resolve ~file:(Some "f") ~arg:(Some "arg") ~stdin:None))

let test_file_content_is_trimmed () =
  Alcotest.(check (option string))
    "file content trimmed" (Some "hi")
    (ok_of (resolve ~file:(Some "  hi\n") ~arg:None ~stdin:None))

let test_empty_file_is_error_even_with_arg () =
  (* --prompt overrides PROMPT, so an empty file is an error, not a fallback
     to the argument. *)
  Alcotest.(check string)
    "empty file errors even when arg is present" "empty_file"
    (result_label (resolve ~file:(Some "") ~arg:(Some "arg") ~stdin:None))

(* --- resolve: PROMPT argument --- *)

let test_arg_alone () =
  Alcotest.(check (option string))
    "arg used when no file" (Some "a")
    (ok_of (resolve ~file:None ~arg:(Some "a") ~stdin:None))

let test_arg_is_trimmed () =
  Alcotest.(check (option string))
    "arg trimmed" (Some "a")
    (ok_of (resolve ~file:None ~arg:(Some "  a \n") ~stdin:None))

let test_empty_arg_alone_is_error () =
  Alcotest.(check string)
    "whitespace arg is empty" "empty_arg"
    (result_label (resolve ~file:None ~arg:(Some "  ") ~stdin:None))

(* --- resolve: stdin --- *)

let test_stdin_alone () =
  Alcotest.(check (option string))
    "stdin used when no file or arg" (Some "s")
    (ok_of (resolve ~file:None ~arg:None ~stdin:(Some "s")))

let test_stdin_is_trimmed () =
  Alcotest.(check (option string))
    "stdin trimmed" (Some "s")
    (ok_of (resolve ~file:None ~arg:None ~stdin:(Some "\n s \n")))

let test_empty_stdin_alone_is_error () =
  Alcotest.(check string)
    "whitespace stdin is empty" "empty_stdin"
    (result_label (resolve ~file:None ~arg:None ~stdin:(Some "\t\n")))

(* --- resolve: arg + stdin interaction (the ambiguity fix) --- *)

let test_arg_and_nonempty_stdin_is_ambiguous () =
  Alcotest.(check string)
    "arg + non-empty stdin is ambiguous" "ambiguous"
    (result_label (resolve ~file:None ~arg:(Some "a") ~stdin:(Some "s")))

let test_arg_and_empty_stdin_uses_arg () =
  (* An empty stdin (e.g. /dev/null) is not a real prompt, so the arg wins. *)
  Alcotest.(check (option string))
    "arg + empty stdin uses arg" (Some "a")
    (ok_of (resolve ~file:None ~arg:(Some "a") ~stdin:(Some "")))

let test_arg_and_whitespace_stdin_uses_arg () =
  Alcotest.(check (option string))
    "arg + whitespace stdin uses arg" (Some "a")
    (ok_of (resolve ~file:None ~arg:(Some "a") ~stdin:(Some "   ")))

let test_empty_arg_and_nonempty_stdin_uses_stdin () =
  Alcotest.(check (option string))
    "empty arg + non-empty stdin uses stdin" (Some "s")
    (ok_of (resolve ~file:None ~arg:(Some "") ~stdin:(Some "s")))

let test_both_empty_names_arg () =
  (* Both provided but empty: not ambiguous (no conflict), just empty. The
     higher-precedence source (arg) is named. *)
  Alcotest.(check string)
    "both empty is empty_arg" "empty_arg"
    (result_label (resolve ~file:None ~arg:(Some "") ~stdin:(Some "")))

(* --- resolve: nothing provided --- *)

let test_nothing_is_no_source () =
  Alcotest.(check string)
    "no sources is no_source" "no_source"
    (result_label (resolve ~file:None ~arg:None ~stdin:None))

(* --- message --- *)

let test_message_no_source_keeps_usage () =
  let m = message ~file_path:None No_source in
  Alcotest.(check bool) "mentions usage" true (contains ~needle:"usage" m);
  Alcotest.(check bool) "mentions --help" true (contains ~needle:"op --help" m)

let test_message_no_source_resuming_names_session () =
  (* When a resume was requested but no prompt given, the message points at the
     --sessionId form rather than the bare usage line. *)
  let m = message ~file_path:None ~resuming:true No_source in
  Alcotest.(check bool)
    "names --sessionId" true
    (contains ~needle:"--sessionId" m);
  Alcotest.(check bool)
    "still mentions a prompt" true
    (contains ~needle:"prompt" m)

let test_message_no_source_not_resuming_omits_session () =
  (* The default (non-resume) No_source message keeps the bare usage line. *)
  let m = message ~file_path:None No_source in
  Alcotest.(check bool)
    "no --sessionId in bare usage" false
    (contains ~needle:"--sessionId" m)

let test_message_empty_file_names_path () =
  let m = message ~file_path:(Some "empty.txt") (Empty File) in
  Alcotest.(check bool) "names the path" true (contains ~needle:"empty.txt" m);
  Alcotest.(check bool) "says empty" true (contains ~needle:"empty" m)

let test_message_empty_file_without_path () =
  let m = message ~file_path:None (Empty File) in
  Alcotest.(check bool)
    "still says empty" true
    (contains ~needle:"prompt file" m)

let test_message_empty_arg () =
  Alcotest.(check bool)
    "names argument" true
    (contains ~needle:"argument" (message ~file_path:None (Empty Arg)))

let test_message_empty_stdin () =
  Alcotest.(check bool)
    "names stdin" true
    (contains ~needle:"stdin" (message ~file_path:None (Empty Stdin)))

let test_message_ambiguous_mentions_devnull () =
  let m = message ~file_path:None Ambiguous in
  Alcotest.(check bool) "says ambiguous" true (contains ~needle:"ambiguous" m);
  Alcotest.(check bool) "hints /dev/null" true (contains ~needle:"/dev/null" m)

let suite =
  ( "prompt",
    [
      ("file wins over arg and stdin", `Quick, test_file_wins_over_arg_and_stdin);
      ( "file wins even with non-empty arg",
        `Quick,
        test_file_wins_even_with_nonempty_arg );
      ("file content is trimmed", `Quick, test_file_content_is_trimmed);
      ( "empty file is error even with arg",
        `Quick,
        test_empty_file_is_error_even_with_arg );
      ("arg alone", `Quick, test_arg_alone);
      ("arg is trimmed", `Quick, test_arg_is_trimmed);
      ("empty arg alone is error", `Quick, test_empty_arg_alone_is_error);
      ("stdin alone", `Quick, test_stdin_alone);
      ("stdin is trimmed", `Quick, test_stdin_is_trimmed);
      ("empty stdin alone is error", `Quick, test_empty_stdin_alone_is_error);
      ( "arg and non-empty stdin is ambiguous",
        `Quick,
        test_arg_and_nonempty_stdin_is_ambiguous );
      ("arg and empty stdin uses arg", `Quick, test_arg_and_empty_stdin_uses_arg);
      ( "arg and whitespace stdin uses arg",
        `Quick,
        test_arg_and_whitespace_stdin_uses_arg );
      ( "empty arg and non-empty stdin uses stdin",
        `Quick,
        test_empty_arg_and_nonempty_stdin_uses_stdin );
      ("both empty names arg", `Quick, test_both_empty_names_arg);
      ("nothing is no source", `Quick, test_nothing_is_no_source);
      ( "message no source keeps usage",
        `Quick,
        test_message_no_source_keeps_usage );
      ( "message no source resuming names session",
        `Quick,
        test_message_no_source_resuming_names_session );
      ( "message no source not resuming omits session",
        `Quick,
        test_message_no_source_not_resuming_omits_session );
      ( "message empty file names path",
        `Quick,
        test_message_empty_file_names_path );
      ( "message empty file without path",
        `Quick,
        test_message_empty_file_without_path );
      ("message empty arg", `Quick, test_message_empty_arg);
      ("message empty stdin", `Quick, test_message_empty_stdin);
      ( "message ambiguous mentions devnull",
        `Quick,
        test_message_ambiguous_mentions_devnull );
    ] )
