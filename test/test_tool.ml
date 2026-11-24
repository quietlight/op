let test_required_string_ok () =
  let json = `Assoc [ ("path", `String "foo.ml") ] in
  match Op.Tool.required_string json "path" with
  | Ok s -> Alcotest.(check string) "value" "foo.ml" s
  | Error e -> Alcotest.fail e

let test_required_string_missing () =
  let json = `Assoc [] in
  match Op.Tool.required_string json "path" with
  | Ok _ -> Alcotest.fail "expected error for missing field"
  | Error e ->
      Alcotest.(check bool)
        "mentions required" true
        (Test_support.contains e "required")

let test_required_string_wrong_type () =
  let json = `Assoc [ ("path", `Int 7) ] in
  match Op.Tool.required_string json "path" with
  | Ok _ -> Alcotest.fail "expected error for non-string field"
  | Error e ->
      Alcotest.(check bool)
        "mentions string" true
        (Test_support.contains e "string")

let test_optional_string () =
  let json = `Assoc [ ("a", `String "x"); ("b", `Int 1) ] in
  Alcotest.(check (option string))
    "present" (Some "x")
    (Op.Tool.optional_string json "a");
  Alcotest.(check (option string))
    "wrong type" None
    (Op.Tool.optional_string json "b");
  Alcotest.(check (option string))
    "absent" None
    (Op.Tool.optional_string json "c")

let test_optional_int () =
  let json = `Assoc [ ("n", `Int 42); ("s", `String "x") ] in
  Alcotest.(check (option int))
    "present" (Some 42)
    (Op.Tool.optional_int json "n");
  Alcotest.(check (option int))
    "wrong type" None
    (Op.Tool.optional_int json "s");
  Alcotest.(check (option int)) "absent" None (Op.Tool.optional_int json "m")

let test_optional_bool () =
  let json = `Assoc [ ("f", `Bool true); ("s", `String "x") ] in
  Alcotest.(check (option bool))
    "present" (Some true)
    (Op.Tool.optional_bool json "f");
  Alcotest.(check (option bool))
    "wrong type" None
    (Op.Tool.optional_bool json "s");
  Alcotest.(check (option bool)) "absent" None (Op.Tool.optional_bool json "g")

let test_pp () =
  let tool : Op.Tool.t =
    {
      name = "demo";
      description = "does a thing";
      parameters = `Assoc [];
      run = (fun _ -> Op.Tool.text "");
    }
  in
  let s = Fmt.str "%a" Op.Tool.pp tool in
  Alcotest.(check bool) "contains name" true (Test_support.contains s "demo");
  Alcotest.(check bool)
    "contains description" true
    (Test_support.contains s "does a thing")

let suite =
  ( "tool",
    [
      ("required_string ok", `Quick, test_required_string_ok);
      ("required_string missing", `Quick, test_required_string_missing);
      ("required_string wrong type", `Quick, test_required_string_wrong_type);
      ("optional_string", `Quick, test_optional_string);
      ("optional_int", `Quick, test_optional_int);
      ("optional_bool", `Quick, test_optional_bool);
      ("pp", `Quick, test_pp);
    ] )
