let fake_nu : Op.Tool.t =
  {
    name = "nu";
    description = "fake nu";
    parameters = `Assoc [];
    run = (fun _ -> Op.Tool.text "");
  }

let names tools = List.map (fun (t : Op.Tool.t) -> t.name) tools

let test_known_builds () =
  let tools = Op.Tools.select ~nu:(Some fake_nu) Op.Tools.known in
  Alcotest.(check (list string))
    "known names in canonical order" Op.Tools.known (names tools)

let test_nu_is_the_execution_tool () =
  Alcotest.(check bool) "nu is known" true (List.mem "nu" Op.Tools.known);
  Alcotest.(check bool) "bash is gone" false (List.mem "bash" Op.Tools.known);
  Alcotest.(check bool) "glob is gone" false (List.mem "glob" Op.Tools.known)

let test_select_canonical_order () =
  let tools =
    Op.Tools.select ~nu:(Some fake_nu) [ "nu"; "read"; "write"; "edit" ]
  in
  Alcotest.(check (list string))
    "canonical order, nu last"
    [ "read"; "write"; "edit"; "nu" ]
    (names tools)

let test_select_drops_unbuilt () =
  let tools = Op.Tools.select ~nu:None [ "read"; "nu" ] in
  Alcotest.(check (list string)) "only read survives" [ "read" ] (names tools)

let test_select_ignores_unknown_and_dups () =
  let tools = Op.Tools.select ~nu:None [ "read"; "read"; "bogus"; "edit" ] in
  Alcotest.(check (list string))
    "dedup + known only" [ "read"; "edit" ] (names tools)

let test_validate () =
  Alcotest.(check (result (list string) string))
    "known subset"
    (Ok [ "read"; "nu" ])
    (Op.Tools.validate [ "read"; "nu" ]);
  Alcotest.(check (result (list string) string))
    "empty is a chat run" (Ok []) (Op.Tools.validate []);
  Alcotest.(check bool)
    "unknown name errors" true
    (Result.is_error (Op.Tools.validate [ "read"; "bash" ]))

let suite =
  ( "tools",
    [
      ("known builds", `Quick, test_known_builds);
      ("validate", `Quick, test_validate);
      ("nu is the execution tool", `Quick, test_nu_is_the_execution_tool);
      ("select canonical order", `Quick, test_select_canonical_order);
      ("select drops unbuilt", `Quick, test_select_drops_unbuilt);
      ( "select ignores unknown and dups",
        `Quick,
        test_select_ignores_unknown_and_dups );
    ] )
