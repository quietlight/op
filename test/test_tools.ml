(* Tests for Op.Tools tool selection.

   These derive their expectations from [Op.Tools.known] rather than
   hard-coding the tool list, so changing the set (e.g. dropping glob once nu
   covers it) does not break them. *)

let fake_nu : Op.Tool.t =
  {
    name = "nu";
    description = "fake nu";
    parameters = `Assoc [];
    run = (fun _ -> Op.Tool.text "");
  }

let names tools = List.map (fun (t : Op.Tool.t) -> t.name) tools

(* Building the full known set yields exactly [known], in canonical order. *)
let test_known_builds () =
  let tools = Op.Tools.select ~nu:(Some fake_nu) Op.Tools.known in
  Alcotest.(check (list string))
    "known names in canonical order" Op.Tools.known (names tools)

(* nu is the execution tool; there is no bash. *)
let test_nu_is_the_execution_tool () =
  Alcotest.(check bool) "nu is known" true (List.mem "nu" Op.Tools.known);
  Alcotest.(check bool) "bash is gone" false (List.mem "bash" Op.Tools.known)

(* [select] emits requested tools in canonical order, regardless of input
   order. *)
let test_select_canonical_order () =
  let tools =
    Op.Tools.select ~nu:(Some fake_nu) [ "nu"; "read"; "write"; "edit"; "glob" ]
  in
  Alcotest.(check (list string))
    "canonical order, nu last"
    [ "read"; "write"; "edit"; "glob"; "nu" ]
    (names tools)

(* A requested caller-built tool with no builder supplied is dropped. *)
let test_select_drops_unbuilt () =
  let tools = Op.Tools.select ~nu:None [ "read"; "nu" ] in
  Alcotest.(check (list string)) "only read survives" [ "read" ] (names tools)

(* Unknown names are ignored and duplicates collapse. *)
let test_select_ignores_unknown_and_dups () =
  let tools = Op.Tools.select ~nu:None [ "read"; "read"; "bogus"; "glob" ] in
  Alcotest.(check (list string))
    "dedup + known only" [ "read"; "glob" ] (names tools)

(* [validate] backs both --tools and a profile's tools list: known names pass
   through, the empty set is valid (a chat run), an unknown name errors. *)
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
