(* Context-file injection. See context.mli for the interface and the shape of
   the rendered block. *)

(* The contents are data, not instructions, so a file holding a literal
   [</context>] must not be able to close its own block. *)
let block (path, contents) =
  Fmt.str "<context file=%S>\n%s\n</context>" path
    (Util.escape_closing_tag ~tag:"context" (String.trim contents))

let render ~files ~prompt =
  match files with
  | [] -> prompt
  | files ->
      let blocks = List.map block files in
      String.concat "\n\n" (blocks @ [ prompt ])
