(* Escape [</context>] so injected file content cannot close its own block. *)
let block (path, contents) =
  Fmt.str "<context file=%S>\n%s\n</context>" path
    (Util.escape_closing_tag ~tag:"context" (String.trim contents))

let render ~files ~prompt =
  match files with
  | [] -> prompt
  | files ->
      let blocks = List.map block files in
      String.concat "\n\n" (blocks @ [ prompt ])
