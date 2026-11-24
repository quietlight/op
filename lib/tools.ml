let known = [ "read"; "write"; "edit"; "nu" ]
let err fmt = Fmt.kstr (fun s -> Error s) fmt

let validate names =
  match List.find_opt (fun n -> not (List.mem n known)) names with
  | Some bad -> err "unknown tool %S (known: %s)" bad (String.concat ", " known)
  | None -> Ok names

let build ~nu = function
  | "read" -> Some Read.tool
  | "write" -> Some Write.tool
  | "edit" -> Some Edit.tool
  | "nu" -> nu
  | _ -> None

let select ~nu requested =
  List.filter_map
    (fun name -> if List.mem name requested then build ~nu name else None)
    known
