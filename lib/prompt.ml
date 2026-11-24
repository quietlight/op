type source = File | Arg | Stdin
type error = No_source | Empty of source | Ambiguous

let resolve ~file ~arg ~stdin =
  let trim = String.trim in
  match file with
  | Some content ->
      let t = trim content in
      if t = "" then Error (Empty File) else Ok t
  | None -> (
      match (arg, stdin) with
      | Some a, Some s ->
          let ta = trim a and ts = trim s in
          if ta <> "" && ts <> "" then Error Ambiguous
          else if ta <> "" then Ok ta
          else if ts <> "" then Ok ts
          else Error (Empty Arg)
      | Some a, None ->
          let ta = trim a in
          if ta = "" then Error (Empty Arg) else Ok ta
      | None, Some s ->
          let ts = trim s in
          if ts = "" then Error (Empty Stdin) else Ok ts
      | None, None -> Error No_source)

let message ~file_path ?(resuming = false) = function
  | No_source when resuming ->
      "usage: op --sessionId ID \"prompt\" — resuming a session still needs a \
       prompt to continue\n\
       op --help prints man page"
  | No_source ->
      "usage: op \"prompt\" (or pipe a prompt on stdin)\n\
       op --help prints man page"
  | Empty File -> (
      match file_path with
      | Some p ->
          Fmt.str "prompt file %s is empty (after trimming whitespace)" p
      | None -> "prompt file is empty (after trimming whitespace)")
  | Empty Arg -> "prompt argument is empty (after trimming whitespace)"
  | Empty Stdin -> "prompt from stdin is empty (after trimming whitespace)"
  | Ambiguous ->
      "ambiguous input: a PROMPT argument and stdin were both provided; pass \
       only one (or redirect stdin from /dev/null)"
