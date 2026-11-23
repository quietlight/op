let ( let* ) = Result.bind

type error =
  | Not_found of string
  | Read_error of string
  | Write_error of string
  | Substring_not_found
  | Not_unique of int
  | No_change

let pp_error = function
  | Not_found path -> Fmt.str "%s: no such file" path
  | Read_error msg -> msg
  | Write_error msg -> msg
  | Substring_not_found -> "old_string was not found in the file"
  | Not_unique n ->
      Fmt.str
        "old_string is not unique (%d occurrences); add more surrounding \
         context or set replace_all"
        n
  | No_change -> "old_string and new_string are identical; nothing to do"

(* ------------------------------------------------------------------ *)
(* Substring search / replacement helpers.                            *)
(* ------------------------------------------------------------------ *)

let from s start sub = Util.substring ~start ~needle:sub s

let count_occurrences ~sub s =
  if sub = "" then 0
  else
    let m = String.length sub in
    let rec loop i acc =
      match from s i sub with None -> acc | Some j -> loop (j + m) (acc + 1)
    in
    loop 0 0

let replace_first ~sub ~by s =
  match from s 0 sub with
  | None -> s
  | Some i ->
      let m = String.length sub in
      String.sub s 0 i ^ by ^ String.sub s (i + m) (String.length s - i - m)

let replace_all_sub ~sub ~by s =
  let m = String.length sub in
  let buf = Buffer.create (String.length s) in
  let rec loop i =
    match from s i sub with
    | None -> Buffer.add_substring buf s i (String.length s - i)
    | Some j ->
        Buffer.add_substring buf s i (j - i);
        Buffer.add_string buf by;
        loop (j + m)
  in
  loop 0;
  Buffer.contents buf

(* Edit holds the whole file in memory (twice, while replacing), so it refuses
   anything past a size no hand-edited text file reaches. *)
let max_file_bytes = 16 * 1024 * 1024

let read_file path =
  if not (Sys.file_exists path) then Error (Not_found path)
  else if Sys.is_directory path then
    Error (Read_error (path ^ ": is a directory"))
  else
    Util.read_file_capped ~max:max_file_bytes path
    |> Result.map_error (fun msg -> Read_error msg)

let write_file path content =
  try
    Out_channel.with_open_bin path (fun oc ->
        Out_channel.output_string oc content);
    Ok ()
  with Sys_error msg -> Error (Write_error msg)

let run ~path ~old_string ~new_string ?(replace_all = false) () =
  let* content = read_file path in
  let n = count_occurrences ~sub:old_string content in
  if n = 0 then Error Substring_not_found
  else if n > 1 && not replace_all then Error (Not_unique n)
  else
    let updated =
      if replace_all then replace_all_sub ~sub:old_string ~by:new_string content
      else replace_first ~sub:old_string ~by:new_string content
    in
    if updated = content then Error No_change
    else
      let count = if replace_all then n else 1 in
      let* () = write_file path updated in
      Ok count

let parameters : Yojson.Safe.t =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ( "path",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "Path of the file to edit");
                ] );
            ( "old_string",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "The exact text to replace");
                ] );
            ( "new_string",
              `Assoc
                [
                  ("type", `String "string");
                  ("description", `String "The text to replace it with");
                ] );
            ( "replace_all",
              `Assoc
                [
                  ("type", `String "boolean");
                  ( "description",
                    `String "Replace all occurrences (default false)" );
                ] );
          ] );
      ( "required",
        `List [ `String "path"; `String "old_string"; `String "new_string" ] );
    ]

let tool : Tool.t =
  {
    name = "edit";
    description =
      "Replace an exact string in a file. old_string must match exactly and be \
       unique unless replace_all is true.";
    parameters;
    run =
      (fun args ->
        let* path = Tool.required_string args "path" in
        let* old_string = Tool.required_string args "old_string" in
        let* new_string = Tool.required_string args "new_string" in
        let replace_all =
          Option.value (Tool.optional_bool args "replace_all") ~default:false
        in
        match run ~path ~old_string ~new_string ~replace_all () with
        | Ok n -> Fmt.kstr Tool.text "Made %d replacement(s) in %s" n path
        | Error e -> Error (pp_error e));
  }
