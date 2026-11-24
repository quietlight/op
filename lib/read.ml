let ( let* ) = Result.bind
let default_limit = 2000
let max_line_length = 2000

(* Soft byte budget per read (stops before the next line once reached). *)
let max_total_bytes = 256 * 1024

type error =
  | Not_found of string
  | Is_directory of string
  | Read_error of string

let pp_error = function
  | Not_found path -> Fmt.str "%s: no such file" path
  | Is_directory path -> Fmt.str "%s: is a directory" path
  | Read_error msg -> msg

(* Reject devices and FIFOs that block or never reach EOF. *)
let open_regular path =
  match (Unix.stat path).st_kind with
  | S_REG -> (
      try Ok (open_in_bin path) with Sys_error msg -> Error (Read_error msg))
  | S_DIR -> Error (Is_directory path)
  | _ -> Error (Read_error (path ^ ": not a regular file"))
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Error (Not_found path)
  | exception Unix.Unix_error (e, _, _) ->
      Error (Read_error (path ^ ": " ^ Unix.error_message e))

(* Stream one line, buffering at most [max_line_length + 1] bytes so
   [truncate_line] can detect truncation without unbounded memory use. *)
let next_line ic =
  let buf = Buffer.create 128 in
  let rec go any =
    match In_channel.input_char ic with
    | None -> if any then Some (Buffer.contents buf) else None
    | Some '\n' -> Some (Buffer.contents buf)
    | Some c ->
        if Buffer.length buf <= max_line_length then Buffer.add_char buf c;
        go true
  in
  go false

let skip_lines ic n =
  let rec go k =
    if k < n && Option.is_some (next_line ic) then go (k + 1) else k
  in
  go 0

let truncate_line s =
  if String.length s <= max_line_length then s
  else Util.drop_partial_utf8_tail (String.sub s 0 max_line_length) ^ "..."

(* Track byte budget directly rather than via [Buffer.length], which lags behind
   unflushed [Format] output. *)
let emit_lines ppf ic ~first ~limit =
  let rec go n bytes =
    if n >= first + limit || bytes >= max_total_bytes then n - 1
    else
      match next_line ic with
      | None -> n - 1
      | Some line ->
          let line = truncate_line line in
          Fmt.pf ppf "%6d\t%s\n" n line;
          go (n + 1) (bytes + String.length line + 8)
  in
  go first 0

let slice ic ~offset ~limit =
  let first = max 1 offset in
  let skipped = skip_lines ic (first - 1) in
  if skipped < first - 1 then
    if skipped = 0 then ""
    else Fmt.str "(file has %d lines; offset %d is past end)" skipped offset
  else
    let buf = Buffer.create 256 in
    let ppf = Fmt.with_buffer buf in
    let limit = max 1 limit in
    let last = emit_lines ppf ic ~first ~limit in
    (* Peek one line to detect truncation without reading the rest of the file. *)
    if Option.is_some (next_line ic) then
      if last < first + limit - 1 then
        Fmt.pf ppf
          "... (more lines from offset %d; output truncated at %d bytes)\n"
          (last + 1) max_total_bytes
      else Fmt.pf ppf "... (more lines from offset %d)\n" (last + 1);
    Fmt.flush ppf ();
    Buffer.contents buf

let run ~path ?(offset = 1) ?(limit = default_limit) () =
  let* ic = open_regular path in
  Ok
    (Fun.protect
       ~finally:(fun () -> close_in_noerr ic)
       (fun () -> slice ic ~offset ~limit))

let tool : Tool.t =
  {
    name = "read";
    description =
      "Read a file from the filesystem. Returns the contents with 1-indexed \
       line numbers. Use offset/limit to read a slice of a large file.";
    parameters =
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
                      ("description", `String "Path to the file to read");
                    ] );
                ( "offset",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "1-indexed line number to start reading from" );
                    ] );
                ( "limit",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ("description", `String "Maximum number of lines to read");
                    ] );
              ] );
          ("required", `List [ `String "path" ]);
        ];
    run =
      (fun args ->
        let* path = Tool.required_string args "path" in
        let offset = Tool.optional_int args "offset" in
        let limit = Tool.optional_int args "limit" in
        match run ~path ?offset ?limit () with
        | Ok content -> Tool.text content
        | Error e -> Error (pp_error e));
  }
