let ( let* ) = Result.bind
let default_limit = 2000
let max_line_length = 2000

(* Total byte budget for one read result. Without it a full 2000-line slice of
   2000-char lines could return ~4 MB in a single tool result — enough to blow
   the context window and the request body (the nu tool caps its output for
   the same reason). 256 KB comfortably covers real source files within the line
   window while keeping the worst case bounded; the model can page further with
   offset/limit. The cap is soft: it stops before the line that would exceed it,
   so it may include up to one extra long line. *)
let max_total_bytes = 256 * 1024

type error =
  | Not_found of string
  | Is_directory of string
  | Read_error of string

let pp_error = function
  | Not_found path -> Fmt.str "%s: no such file" path
  | Is_directory path -> Fmt.str "%s: is a directory" path
  | Read_error msg -> msg

let file_contents path =
  if not (Sys.file_exists path) then Error (Not_found path)
  else if Sys.is_directory path then Error (Is_directory path)
  else Util.read_file path |> Result.map_error (fun msg -> Read_error msg)

(* Split file contents into lines, dropping the trailing empty element that a
   final newline produces. An empty file yields no lines. *)
let lines_of_string content =
  if content = "" then [||]
  else
    let parts = String.split_on_char '\n' content in
    let parts =
      match List.rev parts with "" :: rest -> List.rev rest | _ -> parts
    in
    Array.of_list parts

(* A long line is cut at a byte offset, so back the cut up to a character
   boundary rather than emitting half a codepoint. *)
let truncate_line s =
  if String.length s <= max_line_length then s
  else Util.drop_partial_utf8_tail (String.sub s 0 max_line_length) ^ "..."

let run ~path ?(offset = 1) ?(limit = default_limit) () =
  let* content = file_contents path in
  let lines = lines_of_string content in
  let total = Array.length lines in
  let start_idx = max 0 (offset - 1) in
  if total = 0 then Ok ""
  else if start_idx >= total then
    Fmt.kstr
      (fun s -> Ok s)
      "(file has %d lines; offset %d is past end)" total offset
  else begin
    let last_idx = min (total - 1) (start_idx + max 1 limit - 1) in
    let buf = Buffer.create 256 in
    let ppf = Fmt.with_buffer buf in
    (* Emit lines until the requested slice ends or the byte budget is reached,
       whichever comes first; returns the index of the last line emitted. The
       running [bytes] count is tracked directly (the line body plus ~8 bytes of
       number/tab/newline) rather than read off the buffer, since the Format
       engine may not have flushed to it yet. *)
    let bytes = ref 0 in
    let rec emit i =
      if i > last_idx || !bytes >= max_total_bytes then i - 1
      else begin
        let line = truncate_line lines.(i) in
        Fmt.pf ppf "%6d\t%s\n" (i + 1) line;
        bytes := !bytes + String.length line + 8;
        emit (i + 1)
      end
    in
    let shown = emit start_idx in
    let remaining = total - 1 - shown in
    if shown < last_idx then
      Fmt.pf ppf "... (%d more lines; output truncated at %d bytes)\n" remaining
        max_total_bytes
    else if last_idx < total - 1 then
      Fmt.pf ppf "... (%d more lines)\n" remaining;
    Fmt.flush ppf ();
    Ok (Buffer.contents buf)
  end

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
