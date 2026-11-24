let drop_partial_utf8_tail s =
  let n = String.length s in
  let is_continuation c = Char.code c land 0xC0 = 0x80 in
  let rec lead_index i =
    if i > 0 && is_continuation s.[i] then lead_index (i - 1) else i
  in
  if n = 0 then s
  else
    let i = lead_index (n - 1) in
    let b = Char.code s.[i] in
    let width =
      if b < 0x80 then 1
      else if b land 0xE0 = 0xC0 then 2
      else if b land 0xF0 = 0xE0 then 3
      else if b land 0xF8 = 0xF0 then 4
      else 1
    in
    if i + width <= n then s else String.sub s 0 i

let truncate ~max_bytes s =
  let n = String.length s in
  if n <= max_bytes then s
  else
    let content = String.sub s 0 max_bytes in
    match String.rindex_opt content '\n' with
    | Some i when i > 0 -> String.sub s 0 i ^ "\n..."
    | _ -> drop_partial_utf8_tail content ^ "..."

let to_valid_utf8 s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let dec = String.get_utf_8_uchar s !i in
    Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar dec);
    i := !i + Uchar.utf_decode_length dec
  done;
  Buffer.contents b

let count_lines s =
  let n = String.length s in
  if n = 0 then 0
  else begin
    let newlines = ref 0 in
    String.iter (fun c -> if c = '\n' then incr newlines) s;
    if s.[n - 1] = '\n' then !newlines else !newlines + 1
  end

let substring ~start ~needle hay =
  let hn = String.length hay and nn = String.length needle in
  let rec loop i =
    if i + nn > hn then None
    else if String.sub hay i nn = needle then Some i
    else loop (i + 1)
  in
  loop start

let escape_closing_tag ~tag s =
  let needle = "</" ^ tag ^ ">" in
  let replacement = "&lt;/" ^ tag ^ "&gt;" in
  let nl = String.length needle and n = String.length s in
  let buf = Buffer.create n in
  let rec go start =
    match substring ~start ~needle s with
    | None -> Buffer.add_string buf (String.sub s start (n - start))
    | Some i ->
        Buffer.add_string buf (String.sub s start (i - start));
        Buffer.add_string buf replacement;
        go (i + nl)
  in
  go 0;
  Buffer.contents buf

let home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some h -> Some h
  | None -> (
      match Sys.getenv_opt "USERPROFILE" with
      | Some h -> Some h
      | None -> (
          match (Sys.getenv_opt "HOMEDRIVE", Sys.getenv_opt "HOMEPATH") with
          | Some d, Some p -> Some (d ^ p)
          | _ -> None))

let path_list_sep = if Sys.win32 then ";" else ":"

let rec mkdir_p ?(perm = 0o755) dir =
  if dir = "" || dir = "." || dir = "/" || Sys.file_exists dir then ()
  else begin
    mkdir_p ~perm (Filename.dirname dir);
    try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error msg -> Error msg

let read_file_capped ~max path =
  match Unix.stat path with
  | { st_kind = S_REG; st_size; _ } when st_size > max ->
      Fmt.error "%s: %d bytes exceeds the %d-byte cap" path st_size max
  | { st_kind = S_REG; _ } -> read_file path
  | _ -> Fmt.error "%s: not a regular file" path
  | exception Unix.Unix_error (e, _, _) ->
      Fmt.error "%s: %s" path (Unix.error_message e)

let call_counter = ref 0

let next_call_id () =
  let n = !call_counter in
  incr call_counter;
  Fmt.str "call_%06d" n

let rec merge_adjacent = function
  | (k1, c1) :: (k2, c2) :: rest when k1 = k2 ->
      merge_adjacent ((k1, c1 @ c2) :: rest)
  | x :: rest -> x :: merge_adjacent rest
  | [] -> []

let human_bytes n =
  if n < 1024 then Fmt.str "%d B" n
  else if n < 1024 * 1024 then Fmt.str "%d KB" (n / 1024)
  else Fmt.str "%d MB" (n / (1024 * 1024))
