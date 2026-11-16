(* Shared utility functions. *)

(* Drop a trailing partial UTF-8 sequence from [s], so a cut made at a byte
   offset never ends mid-character. Walks back over continuation bytes
   (0b10xxxxxx) to the last lead byte and drops it too if its declared length
   runs past the end. An invalid lead byte is left alone for [to_valid_utf8] to
   scrub — this trims truncation damage, it does not validate. *)
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

(* Head-keeping truncation: keep the first [max_bytes] bytes of [s], appending
   "..." if truncation occurred. Preserves at most [max_bytes] bytes of the
   original content (the ellipsis is extra). *)
let truncate ~max_bytes s =
  let n = String.length s in
  if n <= max_bytes then s
  else
    let content = String.sub s 0 max_bytes in
    (* Prefer to break at the last complete line so we don't end on a partial
       one. Failing that, back the byte cut up to a character boundary: not
       every truncated string reaches a request body through [to_valid_utf8]
       (gate summaries and session listing previews do not), so the cut itself
       must not manufacture invalid UTF-8. *)
    match String.rindex_opt content '\n' with
    | Some i when i > 0 -> String.sub s 0 i ^ "\n..."
    | _ -> drop_partial_utf8_tail content ^ "..."

(* Scrub [s] into well-formed UTF-8, replacing each malformed byte sequence with
   U+FFFD. Tool output (truncated mid-character, or raw binary/latin-1 bytes) can
   otherwise carry invalid UTF-8 into a request body, which strict providers
   reject with HTTP 400. Already-valid input round-trips byte-for-byte. Uses the
   stdlib UTF-8 codec (OCaml >= 4.14); no external dependency. *)
let to_valid_utf8 s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    let dec = String.get_utf_8_uchar s !i in
    (* [utf_decode_uchar] is the decoded scalar, or [Uchar.rep] (U+FFFD) when the
       sequence is malformed; [utf_decode_length] is always >= 1, so we advance. *)
    Buffer.add_utf_8_uchar b (Uchar.utf_decode_uchar dec);
    i := !i + Uchar.utf_decode_length dec
  done;
  Buffer.contents b

(* Count the lines in [s] for display purposes. An empty string is 0 lines;
   otherwise it is one more than the number of internal newlines, with a single
   trailing newline not counting as an extra (empty) line. *)
let count_lines s =
  let n = String.length s in
  if n = 0 then 0
  else begin
    let newlines = ref 0 in
    String.iter (fun c -> if c = '\n' then incr newlines) s;
    (* A trailing newline terminates the last line rather than starting a new
       empty one. *)
    if s.[n - 1] = '\n' then !newlines else !newlines + 1
  end

(* Substring search: find the first occurrence of [needle] in [haystack],
   starting at position [start]. Returns the 0-indexed position or [None]. *)
let substring ~start ~needle hay =
  let hn = String.length hay and nn = String.length needle in
  let rec loop i =
    if i + nn > hn then None
    else if String.sub hay i nn = needle then Some i
    else loop (i + 1)
  in
  loop start

(* Neutralise [</tag>] inside text that is about to be wrapped in [<tag>...],
   so injected file content cannot close its own block and have whatever follows
   read as instructions rather than data. Rewritten to the entity form, which is
   still legible to a model but no longer a tag. *)
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

(* Recursively create [dir] and its missing parents (like `mkdir -p`). Existing
   directories and the "", ".", "/" roots are no-ops; a concurrent create that
   wins the race (EEXIST) is ignored. Other Unix errors propagate to the
   caller.

   [perm] defaults to 0o755, right for files the agent writes into a project.
   Pass 0o700 for op's own state under ~/.op_global: the session *files* are
   already 0600, but a world-readable directory still leaks the listing — every
   session name and the branch structure — to any other user on the host. *)
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

(* Read the whole file at [path] as raw bytes. Callers map the [string] error
   (a [Sys_error] message) into their own error variant. Does not distinguish
   "not found" from "is a directory" — a caller that needs those cases should
   check them before calling. *)
let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error msg -> Error msg

(* A process-global monotonic counter for synthesising unique tool-call ids.
   Some providers (older Gemini models) omit call
   ids, and a naive hash of (name, args) collides when a model repeats the same
   bare call — which would create duplicate tool_use_ids if replayed. The agent
   loop's uniquifier repairs any residual collisions, so this only needs to be
   unique within a run. *)
let call_counter = ref 0

let next_call_id () =
  let n = !call_counter in
  incr call_counter;
  Fmt.str "call_%06d" n

(* Merge adjacent same-key groups: consecutive [(k, xs)] pairs sharing a key are
   folded into one by concatenating their lists, preserving order. Adapters use
   this so consecutive tool results (or a trailing hint) land in a single
   message, keeping roles alternating as Anthropic and Gemini expect. *)
let rec merge_adjacent = function
  | (k1, c1) :: (k2, c2) :: rest when k1 = k2 ->
      merge_adjacent ((k1, c1 @ c2) :: rest)
  | x :: rest -> x :: merge_adjacent rest
  | [] -> []
