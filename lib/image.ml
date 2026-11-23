(* Loading and validating image attachments for [--image]. Detects the media
   type from magic bytes (not the extension), enforces a per-image size cap, and
   base64-encodes the payload into a neutral [Llm.Image] block. No decoding or
   resizing: op only reads bytes, sniffs the type, and hands the base64 to the
   adapters. See image.mli. *)

(* 5 MB raw: a generous safety net at the ~100-250 KB payloads this targets,
   well inside every provider limit. *)
let max_bytes = 5 * 1024 * 1024

(* --- base64 (standard alphabet, padded) --------------------------------- *)

let b64_alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode s =
  let n = String.length s in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let byte i = Char.code s.[i] in
  let emit v = Buffer.add_char buf b64_alphabet.[v] in
  let i = ref 0 in
  while !i + 3 <= n do
    let b0 = byte !i and b1 = byte (!i + 1) and b2 = byte (!i + 2) in
    emit (b0 lsr 2);
    emit (((b0 land 0x3) lsl 4) lor (b1 lsr 4));
    emit (((b1 land 0xf) lsl 2) lor (b2 lsr 6));
    emit (b2 land 0x3f);
    i := !i + 3
  done;
  (match n - !i with
  | 1 ->
      let b0 = byte !i in
      emit (b0 lsr 2);
      emit ((b0 land 0x3) lsl 4);
      Buffer.add_string buf "=="
  | 2 ->
      let b0 = byte !i and b1 = byte (!i + 1) in
      emit (b0 lsr 2);
      emit (((b0 land 0x3) lsl 4) lor (b1 lsr 4));
      emit ((b1 land 0xf) lsl 2);
      Buffer.add_char buf '='
  | _ -> ());
  Buffer.contents buf

(* --- magic-byte sniffing ------------------------------------------------- *)

let starts_with s prefix =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* Match Anthropic's accepted set: png, jpeg, webp, gif. The media type is
   decided by content, not the file name. Returns None for anything else. *)
let media_type_of_bytes s =
  if starts_with s "\x89PNG\r\n\x1a\n" then Some "image/png"
  else if starts_with s "\xff\xd8\xff" then Some "image/jpeg"
  else if starts_with s "GIF87a" || starts_with s "GIF89a" then Some "image/gif"
  else if
    String.length s >= 12
    && String.sub s 0 4 = "RIFF"
    && String.sub s 8 4 = "WEBP"
  then Some "image/webp"
  else None

(* --- loading ------------------------------------------------------------- *)

let load path =
  if not (Sys.file_exists path) then Fmt.error "image %s: no such file" path
  else
    match Util.read_file_capped ~max:max_bytes path with
    | Error e -> Fmt.error "image %s" e
    | Ok data -> (
        if data = "" then Fmt.error "image %s: file is empty" path
        else
          match media_type_of_bytes data with
          | None ->
              Fmt.error
                "image %s: unsupported format (accepted: png, jpeg, webp, gif)"
                path
          | Some media_type ->
              Ok (Llm.Image { media_type; data = base64_encode data }))

(* [--image] is repeatable, so the per-image cap alone bounds nothing: five
   images inside the per-file cap still make a 25 MB request once base64 adds
   its third, which turns a typo in a glob into a provider 400 and a billing
   spike. Cap the run as a whole too, measured on the encoded payload — that is
   what actually goes on the wire. *)
let max_images = 5
let max_total_encoded_bytes = 20 * 1024 * 1024

let encoded_bytes = function
  | Llm.Image { data; _ } -> String.length data
  | _ -> 0

let load_all paths =
  let n = List.length paths in
  if n > max_images then
    Fmt.error "%d images exceeds the %d-image cap for one run" n max_images
  else
    let rec go total = function
      | [] -> Ok []
      | path :: rest -> (
          match load path with
          | Error _ as e -> e
          | Ok block ->
              let total = total + encoded_bytes block in
              if total > max_total_encoded_bytes then
                Fmt.error
                  "images total %d encoded bytes, over the %d-byte cap for one \
                   run"
                  total max_total_encoded_bytes
              else Result.map (List.cons block) (go total rest))
    in
    go 0 paths
