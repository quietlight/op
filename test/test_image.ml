open Test_support

let png_magic = "\x89PNG\r\n\x1a\n"
let gif_magic = "GIF89a"
let jpeg_magic = "\xff\xd8\xff\xe0"
let webp_magic = "RIFF\x00\x00\x00\x00WEBP"

(* --- base64 --- *)

let test_base64_vectors () =
  let cases =
    [
      ("", "");
      ("f", "Zg==");
      ("fo", "Zm8=");
      ("foo", "Zm9v");
      ("foob", "Zm9vYg==");
      ("fooba", "Zm9vYmE=");
      ("foobar", "Zm9vYmFy");
    ]
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        (Fmt.str "base64 %S" input)
        expected
        (Op.Image.base64_encode input))
    cases

(* --- magic-byte sniffing --- *)

let test_media_type_detection () =
  let check label bytes expected =
    Alcotest.(check (option string))
      label expected
      (Op.Image.media_type_of_bytes bytes)
  in
  check "png" png_magic (Some "image/png");
  check "jpeg" jpeg_magic (Some "image/jpeg");
  check "gif" gif_magic (Some "image/gif");
  check "webp" webp_magic (Some "image/webp");
  check "extension is ignored, content wins" ("plain text" ^ png_magic) None;
  check "unknown" "not an image at all" None

(* --- load / validation --- *)

let test_load_png () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "img.png" in
  let raw = png_magic ^ "payload-bytes" in
  write_file path raw;
  match Op.Image.load path with
  | Ok (Op.Llm.Image { media_type; data }) ->
      Alcotest.(check string) "media type" "image/png" media_type;
      Alcotest.(check string) "base64 payload" (Op.Image.base64_encode raw) data
  | Ok (Op.Llm.Text _) -> Alcotest.fail "expected Image, got Text"
  | Error e -> Alcotest.fail e

let test_load_missing () =
  with_temp_dir @@ fun dir ->
  match Op.Image.load (Filename.concat dir "nope.png") with
  | Error e ->
      Alcotest.(check bool)
        "mentions no such file" true
        (contains e "no such file")
  | Ok _ -> Alcotest.fail "expected error for missing file"

let test_load_unsupported () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "doc.png" in
  write_file path "%PDF-1.4 not an image";
  match Op.Image.load path with
  | Error e ->
      Alcotest.(check bool)
        "mentions unsupported" true (contains e "unsupported")
  | Ok _ -> Alcotest.fail "expected error for non-image content"

let test_load_oversize () =
  with_temp_dir @@ fun dir ->
  let path = Filename.concat dir "big.png" in
  let big = png_magic ^ String.make (Op.Image.max_bytes + 1) 'x' in
  write_file path big;
  match Op.Image.load path with
  | Error e -> Alcotest.(check bool) "mentions cap" true (contains e "cap")
  | Ok _ -> Alcotest.fail "expected error for oversize image"

let test_load_all_order () =
  with_temp_dir @@ fun dir ->
  let mk name magic =
    let p = Filename.concat dir name in
    write_file p (magic ^ name);
    p
  in
  let paths =
    [ mk "a.png" png_magic; mk "b.gif" gif_magic; mk "c.jpg" jpeg_magic ]
  in
  match Op.Image.load_all paths with
  | Ok blocks ->
      let types =
        List.map
          (function
            | Op.Llm.Image { media_type; _ } -> media_type
            | Op.Llm.Text _ -> "text")
          blocks
      in
      Alcotest.(check (list string))
        "order preserved"
        [ "image/png"; "image/gif"; "image/jpeg" ]
        types
  | Error e -> Alcotest.fail e

let test_load_all_fails_fast () =
  with_temp_dir @@ fun dir ->
  let ok = Filename.concat dir "ok.png" in
  write_file ok (png_magic ^ "x");
  match Op.Image.load_all [ ok; Filename.concat dir "missing.png" ] with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected the missing file to fail the batch"

(* --image is repeatable, so the per-image cap bounds nothing on its own: the
   run as a whole is capped by image count too. *)
let test_load_all_image_count_cap () =
  with_temp_dir @@ fun dir ->
  let mk i =
    let p = Filename.concat dir (Fmt.str "i%d.png" i) in
    write_file p (png_magic ^ "x");
    p
  in
  let paths = List.init 6 mk in
  match Op.Image.load_all paths with
  | Error e -> Alcotest.(check bool) "names the cap" true (contains e "cap")
  | Ok _ -> Alcotest.fail "expected 6 images to exceed the per-run cap"

let test_load_all_under_count_cap_ok () =
  with_temp_dir @@ fun dir ->
  let mk i =
    let p = Filename.concat dir (Fmt.str "i%d.png" i) in
    write_file p (png_magic ^ "x");
    p
  in
  match Op.Image.load_all (List.init 5 mk) with
  | Ok blocks -> Alcotest.(check int) "all loaded" 5 (List.length blocks)
  | Error e -> Alcotest.fail e

(* --- adapter serialization of image user messages --- *)

let sample_image = Op.Llm.Image { media_type = "image/png"; data = "QUJD" }

let user_with_image =
  [ Op.Llm.System "sys"; Op.Llm.user ~blocks:[ sample_image ] "look" ]

let test_anthropic_image_block () =
  let body =
    Op.Anthropic_adapter.For_testing.build_body ~model:"m" ~tools:[]
      ~messages:user_with_image ()
  in
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body in
  let blocks =
    member "messages" json |> to_list |> List.hd |> member "content" |> to_list
  in
  let types = List.map (fun b -> member "type" b |> to_string) blocks in
  Alcotest.(check (list string)) "text then image" [ "text"; "image" ] types;
  let image = List.nth blocks 1 in
  let source = member "source" image in
  Alcotest.(check string)
    "base64 source" "base64"
    (member "type" source |> to_string);
  Alcotest.(check string)
    "media type" "image/png"
    (member "media_type" source |> to_string);
  Alcotest.(check string) "data" "QUJD" (member "data" source |> to_string)

let test_openai_image_url () =
  let body =
    Op.Openai_adapter.For_testing.build_body ~model:"m" ~tools:[]
      ~messages:user_with_image ()
  in
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body in
  let user =
    member "messages" json |> to_list
    |> List.find (fun m -> member "role" m |> to_string = "user")
  in
  let parts = member "content" user |> to_list in
  let img =
    List.find (fun p -> member "type" p |> to_string = "image_url") parts
  in
  Alcotest.(check string)
    "data uri" "data:image/png;base64,QUJD"
    (member "image_url" img |> member "url" |> to_string)

let test_openai_text_only_is_bare_string () =
  (* A text-only user message must serialize byte-identically to before: a bare
     string, not a part list. *)
  let body =
    Op.Openai_adapter.For_testing.build_body ~model:"m" ~tools:[]
      ~messages:[ Op.Llm.user "plain" ]
      ()
  in
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string body in
  let user =
    member "messages" json |> to_list
    |> List.find (fun m -> member "role" m |> to_string = "user")
  in
  Alcotest.(check string)
    "bare string content" "plain"
    (member "content" user |> to_string)

let suite =
  ( "image",
    [
      Alcotest.test_case "base64 vectors" `Quick test_base64_vectors;
      Alcotest.test_case "media type detection" `Quick test_media_type_detection;
      Alcotest.test_case "load png" `Quick test_load_png;
      Alcotest.test_case "load missing" `Quick test_load_missing;
      Alcotest.test_case "load unsupported" `Quick test_load_unsupported;
      Alcotest.test_case "load oversize" `Quick test_load_oversize;
      Alcotest.test_case "load_all order" `Quick test_load_all_order;
      Alcotest.test_case "load_all fails fast" `Quick test_load_all_fails_fast;
      Alcotest.test_case "load_all image count cap" `Quick
        test_load_all_image_count_cap;
      Alcotest.test_case "load_all under count cap" `Quick
        test_load_all_under_count_cap_ok;
      Alcotest.test_case "anthropic image block" `Quick
        test_anthropic_image_block;
      Alcotest.test_case "openai image_url" `Quick test_openai_image_url;
      Alcotest.test_case "openai text-only bare string" `Quick
        test_openai_text_only_is_bare_string;
    ] )
