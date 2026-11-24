type t = {
  oc : out_channel option;
  path : string;
  rng : Random.State.t;
  ids : (string, unit) Hashtbl.t;
  sink : Yojson.Safe.t -> unit;
  mutable leaf : string option;
}

let version = 3

let err_leaf_not_found id =
  Fmt.kstr (fun s -> Error s) "leaf id %s not found in session" id

let now_ms () = Unix.gettimeofday () *. 1000.

let iso8601 ms =
  let tm = Unix.gmtime (ms /. 1000.) in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    (int_of_float ms mod 1000 |> abs)

let hex bytes =
  String.concat "" (List.map (Fmt.str "%02x") (Array.to_list bytes))

let random_bytes rng n = Array.init n (fun _ -> Random.State.int rng 256)

(* UUIDv7 (48-bit ms timestamp + version 7 / variant 10) for time-sorted filenames. *)
let uuid_v7 rng ms =
  let b = random_bytes rng 16 in
  let t = Int64.of_float ms in
  for i = 0 to 5 do
    b.(i) <- Int64.(to_int (logand (shift_right_logical t ((5 - i) * 8)) 0xFFL))
  done;
  b.(6) <- 0x70 lor (b.(6) land 0x0f);
  b.(8) <- 0x80 lor (b.(8) land 0x3f);
  let s = hex b in
  Fmt.str "%s-%s-%s-%s-%s" (String.sub s 0 8) (String.sub s 8 4)
    (String.sub s 12 4) (String.sub s 16 4) (String.sub s 20 12)

(* 8-hex-char entry id unique within the session (pi convention). *)
let fresh_id t =
  let rec gen () =
    let id = hex (random_bytes t.rng 4) in
    if Hashtbl.mem t.ids id then gen () else id
  in
  let id = gen () in
  Hashtbl.add t.ids id ();
  id

let default_root () =
  match Util.home_dir () with
  | Some home -> Ok (Filename.concat home ".op_global/sessions")
  | None -> Error "HOME environment variable not set"

let dir_for ~root ~cwd =
  let stripped =
    if cwd <> "" && (cwd.[0] = '/' || cwd.[0] = '\\') then
      String.sub cwd 1 (String.length cwd - 1)
    else cwd
  in
  let mapped =
    String.map (function '/' | '\\' | ':' -> '-' | c -> c) stripped
  in
  Filename.concat root (Fmt.str "--%s--" mapped)

(* Flush to disk before invoking [t.sink] so disk state never depends on the consumer. *)
let write_json t json =
  Option.iter
    (fun oc ->
      output_string oc (Yojson.Safe.to_string json);
      output_char oc '\n';
      flush oc)
    t.oc;
  t.sink json

let append t ~type_ fields =
  let id = fresh_id t in
  let parent = match t.leaf with Some p -> `String p | None -> `Null in
  write_json t
    (`Assoc
       (("type", `String type_)
       :: ("id", `String id)
       :: ("parentId", parent)
       :: ("timestamp", `String (iso8601 (now_ms ())))
       :: fields));
  t.leaf <- Some id

let append_message t message =
  append t ~type_:"message" [ ("message", message) ]

let append_model_change t ~provider ~model =
  append t ~type_:"model_change"
    [ ("provider", `String provider); ("modelId", `String model) ]

let path t = t.path
let leaf t = t.leaf

let id t =
  let base = Filename.remove_extension (Filename.basename t.path) in
  match String.rindex_opt base '_' with
  | Some i -> String.sub base (i + 1) (String.length base - i - 1)
  | None -> base

let write_preamble t ~id ~timestamp ~cwd ~parent_session ~provider ~model =
  let parent =
    match parent_session with
    | Some p -> [ ("parentSession", `String p) ]
    | None -> []
  in
  write_json t
    (`Assoc
       ([
          ("type", `String "session");
          ("version", `Int version);
          ("id", `String id);
          ("timestamp", `String timestamp);
          ("cwd", `String cwd);
        ]
       @ parent));
  append_model_change t ~provider ~model;
  append t ~type_:"thinking_level_change" [ ("thinkingLevel", `String "off") ]

let unsaved ?(sink = ignore) ?parent_session ~cwd ~provider ~model () =
  let rng = Random.State.make_self_init () in
  let ms = now_ms () in
  let t =
    { oc = None; path = ""; rng; ids = Hashtbl.create 64; sink; leaf = None }
  in
  write_preamble t ~id:(uuid_v7 rng ms) ~timestamp:(iso8601 ms) ~cwd
    ~parent_session ~provider ~model;
  t

let v ?(sink = ignore) ?parent_session ~dir ~cwd ~provider ~model () =
  try
    (* 0700 dir / 0600 file: session logs contain private file contents and output. *)
    Util.mkdir_p ~perm:0o700 dir;
    let rng = Random.State.make_self_init () in
    let ms = now_ms () in
    let timestamp = iso8601 ms in
    let id = uuid_v7 rng ms in
    let file_stamp =
      String.map (function ':' | '.' -> '-' | c -> c) timestamp
    in
    let path = Filename.concat dir (Fmt.str "%s_%s.jsonl" file_stamp id) in
    let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o600 path in
    let t =
      { oc = Some oc; path; rng; ids = Hashtbl.create 64; sink; leaf = None }
    in
    write_preamble t ~id ~timestamp ~cwd ~parent_session ~provider ~model;
    Ok t
  with Sys_error msg | Unix.Unix_error (_, msg, _) -> Error msg

let ms_field () = ("timestamp", `Int (int_of_float (now_ms ())))

let user_content ~text ~blocks =
  let text_block =
    if text = "" && blocks <> [] then []
    else [ `Assoc [ ("type", `String "text"); ("text", `String text) ] ]
  in
  text_block
  @ List.map
      (function
        | Llm.Text s -> `Assoc [ ("type", `String "text"); ("text", `String s) ]
        | Llm.Image { media_type; data } ->
            `Assoc
              [
                ("type", `String "image");
                ("data", `String data);
                ("mimeType", `String media_type);
              ])
      blocks

let append_user t ?(blocks = []) ~text () =
  append_message t
    (`Assoc
       [
         ("role", `String "user");
         ("content", `List (user_content ~text ~blocks));
         ms_field ();
       ])

(* Pi Usage object; cost fields stay 0 since op does not track pricing. *)
let usage_object ~input ~output ~cache_read ~cache_write =
  let zero = `Int 0 in
  `Assoc
    [
      ("input", `Int input);
      ("output", `Int output);
      ("cacheRead", `Int cache_read);
      ("cacheWrite", `Int cache_write);
      ("totalTokens", `Int (input + output));
      ( "cost",
        `Assoc
          [
            ("input", zero);
            ("output", zero);
            ("cacheRead", zero);
            ("cacheWrite", zero);
            ("total", zero);
          ] );
    ]

let append_assistant t ~api ~provider ~model (turn : Llm.assistant_turn) =
  let thinking =
    match turn.thinking with
    | Some (Reasoning { text; signature }) ->
        let fields =
          [ ("type", `String "thinking"); ("thinking", `String text) ]
        in
        let fields =
          match signature with
          | Some signature ->
              fields @ [ ("thinkingSignature", `String signature) ]
          | None -> fields
        in
        [ `Assoc fields ]
    | Some (Redacted data) ->
        [
          `Assoc
            [
              ("type", `String "thinking");
              ("redacted", `Bool true);
              ("data", `String data);
            ];
        ]
    | None -> []
  in
  let text =
    if turn.text = "" then []
    else [ `Assoc [ ("type", `String "text"); ("text", `String turn.text) ] ]
  in
  let calls =
    List.map
      (fun (tc : Llm.tool_call) ->
        `Assoc
          [
            ("type", `String "toolCall");
            ("id", `String tc.id);
            ("name", `String tc.name);
            ("arguments", tc.arguments);
          ])
      turn.tool_calls
  in
  let stop_reason = if turn.tool_calls = [] then "stop" else "toolUse" in
  let usage =
    match turn.usage with
    | Some { input; output; cache_read; cache_creation } ->
        usage_object ~input ~output ~cache_read ~cache_write:cache_creation
    | None -> usage_object ~input:0 ~output:0 ~cache_read:0 ~cache_write:0
  in
  append_message t
    (`Assoc
       [
         ("role", `String "assistant");
         ("content", `List (thinking @ text @ calls));
         ("api", `String api);
         ("provider", `String provider);
         ("model", `String model);
         ("usage", usage);
         ("stopReason", `String stop_reason);
         ms_field ();
       ])

let details_field (tr : Llm.tool_result) =
  match tr.details with Some d -> [ ("details", d) ] | None -> []

let append_tool_result t (tr : Llm.tool_result) =
  append_message t
    (`Assoc
       ([
          ("role", `String "toolResult");
          ("toolCallId", `String tr.id);
          ("toolName", `String tr.name);
          ( "content",
            `List
              [
                `Assoc
                  [ ("type", `String "text"); ("text", `String tr.content) ];
              ] );
          ("isError", `Bool tr.is_error);
          ms_field ();
        ]
       @ details_field tr))

let close t = Option.iter close_out t.oc

let resume ?(sink = ignore) ~path ~leaf () =
  let ( let* ) = Result.bind in
  let* tree = Session_read.load path in
  let ids = Session_read.entry_ids tree in
  if not (List.mem leaf ids) then err_leaf_not_found leaf
  else
    try
      let tbl = Hashtbl.create 64 in
      List.iter (fun id -> Hashtbl.replace tbl id ()) ids;
      let oc = open_out_gen [ Open_append; Open_wronly ] 0o600 path in
      Ok
        {
          oc = Some oc;
          path;
          rng = Random.State.make_self_init ();
          ids = tbl;
          sink;
          leaf = Some leaf;
        }
    with Sys_error msg | Unix.Unix_error (_, msg, _) -> Error msg
