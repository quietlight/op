(* Pi-compatible session logging. See session.mli for the contract and
   docs/pi-session-format.md for the full format description.

   Entries are appended (and flushed) as they happen, so a crashed or
   interrupted run still leaves a readable log. *)

type t = {
  oc : out_channel;
  path : string;
  rng : Random.State.t;
  ids : (string, unit) Hashtbl.t;
  sink : Yojson.Safe.t -> unit;
      (* tee for every written line, e.g. --stream's stdout *)
  mutable leaf : string option; (* parentId for the next entry *)
}

let version = 3

let err_leaf_not_found id =
  Fmt.kstr (fun s -> Error s) "leaf id %s not found in session" id

(* --- time --------------------------------------------------------------- *)

let now_ms () = Unix.gettimeofday () *. 1000.

(* ISO-8601 UTC with millisecond precision, e.g. 2026-06-10T01:46:04.743Z *)
let iso8601 ms =
  let tm = Unix.gmtime (ms /. 1000.) in
  Fmt.str "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
    (int_of_float ms mod 1000 |> abs)

(* --- identifiers --------------------------------------------------------- *)

let hex bytes =
  String.concat "" (List.map (Fmt.str "%02x") (Array.to_list bytes))

let random_bytes rng n = Array.init n (fun _ -> Random.State.int rng 256)

(* UUIDv7: 48-bit big-endian millisecond timestamp, then random bits with the
   version (7) and variant (10) fields set. Used for session ids so that file
   names sort chronologically, mirroring pi. *)
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

(* Entry ids are 8 hex chars (as in pi), unique within the session. *)
let fresh_id t =
  let rec gen () =
    let id = hex (random_bytes t.rng 4) in
    if Hashtbl.mem t.ids id then gen () else id
  in
  let id = gen () in
  Hashtbl.add t.ids id ();
  id

(* --- paths --------------------------------------------------------------- *)

let default_root () =
  match Util.home_dir () with
  | Some home -> Ok (Filename.concat home ".op_global/sessions")
  | None -> Error "HOME environment variable not set"

(* Mirror pi's encoding: strip one leading separator, then map path
   separators and ':' to '-', wrapped in '--'. *)
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

(* --- entry writing ------------------------------------------------------- *)

(* The single choke point every line — header and tree entries alike — passes
   through, so the sink sees exactly the file's lines, in order. It runs after
   the flush: the log on disk is never held hostage to the consumer. *)
let write_json t json =
  output_string t.oc (Yojson.Safe.to_string json);
  output_char t.oc '\n';
  flush t.oc;
  t.sink json

(* Append a tree entry: id/parentId/timestamp are filled in, and the new
   entry becomes the leaf (parent of whatever comes next). *)
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

(* --- public api ----------------------------------------------------------- *)

let path t = t.path
let leaf t = t.leaf

(* The session id is the [<timestamp>_<uuid>.jsonl] filename's uuid: the part
   after the last '_' (the timestamp carries no '_'). *)
let id t =
  let base = Filename.remove_extension (Filename.basename t.path) in
  match String.rindex_opt base '_' with
  | Some i -> String.sub base (i + 1) (String.length base - i - 1)
  | None -> base

let v ?(sink = ignore) ?parent_session ~dir ~cwd ~provider ~model () =
  try
    (* 0700: the session files are 0600, but a 0755 directory would still hand
       every other user on the host the session listing. *)
    Util.mkdir_p ~perm:0o700 dir;
    let rng = Random.State.make_self_init () in
    let ms = now_ms () in
    let timestamp = iso8601 ms in
    let id = uuid_v7 rng ms in
    let file_stamp =
      String.map (function ':' | '.' -> '-' | c -> c) timestamp
    in
    let path = Filename.concat dir (Fmt.str "%s_%s.jsonl" file_stamp id) in
    (* 0600: session logs can hold file contents and command output, so they are
       created private to the user rather than the default world-readable 0644. *)
    let oc = open_out_gen [ Open_wronly; Open_creat; Open_trunc ] 0o600 path in
    let t = { oc; path; rng; ids = Hashtbl.create 64; sink; leaf = None } in
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
    append t ~type_:"thinking_level_change" [ ("thinkingLevel", `String "off") ];
    Ok t
  with Sys_error msg | Unix.Unix_error (_, msg, _) -> Error msg

(* pi stores the message-level timestamp as Unix milliseconds. *)
let ms_field () = ("timestamp", `Int (int_of_float (now_ms ())))

(* A pi user content list: the text body (when non-empty) then each attachment
   as an image block ([docs/pi-session-format.md]). Writing image blocks is
   schema-valid and additive; a text-only turn keeps the single text-block list
   op has always written. *)
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

(* A pi Usage object. Token counts come from the provider when known; the cost
   fields stay 0 since op does not know prices. cacheRead/cacheWrite carry
   prompt-caching token counts when the provider reports them (0 otherwise).
   With [~input:0 ~output:0 ~cache_read:0 ~cache_write:0] this is the fully
   zeroed object used when the turn carried no usage. *)
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

(* pi's [details] is optional and omitted when absent, so a details-free result
   keeps the exact shape op has always written. *)
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

let append_custom t ~custom_type ~data =
  append t ~type_:"custom"
    [ ("customType", `String custom_type); ("data", data) ]

let close t = close_out t.oc

(* --- resuming ------------------------------------------------------------ *)

(* Reopen an existing session file for appending. Reading (id collection, leaf
   validation) is delegated to {!Session_read}; here we only open the file in
   append mode and seed the id set so fresh ids stay unique. *)
let resume ?(sink = ignore) ~path ~leaf () =
  let ( let* ) = Result.bind in
  let* ids = Session_read.entry_ids path in
  if not (List.mem leaf ids) then err_leaf_not_found leaf
  else
    try
      let tbl = Hashtbl.create 64 in
      List.iter (fun id -> Hashtbl.replace tbl id ()) ids;
      let oc = open_out_gen [ Open_append; Open_wronly ] 0o600 path in
      Ok
        {
          oc;
          path;
          rng = Random.State.make_self_init ();
          ids = tbl;
          sink;
          leaf = Some leaf;
        }
    with Sys_error msg | Unix.Unix_error (_, msg, _) -> Error msg
