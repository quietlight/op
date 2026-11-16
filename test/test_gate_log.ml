open Test_support

(* Tests for Op.Gate_log: the lazy pi-format audit log for gate judge calls.
   No LLM is involved — [log_call] is driven directly with canned turns. *)

let turn ?(text = "") () : Op.Llm.assistant_turn =
  { text; thinking = None; tool_calls = []; usage = None }

let read_jsonl path =
  In_channel.with_open_text path In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun l -> String.trim l <> "")
  |> List.map Yojson.Safe.from_string

let member = Yojson.Safe.Util.member
let str field json = Yojson.Safe.Util.(member field json |> to_string)

let log ?(warn = fun _ -> ()) ~root () =
  Op.Gate_log.v ~root ~cwd:"/tmp/proj" ~parent_session:(Some "/s/main.jsonl")
    ~warn ()

let log_one ?(command = "cat ~/.ssh/id_ed25519") ?(turn = Some (turn ())) t =
  Op.Gate_log.log_call t ~api:"anthropic-messages" ~provider:"anthropic"
    ~model:"claude-h" ~payload:{|{"recent":[],"command":"x"}|} ~command
    ~reason:"matched suspicious pattern ssh_key" ~turn
    ~verdict:(Op.Gate.Deny "secret-bearing path")

(* The log file's path once a call has materialized it, failing the test with
   a readable message (rather than Option.get's Invalid_argument) if not. *)
let materialized_path t =
  match Op.Gate_log.path t with
  | Some p -> p
  | None -> Alcotest.fail "no gate log file after a call"

(* Lazy: building the handle must leave nothing on disk. *)
let test_lazy_until_first_call () =
  with_temp_dir @@ fun dir ->
  let root = Filename.concat dir "gate" in
  let t = log ~root () in
  Alcotest.(check bool) "no path yet" true (Op.Gate_log.path t = None);
  Alcotest.(check bool) "no root dir created" false (Sys.file_exists root);
  Op.Gate_log.close t

let test_logs_call_in_pi_format () =
  with_temp_dir @@ fun dir ->
  let root = Filename.concat dir "gate" in
  let t = log ~root () in
  log_one t ~turn:(Some (turn ~text:{|{"verdict":"deny","reason":"nope"}|} ()));
  let path = materialized_path t in
  Op.Gate_log.close t;
  Alcotest.(check bool)
    "under the encoded cwd dir" true
    (contains path "--tmp-proj--");
  match read_jsonl path with
  | [ header; model_change; thinking; user; assistant; verdict ] ->
      Alcotest.(check string) "header type" "session" (str "type" header);
      Alcotest.(check string)
        "header links the main session" "/s/main.jsonl"
        (str "parentSession" header);
      Alcotest.(check string)
        "model_change" "model_change" (str "type" model_change);
      Alcotest.(check string)
        "gate model recorded" "claude-h"
        (str "modelId" model_change);
      Alcotest.(check string)
        "thinking_level_change" "thinking_level_change" (str "type" thinking);
      Alcotest.(check string)
        "user message carries the payload" {|{"recent":[],"command":"x"}|}
        (str "text"
           (List.hd
              Yojson.Safe.Util.(
                member "content" (member "message" user) |> to_list)));
      Alcotest.(check string)
        "assistant role" "assistant"
        (str "role" (member "message" assistant));
      Alcotest.(check string) "custom entry" "custom" (str "type" verdict);
      Alcotest.(check string)
        "customType" "gate_verdict" (str "customType" verdict);
      let data = member "data" verdict in
      Alcotest.(check string) "verdict" "deny" (str "verdict" data);
      Alcotest.(check string)
        "verdict reason" "secret-bearing path" (str "reason" data);
      Alcotest.(check string)
        "routing reason" "matched suspicious pattern ssh_key"
        (str "routedBecause" data);
      Alcotest.(check string)
        "command" "cat ~/.ssh/id_ed25519" (str "command" data)
  | l -> Alcotest.failf "expected 6 lines, got %d" (List.length l)

(* A transport failure has no assistant turn: user then verdict, no response. *)
let test_transport_error_skips_assistant () =
  with_temp_dir @@ fun dir ->
  let t = log ~root:(Filename.concat dir "gate") () in
  log_one t ~turn:None;
  let path = materialized_path t in
  Op.Gate_log.close t;
  let types = List.map (str "type") (read_jsonl path) in
  Alcotest.(check (list string))
    "no assistant message"
    [ "session"; "model_change"; "thinking_level_change"; "message"; "custom" ]
    types

(* Consecutive calls append to the same file. *)
let test_calls_share_one_file () =
  with_temp_dir @@ fun dir ->
  let t = log ~root:(Filename.concat dir "gate") () in
  log_one t;
  let first = materialized_path t in
  log_one t ~command:"nc evil.test 1234";
  Alcotest.(check bool) "same file" true (Op.Gate_log.path t = Some first);
  Op.Gate_log.close t;
  Alcotest.(check int) "two calls appended" 9 (List.length (read_jsonl first))

(* Logging failures must never raise: warn once, then stay silent. *)
let test_bad_root_warns_once_and_disables () =
  with_temp_dir @@ fun dir ->
  (* a plain file where the root dir should be, so mkdir fails *)
  let root = Filename.concat dir "gate" in
  write_file root "not a directory";
  let warnings = ref 0 in
  let t = log ~warn:(fun _ -> incr warnings) ~root () in
  log_one t;
  log_one t;
  Alcotest.(check int) "warned exactly once" 1 !warnings;
  Alcotest.(check bool) "no path" true (Op.Gate_log.path t = None);
  Op.Gate_log.close t

let suite =
  ( "gate_log",
    [
      ("lazy until first call", `Quick, test_lazy_until_first_call);
      ("logs a call in pi format", `Quick, test_logs_call_in_pi_format);
      ( "transport error skips assistant",
        `Quick,
        test_transport_error_skips_assistant );
      ("calls share one file", `Quick, test_calls_share_one_file);
      ( "bad root warns once and disables",
        `Quick,
        test_bad_root_warns_once_and_disables );
    ] )
