let ( let* ) = Result.bind

type app_error = { msg : string; hint : string option }

let app_error_of_config e =
  let msg, hint = Op.Config.json_error e in
  { msg; hint }

let list_fail ?(text_prefix = "error: ") ?hint ~json msg =
  if json then print_endline (Op.List_report.error ?hint msg)
  else prerr_endline (text_prefix ^ msg);
  exit 1

let list_fail_config ~json e =
  let msg, hint = Op.Config.json_error e in
  if json then list_fail ?hint ~json msg
  else list_fail ~json (Op.Config.pp_error e)

(* [Sys.getcwd] raises [Sys_error] when the process's working directory has been
   removed; route that through [list_fail] so --list still honours its "one JSON
   object on stdout, exit 1" contract instead of leaking a backtrace. *)
let cwd_or_fail ~json () =
  try Sys.getcwd ()
  with Sys_error msg ->
    Fmt.kstr
      (fun s -> list_fail ~json s)
      "cannot determine working directory: %s" msg

(* [tail n xs] keeps the last [n] rows (every --list view orders oldest/first so
   the tail is the freshest, most relevant slice) and reports how many were
   dropped off the front, so the caller can nudge about the hidden rows. *)
let tail n xs =
  let len = List.length xs in
  if len <= n then (xs, 0) else (List.drop (len - n) xs, len - n)

(* When --limit hid rows, say so on stderr (a diagnostic, not data — keeps stdout a
   clean list for pipes) so --list never truncates silently. The --json path
   returns the capped array and silences its own stderr, so skip it there. *)
let truncated_hint ~json ~omitted =
  if (not json) && omitted > 0 then
    Fmt.epr "… %d more; use --limit N to show more\n" omitted

let floor_div a b =
  let q = a / b and r = a mod b in
  if r <> 0 && r < 0 <> (b < 0) then q - 1 else q

let utc_epoch_seconds ~year ~month ~day ~hour ~minute ~second =
  let y = if month <= 2 then year - 1 else year in
  let era = floor_div y 400 in
  let yoe = y - (era * 400) in
  let month_prime = month + if month > 2 then -3 else 9 in
  let doy = (((153 * month_prime) + 2) / 5) + day - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  let days = (era * 146097) + doe - 719468 in
  float_of_int ((days * 86400) + (hour * 3600) + (minute * 60) + second)

let human_time iso =
  try
    Scanf.sscanf iso "%d-%d-%dT%d:%d:%d" (fun y mo d h mi s ->
        let lt =
          Unix.localtime
            (utc_epoch_seconds ~year:y ~month:mo ~day:d ~hour:h ~minute:mi
               ~second:s)
        in
        Fmt.str "%02d-%02d %02d:%02d" (lt.tm_mon + 1) lt.tm_mday lt.tm_hour
          lt.tm_min)
  with Scanf.Scan_failure _ | End_of_file | Failure _ -> iso

let list_sessions ~json limit =
  let cwd = cwd_or_fail ~json () in
  let result =
    let* root = Op.Session.default_root () in
    let dir = Op.Session.dir_for ~root ~cwd in
    Op.Session_list.list_sessions ~dir ()
  in
  match result with
  | Error e -> list_fail ~json e
  | Ok all -> (
      let sessions, omitted = tail limit all in
      match (json, sessions) with
      | true, _ -> print_endline (Op.List_report.sessions sessions)
      | false, [] -> Fmt.epr "no sessions for %s\n" cwd
      | false, sessions ->
          let tty = Unix.isatty Unix.stdout in
          if tty then
            Fmt.pr "%-36s  %-11s  %4s  %s\n" "SESSION ID" "WHEN" "MSGS"
              "SUMMARY";
          List.iter
            (fun (s : Op.Session_list.summary) ->
              Fmt.pr "%-36s  %-11s  %4d  %s\n" s.id (human_time s.timestamp)
                s.count s.summary)
            sessions;
          truncated_hint ~json ~omitted;
          if tty then
            Fmt.pr
              "\n\n\
               resume at tail: op --sessionId <id>\n\
               messages: op --list messages --sessionId <id>\n")

let list_messages ~json session limit =
  let cwd = cwd_or_fail ~json () in
  let result =
    let* root = Op.Session.default_root () in
    let dir = Op.Session.dir_for ~root ~cwd in
    let* path = Op.Session_read.file_for_session ~dir ~session_id:session in
    Op.Session_list.list_entries ~path ()
  in
  match result with
  | Error e -> list_fail ~json e
  | Ok all -> (
      let entries, omitted = tail limit all in
      match json with
      | true ->
          print_endline (Op.List_report.messages ~session_id:session entries)
      | false ->
          let tty = Unix.isatty Unix.stdout in
          if tty then Fmt.pr "%-2s %-8s %-9s  %s\n" "" "ENTRY" "ROLE" "MESSAGE";
          List.iter
            (fun (e : Op.Session_list.entry_summary) ->
              let mark = if e.is_fork then "*" else " " in
              Fmt.pr "%-2s %-8s %-9s  %s\n" mark e.id e.role e.preview)
            entries;
          truncated_hint ~json ~omitted;
          if tty then
            Fmt.pr
              "\n\n\
               * = branch point   branch: op --sessionId %s --parentId <entry>\n"
              session)

(* One row per branch tip: the ids [--parentId] takes to *continue* a branch, as
   opposed to --list messages' entry ids, which fork mid-thread. Tips are the
   only way to name a branch whose last entry is a tool result — the shape a run
   killed mid-turn leaves behind — since those never appear in --list messages. *)
let list_branches ~json session limit =
  let cwd = cwd_or_fail ~json () in
  let result =
    let* root = Op.Session.default_root () in
    let dir = Op.Session.dir_for ~root ~cwd in
    let* path = Op.Session_read.file_for_session ~dir ~session_id:session in
    Op.Session_list.list_branches ~path ()
  in
  match result with
  | Error e -> list_fail ~json e
  | Ok all -> (
      let branches, omitted = tail limit all in
      match json with
      | true ->
          print_endline (Op.List_report.branches ~session_id:session branches)
      | false ->
          let tty = Unix.isatty Unix.stdout in
          if tty then Fmt.pr "%-8s %-11s %-9s  %s\n" "TIP" "WHEN" "ROLE" "TAIL";
          List.iter
            (fun (b : Op.Session_list.branch_tip) ->
              Fmt.pr "%-8s %-11s %-9s  %s\n" b.tip (human_time b.timestamp)
                b.role b.preview)
            branches;
          truncated_hint ~json ~omitted;
          if tty then
            Fmt.pr "\n\ncontinue: op --sessionId %s --parentId <tip>\n" session)

let list_models ~json limit =
  match Op.Config.load_config () with
  | Error e -> list_fail_config ~json e
  | Ok config ->
      let pairs, omitted =
        Op.Config.providers config
        |> List.concat_map (fun (p : Op.Config.provider) ->
            List.map (fun (m : Op.Config.model) -> (p.name, m.id)) p.models)
        |> tail limit
      in
      if json then print_endline (Op.List_report.models pairs)
      else List.iter (fun (prov, model) -> Fmt.pr "%s/%s\n" prov model) pairs;
      truncated_hint ~json ~omitted

let no_default_msg =
  "no default model configured; set one with op --set default PROVIDER/MODEL"

let list_default ~json () =
  match (json, Op.Config.load_config ()) with
  | _, Error e -> list_fail_config ~json e
  | true, Ok config -> (
      match
        (Op.Config.default_provider config, Op.Config.default_model config)
      with
      | Some p, Some m ->
          print_endline (Op.List_report.default ~provider:p ~model:m)
      | _ -> list_fail ~text_prefix:"" ~json no_default_msg)
  | false, Ok config -> (
      match
        (Op.Config.default_provider config, Op.Config.default_model config)
      with
      | Some p, Some m -> Fmt.pr "%s/%s\n" p m
      | _ -> list_fail ~text_prefix:"" ~json no_default_msg)

let set_default ~json spec =
  let resolved =
    let* spec =
      Option.to_result spec
        ~none:
          {
            msg = "--set default requires a PROVIDER/MODEL argument";
            hint = None;
          }
    in
    let* config =
      Op.Config.load_config () |> Result.map_error app_error_of_config
    in
    Op.Config.resolve_default_spec config spec
    |> Result.map_error (function
      | `Missing_provider _ ->
          {
            msg = "--set default needs an explicit provider: PROVIDER/MODEL";
            hint = None;
          }
      | `Unknown_provider name ->
          { msg = Fmt.str "unknown provider '%s'" name; hint = None }
      | `Unknown_model (provider, model) ->
          {
            msg =
              Fmt.str "provider '%s' has no model '%s' (run op --list models)"
                provider model;
            hint = None;
          })
  in
  match resolved with
  | Error e -> list_fail ~text_prefix:"" ?hint:e.hint ~json e.msg
  | Ok (provider, model) -> (
      match Op.Config.save_default ~provider ~model with
      | Error e -> list_fail ~json (Op.Config.pp_error e)
      | Ok () ->
          if json then print_endline (Op.List_report.default ~provider ~model)
          else Fmt.pr "%s/%s\n" provider model)

let run_list ~json ~limit ~session_id = function
  | Cli.Sessions -> list_sessions ~json limit
  | Messages -> (
      match session_id with
      | Some id -> list_messages ~json id limit
      | None ->
          list_fail ~text_prefix:"" ~json
            "--list messages requires --sessionId ID")
  | Branches -> (
      match session_id with
      | Some id -> list_branches ~json id limit
      | None ->
          list_fail ~text_prefix:"" ~json
            "--list branches requires --sessionId ID")
  | Models -> list_models ~json limit
  | Default -> list_default ~json ()
