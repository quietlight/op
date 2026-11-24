let preview_lines = 5
let error_preview_lines = 15
let nu_preview_lines = 10
let script_lines = 15
let thinking_lines = 5
let value_chars = 60
let edit_chars = 40
let line_chars = 200
let indent = "    "

type t = {
  color : bool;
  echo_user : bool;
  calls : (string, string * Yojson.Safe.t) Hashtbl.t;
  mutable cwd : string option;
}

let v ~color ?cwd ?(echo_user = false) () =
  { color; echo_user; calls = Hashtbl.create 64; cwd }

(* Total JSON accessors so partial or foreign entries never raise. *)
let mem name json =
  match json with `Assoc _ -> Yojson.Safe.Util.member name json | _ -> `Null

let str name json = match mem name json with `String s -> Some s | _ -> None
let int name json = match mem name json with `Int n -> Some n | _ -> None
let bool name json = match mem name json with `Bool b -> b | _ -> false
let str_or name ~default json = Option.value (str name json) ~default

let num name json =
  match mem name json with
  | `Int n -> Some (float_of_int n)
  | `Float f -> Some f
  | _ -> None

let lines s =
  match String.split_on_char '\n' s with
  | [ "" ] -> []
  | parts -> (
      match List.rev parts with "" :: rest -> List.rev rest | _ -> parts)

(* Scrub terminal control bytes before styling so untrusted text cannot emit
   escape sequences or overwrite lines via CR. *)
let is_control c = (c < ' ' && c <> '\n' && c <> '\t') || c = '\127'

let scrub_string s =
  if not (String.exists is_control s) then s
  else begin
    let b = Buffer.create (String.length s) in
    String.iter
      (fun c ->
        if c <> '\r' then Buffer.add_char b (if is_control c then '?' else c))
      s;
    Buffer.contents b
  end

let rec scrub : Yojson.Safe.t -> Yojson.Safe.t = function
  | `String s -> `String (scrub_string s)
  | `List l -> `List (List.map scrub l)
  | `Assoc kv -> `Assoc (List.map (fun (k, v) -> (k, scrub v)) kv)
  | j -> j

let clip n s =
  if String.length s <= n then s
  else Util.drop_partial_utf8_tail (String.sub s 0 n) ^ "…"

let flatten s = String.concat " " (String.split_on_char '\n' s) |> String.trim
let text_of = function `String s -> s | v -> Yojson.Safe.to_string v

let block_text content =
  match content with
  | `List blocks ->
      List.filter_map
        (fun b -> if str "type" b = Some "text" then str "text" b else None)
        blocks
      |> String.concat "\n"
  | `String s -> s
  | _ -> ""

(* Use literal "/" so session paths render consistently across host OSes. *)
let rel t path =
  match t.cwd with
  | Some cwd when cwd <> "" && String.starts_with ~prefix:(cwd ^ "/") path ->
      let n = String.length cwd + 1 in
      String.sub path n (String.length path - n)
  | _ -> path

let human_bytes n =
  let k = 1024. in
  let f = float_of_int n in
  if n < 1024 then Fmt.str "%d B" n
  else if n < 1024 * 1024 then Fmt.str "%.1f kB" (f /. k)
  else Fmt.str "%.1f MB" (f /. k /. k)

let sty t code s =
  if t.color && s <> "" then Fmt.str "\027[%sm%s\027[0m" code s else s

let dim t = sty t "2"
let red t = sty t "31"
let green t = sty t "32"
let yellow t = sty t "33"
let magenta t = sty t "35"

let read_range args =
  match (int "offset" args, int "limit" args) with
  | None, None -> ""
  | offset, limit -> (
      let offset = Option.value offset ~default:1 in
      match limit with
      | Some limit -> Fmt.str "  lines %d-%d" offset (offset + max 1 limit - 1)
      | None -> Fmt.str "  from line %d" offset)

let edit_summary t args =
  let snippet name = clip edit_chars (flatten (str_or name ~default:"" args)) in
  let all = if bool "replace_all" args then dim t "  (all)" else "" in
  Fmt.str "%s  %s → %s%s"
    (rel t (str_or "path" ~default:"?" args))
    (snippet "old_string") (snippet "new_string") all

let generic_summary args =
  match args with
  | `Assoc kvs ->
      List.map
        (fun (k, v) ->
          Fmt.str "%s=%s" k (clip value_chars (flatten (text_of v))))
        kvs
      |> String.concat ", "
  | `Null -> ""
  | v -> flatten (Yojson.Safe.to_string v)

let script_block t args =
  let ls = lines (str_or "script" ~default:"" args) in
  let n = List.length ls in
  List.filteri (fun i _ -> i < script_lines) ls
  |> List.map (fun l -> indent ^ dim t (clip line_chars l))
  |> fun shown ->
  if n > script_lines then
    shown @ [ dim t (Fmt.str "%s…  %d lines total" indent n) ]
  else shown

(* Pad the tool-name column for alignment, then trim trailing spaces when [rest]
   is empty. *)
let head t arrow name rest =
  let s = Fmt.str "  %s %-5s %s" (magenta t arrow) name rest in
  let rec last i = if i > 0 && s.[i - 1] = ' ' then last (i - 1) else i in
  String.sub s 0 (last (String.length s))

let call_head t name rest = head t "→" name rest

let call_lines t name args =
  let path () = rel t (str_or "path" ~default:"?" args) in
  match name with
  | "read" -> [ call_head t name (path () ^ read_range args) ]
  | "glob" ->
      (* Kept for replaying older sessions created before [glob] was removed. *)
      let where =
        match str "path" args with
        | Some p when Some p <> t.cwd -> "  in " ^ rel t p
        | _ -> ""
      in
      [ call_head t name (str_or "pattern" ~default:"?" args ^ where) ]
  | "write" ->
      let bytes = String.length (str_or "content" ~default:"" args) in
      [ call_head t name (Fmt.str "%s  (%s)" (path ()) (human_bytes bytes)) ]
  | "edit" -> [ call_head t name (edit_summary t args) ]
  | "nu" ->
      let timeout =
        match int "timeout" args with
        | Some s when s <> 120 -> Fmt.str "timeout %ds" s
        | _ -> ""
      in
      call_head t name timeout :: script_block t args
  | name -> [ call_head t name (generic_summary args) ]

let result_head t name summary = head t "←" name summary

let preview t ~limit text =
  let ls = lines text in
  let n = List.length ls in
  let shown =
    List.filteri (fun i _ -> i < limit) ls
    |> List.map (fun l -> indent ^ dim t (clip line_chars l))
  in
  if n > limit then shown @ [ dim t (Fmt.str "%s…  %d lines total" indent n) ]
  else shown

(* Match the "(exit code: N)" footer appended by [Nu.tool]. *)
let exit_prefix = "(exit code: "

let exit_code_of line =
  let n = String.length exit_prefix in
  if
    String.length line > n
    && String.starts_with ~prefix:exit_prefix line
    && String.ends_with ~suffix:")" line
  then
    match String.sub line n (String.length line - n - 1) with
    | "" -> None
    | code -> Some code
  else None

let split_exit text =
  match List.rev (lines text) with
  | last :: rest -> (
      match exit_code_of last with
      | Some code -> (Fmt.str "exit %s" code, String.concat "\n" (List.rev rest))
      | None -> ("done", text))
  | [] -> ("done", text)

let result_summary t ~name ~args text =
  let n = List.length (lines text) in
  match name with
  | "read" -> (
      match str "path" args with
      | None -> (Fmt.str "%d lines" n, preview_lines)
      | Some path -> (Fmt.str "%s  %d lines" (rel t path) n, preview_lines))
  | "glob" -> (Fmt.str "%d paths" n, preview_lines)
  | "write" | "edit" -> (flatten text, 0)
  | "nu" -> (fst (split_exit text), nu_preview_lines)
  | _ -> (Fmt.str "%d lines" n, preview_lines)

let body_of t ~name text =
  match name with
  | "nu" -> snd (split_exit text)
  | "glob" -> String.concat "\n" (List.map (rel t) (lines text))
  | _ -> text

let tool_result t m =
  let name = str_or "toolName" ~default:"?" m in
  let text = block_text (mem "content" m) in
  let args =
    match str "toolCallId" m with
    | Some id -> (
        match Hashtbl.find_opt t.calls id with
        | Some (_, a) -> a
        | None -> `Null)
    | None -> `Null
  in
  if bool "isError" m then
    let tag =
      match name with
      | "nu" -> Fmt.str "%s %s" (red t "[error]") (fst (split_exit text))
      | _ -> red t "[error]"
    in
    result_head t name tag
    :: preview t ~limit:error_preview_lines (body_of t ~name text)
  else
    let summary, limit = result_summary t ~name ~args text in
    let body = body_of t ~name text in
    result_head t name summary
    :: (if limit = 0 then [] else preview t ~limit body)

let thinking_block t b =
  match str "thinking" b with
  | None -> []
  | Some text ->
      let ls = lines text in
      let n = List.length ls in
      List.filteri (fun i _ -> i < thinking_lines) ls
      |> List.map (fun l -> "  " ^ dim t (clip line_chars l))
      |> fun shown ->
      if n > thinking_lines then
        shown @ [ dim t (Fmt.str "  …  %d lines of thinking" n) ]
      else shown

let assistant_header t m =
  let out = Option.value (int "output" (mem "usage" m)) ~default:0 in
  Fmt.str "%s %s"
    (sty t "1;34" "### ASSISTANT")
    (sty t "34"
       (Fmt.str "[%s]  out=%d tok" (str_or "model" ~default:"?" m) out))

let remember t b =
  let name = str_or "name" ~default:"?" b in
  Option.iter
    (fun id -> Hashtbl.replace t.calls id (name, mem "arguments" b))
    (str "id" b);
  call_lines t name (mem "arguments" b)

let assistant t m =
  let blocks = match mem "content" m with `List l -> l | _ -> [] in
  let of_kind kind f =
    List.concat_map
      (fun b -> if str "type" b = Some kind then f b else [])
      blocks
  in
  let text = block_text (mem "content" m) in
  let body = if String.trim text = "" then [] else [ text ] in
  ("" :: assistant_header t m :: of_kind "thinking" (thinking_block t))
  @ body
  @ of_kind "toolCall" (remember t)

let message t m =
  match str "role" m with
  | Some "user" when t.echo_user ->
      [ ""; sty t "1;36" "### USER"; block_text (mem "content" m) ]
  | Some "assistant" -> assistant t m
  | Some "toolResult" -> tool_result t m
  | _ -> []

let session_header t e =
  t.cwd <- str "cwd" e;
  [
    sty t "1;32" (Fmt.str "══ session %s ══" (str_or "id" ~default:"?" e));
    Fmt.str "%s %s" (green t "cwd: ") (str_or "cwd" ~default:"?" e);
    Fmt.str "%s %s" (green t "time:") (str_or "timestamp" ~default:"?" e);
  ]

let model_change t e =
  [
    yellow t
      (Fmt.str "[model → %s/%s]"
         (str_or "provider" ~default:"?" e)
         (str_or "modelId" ~default:"?" e));
  ]

let entry t json =
  let json = scrub json in
  match str "type" json with
  | Some "session" -> session_header t json
  | Some "model_change" -> model_change t json
  | Some "message" -> message t (mem "message" json)
  | _ -> []

let rule t = dim t (String.concat "" (List.init 56 (fun _ -> "─")))

let compact_tokens n =
  let f = float_of_int n in
  if n < 1000 then string_of_int n
  else if n < 1_000_000 then Fmt.str "%.1fk" (f /. 1000.)
  else Fmt.str "%.1fM" (f /. 1_000_000.)

let gauge_color pct =
  if pct < 0.6 then green else if pct < 0.85 then yellow else red

let context_line t env =
  match mem "context" env with
  | `Assoc _ as c -> (
      match (int "tokens" c, int "window" c, num "pct" c) with
      | Some tokens, Some window, Some pct ->
          let filled =
            max 0 (min 10 (int_of_float (Float.round (pct *. 10.))))
          in
          let bar =
            Fmt.str "[%s%s]" (String.make filled '=')
              (String.make (10 - filled) '-')
          in
          [
            Fmt.str "context: %s / %s  %s  %.0f%%" (compact_tokens tokens)
              (compact_tokens window) (gauge_color pct t bar) (pct *. 100.);
          ]
      | _ -> [])
  | _ -> []

let tokens_line t env =
  match mem "tokens" env with
  | `Assoc _ as u ->
      let get name = Option.value (int name u) ~default:0 in
      [
        dim t
          (Run_report.tokens_line
             {
               Llm.input = get "input";
               output = get "output";
               cache_read = get "cacheRead";
               cache_creation = get "cacheWrite";
             });
      ]
  | _ -> []

let resume_line t env =
  match (str "sessionId" env, str "parentId" env) with
  | Some id, Some parent ->
      [
        dim t
          (Fmt.str "resume: op --sessionId %s --parentId %s \"prompt\"" id
             parent);
      ]
  | Some id, None -> [ dim t (Fmt.str "session: %s" id) ]
  | None, _ -> []

let plural n word = Fmt.str "%d %s%s" n word (if n = 1 then "" else "s")

let outcome_line t env =
  let stop = str_or "stopReason" ~default:"?" env in
  let count name = Option.value (int name env) ~default:0 in
  let errors = count "toolErrors" in
  let errors =
    if errors = 0 then "" else "  ·  " ^ red t (plural errors "tool error")
  in
  Fmt.str "%s  ·  %s  ·  %s%s"
    ((if stop = "finished" then green t else red t) stop)
    (plural (count "turns") "turn")
    (plural (count "toolCalls") "tool call")
    errors

let footer t env =
  let env = scrub env in
  match str "error" env with
  | Some msg ->
      let hint =
        match str "hint" env with
        | Some h -> [ dim t ("hint: " ^ h) ]
        | None -> []
      in
      (red t ("error: " ^ msg) :: hint) @ resume_line t env
  | None ->
      ("" :: rule t :: outcome_line t env :: tokens_line t env)
      @ context_line t env @ resume_line t env

let line t text =
  match Yojson.Safe.from_string text with
  | exception Yojson.Json_error _ -> []
  | json when str "type" json = Some "result" -> footer t json
  | json -> entry t json
