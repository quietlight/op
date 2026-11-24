type verdict =
  | Allow
  | Warn of string
  | Deny of string
  | Unparsed of string
  | Code_red of string

type nu_gate = script:string -> verdict

let window_size = 4

(* [verdict] is the rule-only base verdict (never window-derived) so reminders
   do not perpetuate themselves. [reminded] makes reminders one-shot while
   keeping the entry in the window for escalation. *)
type entry = { verdict : verdict; mutable reminded : bool }
type t = { nu : nu_gate option; cwd : string; mutable window : entry list }

let v ?nu ~cwd () = { nu; cwd; window = [] }

let keep_last n xs =
  let len = List.length xs in
  if len <= n then xs
  else
    let rec drop k = function
      | _ :: tl when k > 0 -> drop (k - 1) tl
      | l -> l
    in
    drop (len - n) xs

(* Exclude [Unparsed] from reminders: syntax slips are not policy violations. *)
let reason_of = function
  | Warn r | Deny r | Code_red r -> Some r
  | Unparsed _ | Allow -> None

(* Textual path normalization (handles '\\' and drive letters on every host so
   verdicts are platform-independent; no realpath). *)
let normalize_segments abs =
  let abs = String.map (function '\\' -> '/' | c -> c) abs in
  let abs =
    if String.length abs >= 2 && abs.[1] = ':' then
      String.sub abs 2 (String.length abs - 2)
    else abs
  in
  String.split_on_char '/' abs
  |> List.fold_left
       (fun acc seg ->
         match seg with
         | "" | "." -> acc
         | ".." -> ( match acc with _ :: tl -> tl | [] -> [])
         | s -> s :: acc)
       []
  |> List.rev

(* Secret-bearing paths denied in both file tools and [Gate_nu] Layer 2.
   Includes: [/proc] (exposes op's inherited env via /proc/*/environ; anchored
   so paths like /mnt/x/proc/ pass), all of [~/.op_global] (keys + sessions),
   shell/db [*history] files, [.config/gh] (word boundary avoids ghostty), and
   [.config/himalaya] (inline passwords + password.cmd shell exec). *)
let sensitive_path_re =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|(\.ssh\b|id_(rsa|dsa|ecdsa|ed25519)|\.aws/credentials|\.netrc\b|\.gnupg\b|\.env\b|\.op_global\b|\.pem\b|\.p12\b|\.pfx\b|\.kube/config|\.docker/config|\.npmrc\b|\.pypirc\b|\.cargo/credentials|\.git-credentials\b|\.config/gh\b|\.pgpass\b|\.config/gcloud\b|\.config/himalaya\b|\.config/typesafe\b|\.config/rclone\b|\.cache/huggingface\b|\.kaggle\b|\.azure\b|\.vault-token\b|\.local/share/keyrings\b|terraform\.tfstate|\.[a-z_]*history\b|(^|[^\w.~/$)-])/proc\b)|}

let path_verdict ~cwd raw =
  let abs = if Filename.is_relative raw then Filename.concat cwd raw else raw in
  let segs = normalize_segments abs in
  if List.exists (fun s -> String.length s > 0 && s.[0] = '.') segs then
    Fmt.kstr (fun s -> Deny s) "path %s is protected (dot-file)" raw
  else
    match Re.exec_opt sensitive_path_re ("/" ^ String.concat "/" segs) with
    | Some g ->
        Fmt.kstr
          (fun s -> Deny s)
          "path %s is protected (secret-bearing: `%s`)" raw
          (String.trim (Re.Group.get g 0))
    | None -> Allow

let str_arg args field =
  match Yojson.Safe.Util.member field args with
  | `String s -> Some s
  | _ -> None

(* Absent/malformed args pass as [Allow] so the tool emits its own error. *)
let base_verdict t (tc : Llm.tool_call) =
  match tc.name with
  | "read" | "write" | "edit" -> (
      match str_arg tc.arguments "path" with
      | Some p -> path_verdict ~cwd:t.cwd p
      | None -> Allow)
  | "nu" -> (
      match (str_arg tc.arguments "script", t.nu) with
      | None, _ -> Allow
      | Some script, Some nu -> nu ~script
      | Some _, None ->
          Deny
            "the nu tool's static gate is not configured; nu calls are denied")
  | _ -> Allow

(* Escalate to [Code_red] on the 3rd [Deny] in the window (ignoring [Unparsed]);
   on [Allow], emit a one-shot reminder for the newest un-reminded flag. *)
let combine ~recent ~base =
  let denies =
    List.length
      (List.filter
         (fun e -> match e.verdict with Deny _ -> true | _ -> false)
         recent)
  in
  match base with
  | Deny _ when denies >= 2 -> Code_red "repeatedly attempting blocked actions"
  | Allow -> (
      let pending e = if e.reminded then None else reason_of e.verdict in
      match List.rev recent |> List.find_map pending with
      | Some r ->
          List.iter
            (fun e -> if reason_of e.verdict <> None then e.reminded <- true)
            recent;
          Warn ("reminder: a recent call was flagged: " ^ r)
      | None -> Allow)
  | v -> v

let check t (tc : Llm.tool_call) =
  let recent = keep_last (window_size - 1) t.window in
  let base = base_verdict t tc in
  let final = combine ~recent ~base in
  t.window <-
    keep_last window_size (t.window @ [ { verdict = base; reminded = false } ]);
  final
