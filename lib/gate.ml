(* The permission gate. Pure path rules over a small sliding window of recent
   calls, with nu scripts delegated to the injected static AST gate; see
   gate.mli for the philosophy and residual gaps. No I/O of its own. *)

type verdict =
  | Allow
  | Warn of string
  | Deny of string
  | Unparsed of string
  | Code_red of string

type nu_gate = script:string -> verdict

(* Internal config, deliberately not user-facing. *)
let window_size = 4 (* last N calls remembered *)

type entry = { verdict : verdict; mutable reminded : bool }
(* [verdict] here is the call's *base* verdict (from the rules alone), never the
   window-combined one — so a reminder Warn injected by persistence does not
   itself keep persistence alive. [reminded] makes the reminder one-shot: set
   once a flagged call has been cited in a reminder, so clean results stop
   carrying the trailer while the entry stays in the window for escalation. *)

type t = {
  nu : nu_gate option;
      (* The nu tool's static AST gate, injected by main (built over a nu
         subprocess). Absent -> a [nu] tool call fails closed. *)
  cwd : string;
  mutable window : entry list;
}

let v ?nu ~cwd () = { nu; cwd; window = [] }

(* Keep the last [n] elements of [xs] (order preserved). *)
let keep_last n xs =
  let len = List.length xs in
  if len <= n then xs
  else
    let rec drop k = function
      | _ :: tl when k > 0 -> drop (k - 1) tl
      | l -> l
    in
    drop (len - n) xs

(* The reason to cite back to the model in a one-shot reminder, and so only for
   verdicts that say the agent reached for something it may not have. An
   [Unparsed] script is a typo, already explained in its own result; reminding
   the model that "a recent call was flagged" teaches it to avoid a command
   nobody objected to. *)
let reason_of = function
  | Warn r | Deny r | Code_red r -> Some r
  | Unparsed _ | Allow -> None

(* --- paths ------------------------------------------------------------ *)

(* Textual normalization: treat '\\' as '/', drop a leading drive letter
   ("C:"), then split, drop '.'/empty segments and resolve '..' against
   preceding components. No realpath (symlink traversal is a documented v1
   gap). Returns the resolved segments of the absolute path. Windows shapes are
   handled on every host so a path's verdict never depends on the platform. *)
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

(* --- shared with Gate_nu ---------------------------------------------- *)

(* Secret-bearing paths: a nu script could [open] them and the file tools could
   otherwise touch the non-dot ones ([server.pem], [terraform.tfstate]), so
   {!path_verdict} denies a file-tool path matching one and {!Gate_nu}'s
   layer-2 text check denies a script naming one. [/proc] is here because
   [/proc/self/environ] (or the parent's, from nu) is op's whole inherited
   environment, API keys included; it must not follow a path character, which
   keeps a data path like [/mnt/x/proc/] out while catching it after a quote,
   bracket, brace or colon. All of [~/.op_global] is here, not only the agent
   config: the sessions hold every past run's prompts, file contents and tool
   output, and a nu read would send them to the current provider. A nested
   [op --list]/[--handoff] (the [op] external) is the deliberate way in. The history entry is deliberately broad
   ([.bash_history], [.zsh_history], [.psql_history], bare [.history]): pasted
   secrets end up in all of them. [.config/gh] is gh's token store; the trailing \b keeps
   [.config/ghostty] and friends out. [.config/himalaya] is the mail config:
   it carries an IMAP/SMTP password inline ([password.raw]), and its
   [password.cmd] is an exec primitive the AST walk never sees, since himalaya
   runs it through [/bin/sh] itself. *)
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

(* --- dispatch + window ------------------------------------------------ *)

let str_arg args field =
  match Yojson.Safe.Util.member field args with
  | `String s -> Some s
  | _ -> None

(* The base verdict for a call. Absent/malformed args pass through as Allow:
   the tool's own validation will produce the real error. *)
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
      (* Absent nu gate -> fail closed: a nu call must clear its static AST
         gate first. *)
      | Some _, None ->
          Deny
            "the nu tool's static gate is not configured; nu calls are denied")
  | _ -> Allow

(* Combine the base verdict with the recent window:
   1. escalation: >= 2 denies already in the window and this call also denied
      -> the agent is pushing on the gate; stop it.
   2. one-shot reminder: the first allowed call after a flagged one cites the
      newest un-reminded flag, then marks every pending flag reminded — one
      reminder per flagging burst, not a sticky trailer on every clean result.
      A later flagged call re-arms it. The entries themselves stay in the
      window untouched, so escalation is unaffected. *)
let combine ~recent ~base =
  let denies =
    List.length
      (List.filter
         (fun e -> match e.verdict with Deny _ -> true | _ -> false)
         recent)
  in
  match base with
  (* Only [Deny] is counted, on both sides. Escalation exists to stop an agent
     leaning on the gate; a script that would not parse is a syntax slip, and
     in the session history about half the runs this rule ended had one of
     those among their three strikes. *)
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
