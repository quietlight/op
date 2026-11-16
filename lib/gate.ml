(* The permission gate. Pure path rules over a small sliding window of recent
   calls, with nu scripts delegated to the injected static AST gate; see
   gate.mli for the philosophy and residual gaps. The only I/O is the injected
   judge closure, reached via {!Gate_nu}'s layer-2 text check. *)

type verdict =
  | Allow
  | Warn of string
  | Deny of string
  | Fault of string
  | Code_red of string

type judge = window:string list -> command:string -> reason:string -> verdict
type nu_gate = window:string list -> script:string -> verdict

(* Internal config, deliberately not user-facing. *)
let window_size = 4 (* last N calls remembered *)

type entry = { summary : string; verdict : verdict; mutable reminded : bool }
(* [verdict] here is the call's *base* verdict (from the rules alone), never the
   window-combined one — so a reminder Warn injected by persistence does not
   itself keep persistence alive. [reminded] makes the reminder one-shot: set
   once a flagged call has been cited in a reminder, so clean results stop
   carrying the trailer while the entry (and its summary, which the judge's
   window still sees) stays in the window for escalation. *)

type t = {
  nu : nu_gate option;
      (* The nu tool's static AST gate, injected by main (built over a nu
         subprocess, with the judge already baked in for its layer-2 check).
         Absent -> a [nu] tool call fails closed. *)
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
   verdicts that say something about what the *agent* did. A Fault is the gate
   failing, not the agent being flagged: reminding a model that "a recent call
   was flagged" when the judge simply failed to answer teaches it to avoid a
   command that was never actually objected to. *)
let reason_of = function
  | Warn r | Deny r | Code_red r -> Some r
  | Fault _ | Allow -> None

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

let path_verdict ~cwd raw =
  let abs = if Filename.is_relative raw then Filename.concat cwd raw else raw in
  let segs = normalize_segments abs in
  if List.exists (fun s -> String.length s > 0 && s.[0] = '.') segs then
    Fmt.kstr (fun s -> Deny s) "path %s is protected (dot-file)" raw
  else Allow

(* --- shared with Gate_nu ---------------------------------------------- *)

(* Secret-bearing paths: a nu script could [open] them and read/write tools
   could otherwise touch the non-dot ones, so {!Gate_nu}'s layer-2 text check
   routes a script naming one to the judge. The history entry is deliberately
   broad ([.bash_history], [.zsh_history], [.psql_history], bare [.history]):
   pasted secrets end up in all of them, and a benign hit costs one judge call,
   not a block. [.config/gh] is gh's token store; the trailing \b keeps
   [.config/ghostty] and friends out. [.config/himalaya] is the mail config:
   it carries an IMAP/SMTP password inline ([password.raw]), and its
   [password.cmd] is an exec primitive the AST walk never sees, since himalaya
   runs it through [/bin/sh] itself. *)
let sensitive_path_re =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|(\.ssh\b|id_(rsa|dsa|ecdsa|ed25519)|\.aws/credentials|\.netrc\b|\.gnupg\b|\.env\b|\.op_global/agent/models|\.pem\b|\.kube/config|\.docker/config|\.npmrc\b|\.git-credentials\b|\.config/gh\b|\.pgpass\b|\.config/gcloud\b|\.config/himalaya\b|terraform\.tfstate|\.[a-z_]*history\b)|}

(* Commands the deterministic rules can't clear are adjudicated by the judge.
   With no judge configured we fail closed; a judge that raises is likewise
   treated as a Deny (its contract is never-raise, but we don't trust it to
   honour that). Shared with {!Gate_nu}, whose layer-2 text check routes here. *)
let adjudicate ~judge ~window ~command reason =
  match judge with
  | None ->
      Fmt.kstr
        (fun s -> Deny s)
        "%s and no gate judge is configured/reachable" reason
  | Some j -> (
      match j ~window ~command ~reason with
      | verdict -> verdict
      | exception exn -> Deny ("gate judge raised: " ^ Printexc.to_string exn))

(* --- dispatch + window ------------------------------------------------ *)

let str_arg args field =
  match Yojson.Safe.Util.member field args with
  | `String s -> Some s
  | _ -> None

(* The base verdict for a call, plus a one-line summary for the window/judge.
   Absent/malformed args pass through as Allow: the tool's own validation will
   produce the real error. *)
let base_verdict t (tc : Llm.tool_call) =
  match tc.name with
  (* [glob] is included so its root [path] can't enumerate a hidden directory
     (e.g. ~/.ssh); glob.ml already skips hidden dirs *during* traversal, but not
     when one is chosen as the root. A missing path defaults to cwd -> Allow. *)
  | "read" | "write" | "edit" | "glob" ->
      let path = str_arg tc.arguments "path" in
      let summary = Fmt.str "%s %s" tc.name (Option.value ~default:"?" path) in
      let v =
        match path with Some p -> path_verdict ~cwd:t.cwd p | None -> Allow
      in
      (summary, v)
  | "nu" ->
      let script = str_arg tc.arguments "script" in
      let summary =
        Fmt.kstr
          (Util.truncate ~max_bytes:120)
          "nu %s"
          (Option.value ~default:"?" script)
      in
      let verdict =
        match script with
        | None -> Allow
        | Some s -> (
            let window =
              Fmt.str "cwd: %s" t.cwd :: List.map (fun e -> e.summary) t.window
            in
            (* Absent nu gate -> fail closed: a nu call must clear its static
               AST gate first. *)
            match t.nu with
            | Some nu -> nu ~window ~script:s
            | None ->
                Deny
                  "the nu tool's static gate is not configured; nu calls are \
                   denied")
      in
      (summary, verdict)
  | other -> (other, Allow)

(* Combine the base verdict with the recent window:
   1. escalation: >= 2 denies already in the window and this call also denied
      -> the agent is pushing on the gate; stop it.
   2. one-shot reminder: the first allowed call after a flagged one cites the
      newest un-reminded flag, then marks every pending flag reminded — one
      reminder per flagging burst, not a sticky trailer on every clean result.
      A later flagged call re-arms it. The entries themselves stay in the
      window untouched, so escalation and the judge's window are unaffected. *)
let combine ~recent ~base =
  let denies, faults =
    List.fold_left
      (fun (d, f) e ->
        match e.verdict with
        | Deny _ -> (d + 1, f)
        | Fault _ -> (d, f + 1)
        | Allow | Warn _ | Code_red _ -> (d, f))
      (0, 0) recent
  in
  match base with
  | Deny _ when denies >= 2 -> Code_red "repeatedly attempting blocked actions"
  (* Faults are counted separately and never mixed into [denies]. Escalation
     exists to stop an agent leaning on the gate, and a Fault is not evidence
     about the agent at all — it is the gate failing to decide. Counting the
     two together is what once killed a healthy run: one genuine Deny, one
     malformed judge reply, one more Deny, and the third call died as
     "repeatedly attempting blocked actions" when only two of the three were
     the agent's doing.
     A fault still needs its own bound, though. If the gate cannot decide three
     times running, continuing means running unadjudicated commands, so the run
     stops — under a message that names the gate as the thing that broke, so the
     next person reads the judge's log rather than the model's transcript. *)
  | Fault _ when faults >= 2 ->
      Code_red
        "the permission gate could not reach a decision three times running"
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
  let summary, base = base_verdict t tc in
  let final = combine ~recent ~base in
  t.window <-
    keep_last window_size
      (t.window @ [ { summary; verdict = base; reminded = false } ]);
  final
