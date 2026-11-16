(* Pi-format audit log for gate judge calls (see gate_log.mli). Lazy: the file
   is created on the first logged call, so the common zero-judge-call run
   leaves nothing behind. All failures are swallowed — logging must never
   change a verdict or break a run. *)

type state =
  | Pending  (** nothing on disk yet *)
  | Open of Session.t
  | Disabled  (** a failure was warned about; stay silent from here on *)

type t = {
  root : (string, string) result;
  cwd : string;
  parent_session : string option;
  warn : string -> unit;
  mutable state : state;
}

let default_root () =
  match Util.home_dir () with
  | Some home -> Ok (Filename.concat home ".op_global/gate")
  | None -> Error "HOME environment variable not set"

let v ?root ~cwd ~parent_session ~warn () =
  let root = match root with Some r -> Ok r | None -> default_root () in
  { root; cwd; parent_session; warn; state = Pending }

(* Materialize the session file on first use. [provider]/[model] come from the
   first call — the gate model is fixed for the run, so they never change. *)
let session t ~provider ~model =
  match t.state with
  | Open s -> Some s
  | Disabled -> None
  | Pending -> (
      let created =
        let ( let* ) = Result.bind in
        let* root = t.root in
        let dir = Session.dir_for ~root ~cwd:t.cwd in
        Session.v ?parent_session:t.parent_session ~dir ~cwd:t.cwd ~provider
          ~model ()
      in
      match created with
      | Ok s ->
          t.state <- Open s;
          Some s
      | Error e ->
          t.state <- Disabled;
          t.warn ("gate log disabled: " ^ e);
          None)

let verdict_fields = function
  | Gate.Allow -> ("allow", "")
  | Gate.Warn r -> ("warn", r)
  | Gate.Deny r -> ("deny", r)
  | Gate.Fault r -> ("fault", r)
  | Gate.Code_red r -> ("code_red", r)

(* The custom "gate_verdict" entry: the final verdict is recorded explicitly
   because it can differ from the raw response (parse failures, transport
   errors and unknown verdicts all coerce to "fault", which blocks the command
   the way a deny does without counting against the agent). [command] is duplicated
   out of the payload so analysis tooling need not re-parse it. *)
let verdict_data ~command ~reason ~verdict =
  let tag, verdict_reason = verdict_fields verdict in
  `Assoc
    [
      ("verdict", `String tag);
      ("reason", `String verdict_reason);
      ("routedBecause", `String reason);
      ("command", `String command);
    ]

let log_call t ~api ~provider ~model ~payload ~command ~reason ~turn ~verdict =
  match session t ~provider ~model with
  | None -> ()
  | Some s -> (
      try
        Session.append_user s ~text:payload ();
        Option.iter (Session.append_assistant s ~api ~provider ~model) turn;
        Session.append_custom s ~custom_type:"gate_verdict"
          ~data:(verdict_data ~command ~reason ~verdict)
      with Sys_error msg ->
        t.state <- Disabled;
        t.warn ("gate log disabled: " ^ msg))

let path t = match t.state with Open s -> Some (Session.path s) | _ -> None

let close t =
  match t.state with
  | Open s -> ( try Session.close s with Sys_error _ -> ())
  | Pending | Disabled -> ()
