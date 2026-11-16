open Cmdliner

type set_kind = Set_default
type list_kind = Sessions | Messages | Branches | Models | Default

(* How a prompt run reports itself on stdout. [Human] (the default) is the
   rendered transcript — what you want when you run op yourself. The machine
   views are opt-in: [Stream] is the session's ndjson lines as they happen, then
   the result envelope tagged with "type":"result" (what hop reads); [Json] is
   the single result envelope alone. Warnings and diagnostics go to stderr in
   every mode. *)
type output = Json | Stream | Human

type opts = {
  set_kind : set_kind option;
  list_kind : list_kind option;
  limit : int;
  session_id : string option;
  parent_id : string option;
  from_session : string option;
  handoff : bool;
  model_spec : string option;
  prompt_file : string option;
  max_steps : int option;
  tools : string list option;
  profile : string option;
  json : bool;
  stream : bool;
  images : string list;
  context : string list;
  prompt : string option;
}

let v set_kind list_kind limit session_id parent_id from_session handoff
    model_spec prompt_file max_steps tools profile json stream images context
    prompt =
  {
    set_kind;
    list_kind;
    limit;
    session_id;
    parent_id;
    from_session;
    handoff;
    model_spec;
    prompt_file;
    max_steps;
    tools;
    profile;
    json;
    stream;
    images;
    context;
    prompt;
  }

let prompt_arg =
  let doc = "The prompt to run (read from stdin when omitted)" in
  Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"PROMPT")

let session_docs = "SESSION OPTIONS"

(* Flag naming is intentionally mixed: [--sessionId]/[--parentId] are camelCase
   to match pi's flag names (op resumes pi-format sessions, so keeping the names
   identical lets the two share muscle memory and copy-pasted commands). The
   newer op-only flags ([--max-steps], [--set], [--list]) use conventional
   kebab-case. Do not "fix" the camelCase pair — it is pinned to pi. *)
let session_id_arg =
  let doc =
    "The session $(i,ID) to act on. When running a prompt, resume that \
     session: continue from its leaf, or from $(b,--parentId) when given. With \
     $(b,--list) $(b,messages), select the session to list."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "sessionId"; "session" ] ~docs:session_docs ~doc ~docv:"ID")

let positive_int =
  let parse s =
    match int_of_string_opt s with
    | Some n when n >= 1 -> Ok n
    | Some _ -> Error (`Msg "must be a positive integer")
    | None -> Error (`Msg "invalid integer")
  in
  Arg.conv (parse, Format.pp_print_int)

let parent_id_arg =
  let doc =
    "Continue from (and branch at) entry $(i,ID) instead of the leaf; requires \
     $(b,--sessionId). To $(i,continue) a branch, pass its tip from $(b,op \
     --list) $(b,branches); to $(i,fork) mid-thread, pass any entry id from \
     $(b,op --list) $(b,messages), whose $(b,--json) form also includes each \
     entry's $(b,parentId) and marks branch points with $(b,isFork). If the \
     entry has unanswered tool calls, op branches from the nearest complete \
     turn instead."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "parentId" ] ~docs:session_docs ~doc ~docv:"ID")

let from_session_arg =
  let doc =
    "Start a $(i,fresh) session whose header records $(i,ID) as its \
     $(b,parentSession) — the provenance link for a handoff, where the new \
     thread continues the work of an old one with a clean context. Mutually \
     exclusive with $(b,--sessionId), which resumes a session rather than \
     forking off it."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "fromSession" ] ~docs:session_docs ~doc ~docv:"ID")

let handoff_arg =
  let doc =
    "Write a handoff prompt for session $(b,--sessionId) instead of running it \
     — a self-contained brief (goal, progress, decisions, next steps and the \
     files touched) for starting a fresh thread once the context window is \
     full. The $(i,PROMPT) argument, when given, is the $(i,goal) that steers \
     what to carry over. The source session is only read, never appended to. \
     Output is the prompt as markdown on stdout, or the full record under \
     $(b,--json); pipe it back to op with $(b,--fromSession) to start the new \
     thread. The summarising model is $(b,handoffModel) from settings.json, or \
     the run's model."
  in
  Arg.(value & flag & info [ "handoff" ] ~doc)

let limit_arg =
  let doc =
    "With $(b,--list), show only the latest positive $(i,N) rows (default 25) \
     — sessions, messages, branches, and models. $(b,--list) $(b,default) is a \
     single value, so $(i,N) is ignored there."
  in
  Arg.(value & opt positive_int 25 & info [ "limit" ] ~doc ~docv:"N")

let list_arg =
  let kinds =
    [
      ("sessions", Sessions);
      ("messages", Messages);
      ("branches", Branches);
      ("models", Models);
      ("default", Default);
    ]
  in
  let doc =
    "List sessions, messages, branches, models, or default (KIND), then exit"
  in
  Arg.(value & opt (some (enum kinds)) None & info [ "list" ] ~doc ~docv:"KIND")

let set_arg =
  let doc =
    "Set a persisted config value (KIND), then exit; currently only \
     $(b,default), which saves the $(i,PROVIDER/MODEL) argument as the default \
     model in $(b,~/.op_global/agent/settings.json) and echoes it back. Unlike \
     $(b,--model) (one run only), this changes the stored default."
  in
  Arg.(
    value
    & opt (some (enum [ ("default", Set_default) ])) None
    & info [ "set" ] ~doc ~docv:"KIND")

let model_arg =
  let doc =
    "Model for this run only, as \"provider/model\" or \"model\" (default \
     provider); overrides the configured default without changing it and may \
     name an unlisted provider model for one run. Use $(b,--set) $(b,default) \
     to change the saved default, which must be listed in config."
  in
  Arg.(value & opt (some string) None & info [ "model" ] ~doc ~docv:"SPEC")

let prompt_file_arg =
  let doc = "Read the prompt from FILE (overrides PROMPT and stdin)" in
  Arg.(value & opt (some string) None & info [ "prompt" ] ~doc ~docv:"FILE")

let max_steps_arg =
  let doc =
    "Cap the agent at $(i,N) steps, where a step is one assistant turn that \
     runs tool calls (default 75). A turn that answers with no tools finishes \
     within budget and never consumes a step, so a text-only reply always \
     completes; hitting the cap on a tool-using run exits 1, as the run is not \
     complete. $(i,N) must be positive."
  in
  Arg.(
    value & opt (some positive_int) None & info [ "max-steps" ] ~doc ~docv:"N")

let err_msg fmt = Fmt.kstr (fun s -> Error (`Msg s)) fmt

(* Parse a comma-separated [--tools] list, validating each name against
   {!Op.Tools.known}. The literal [none] is the empty set — a tool-less chat
   run. A bare empty list or an unknown name is a cmdliner error (exit 124),
   so the mistake is caught before any run starts. *)
let tools_conv =
  let parse s =
    if String.trim s = "none" then Ok []
    else
      let names =
        String.split_on_char ',' s |> List.map String.trim
        |> List.filter (fun x -> x <> "")
      in
      if names = [] then err_msg "no tools given (use 'none' for a chat run)"
      else
        match Op.Tools.validate names with
        | Ok names -> Ok names
        | Error msg -> err_msg "%s" msg
  in
  let print ppf = function
    | [] -> Format.pp_print_string ppf "none"
    | names -> Format.pp_print_string ppf (String.concat "," names)
  in
  Arg.conv (parse, print)

let tools_arg =
  let doc =
    "The comma-separated tool set for this run, chosen from \
     $(b,read,write,edit,glob,nu), or $(b,none) for a tool-less chat run. Use \
     this to pick a subset, e.g. $(b,--tools) $(i,read,write,edit,glob). \
     Defaults to the profile's $(b,tools) list when it has one, and to all of \
     them otherwise. An empty list or an unknown name is rejected."
  in
  Arg.(value & opt (some tools_conv) None & info [ "tools" ] ~doc ~docv:"LIST")

let profile_arg =
  let doc =
    "The capability profile for this run: a NAME from $(b,nuProfiles) in \
     $(b,~/.op_global/agent/settings.json), selecting the nu tool's allowed \
     externals, vetted modules, and plugins — and, when the profile has a \
     $(b,tools) list, the run's default tool set ($(b,\"tools\": []) makes a \
     chat profile; $(b,--tools) still overrides). Defaults to \
     $(b,nuDefaultProfile) when set; with neither, the toolbox is empty \
     (builtins-only Nushell). An unknown NAME is an error listing the \
     available profiles."
  in
  Arg.(value & opt (some string) None & info [ "profile" ] ~doc ~docv:"NAME")

let image_arg =
  let doc =
    "Attach an image $(i,FILE) to the prompt (repeatable, order-preserving). \
     The images ride in the initial user message; attachment order is \
     semantic, so a prompt can refer to \"reference 1 … query\". Accepts png, \
     jpeg, webp and gif (media type detected from the file's magic bytes), up \
     to 5 MB each. A missing file, an unsupported format, or an oversized \
     image is rejected before the run starts. No resizing is done."
  in
  Arg.(value & opt_all string [] & info [ "image" ] ~doc ~docv:"FILE")

let context_arg =
  let doc =
    "Inject the contents of $(i,FILE) (e.g. a project CLAUDE.md or AGENTS.md) \
     ahead of the prompt, wrapped in a $(b,<context file=...>) block \
     (repeatable, order-preserving). The block rides in the user message, so \
     it is logged to the session and survives resumes without re-passing the \
     flag. A missing or unreadable file is a hard error before the run starts."
  in
  Arg.(value & opt_all string [] & info [ "context" ] ~doc ~docv:"FILE")

let json_arg =
  let doc =
    "Emit a single JSON result object on stdout instead of the default human \
     transcript — for scripts and subagent calls that only want the outcome. \
     For prompt runs the object carries $(b,answer), \
     $(b,sessionId)/$(b,parentId) (to resume), $(b,model), $(b,tokens), this \
     run's activity ($(b,turns), $(b,toolCalls), $(b,toolErrors), $(b,tools)), \
     $(b,stopReason) and $(b,isError). $(b,isError) is false with exit 0 only \
     for a completed run ($(b,stopReason) $(i,finished)); a step-cap \
     truncation ($(b,stopReason) $(i,max_steps)) or a full context window \
     ($(b,stopReason) $(i,context_limit)) keeps the partial $(b,answer), and \
     $(b,sessionId) when available, but sets $(b,isError) and exits 1 (resume \
     with $(b,--sessionId), or hand off with $(b,--handoff)); a hard failure \
     is $(b,{\"error\":…,\"isError\":true}) and exits 1. Runs whose model has \
     a $(b,contextWindow) in models.json also carry $(b,context) \
     ($(b,tokens)/$(b,window)/$(b,pct)). With $(b,--list) $(i,KIND) too, each \
     listing is instead one JSON object on stdout: a $(b,kind) field plus the \
     data ($(b,sessions), $(b,messages), $(b,models), or \
     $(b,provider)/$(b,model) for $(b,default)); empty results are a success \
     with an empty array. Any listing failure is \
     $(b,{\"error\":…,\"isError\":true}) on stdout with exit 1, so callers \
     never scrape stderr."
  in
  Arg.(value & flag & info [ "json" ] ~doc)

let stream_arg =
  let doc =
    "Stream the run as newline-delimited JSON on stdout instead of the default \
     human transcript — the machine view of the same run, for frontends such \
     as $(b,hop). Each line is one session-log entry exactly as written to the \
     session file (see $(b,docs/pi-session-format.md)): the $(b,session) \
     header for a fresh run, then $(b,model_change), the user $(b,message), \
     each assistant turn and each $(b,toolResult), as they happen. A resumed \
     run streams only its new entries. The final line is the $(b,--json) \
     result object with a $(b,type) field of $(i,result) added, so a consumer \
     can tell it apart without reassembling $(b,answer) from the entries; exit \
     codes match $(b,--json). Mutually exclusive with $(b,--json)."
  in
  Arg.(value & flag & info [ "stream" ] ~doc)

(* The prompt run's output mode: the human transcript unless a flag asked for
   one of the machine views. *)
let output opts =
  if opts.json then Json else if opts.stream then Stream else Human

(* What one invocation does. The flags are flat because their spellings are
   pinned (to pi, and to hop), but [--set], [--list] and [--handoff] are really
   separate commands, and each reads only some of the flags. Parsing lands
   here, so the dispatcher matches on a command that carries exactly what it
   needs instead of consulting an options record for combinations that cannot
   occur. *)
type run = {
  mode : output;
  session_id : string option;
  parent_id : string option;
  from_session : string option;
  model_spec : string option;
  prompt_file : string option;
  max_steps : int option;
  tools : string list option;
  profile : string option;
  images : string list;
  context : string list;
  prompt : string option;
}

type command =
  | Set of { kind : set_kind; json : bool; value : string option }
  | List of {
      kind : list_kind;
      json : bool;
      limit : int;
      session_id : string option;
      stray_prompt : bool;
    }
  | Handoff of {
      mode : output;
      session_id : string;
      parent_id : string option;
      model_spec : string option;
      prompt_file : string option;
      prompt : string option;
    }
  | Run of run

let usage_error msg = Error (`Msg msg)

(* Flag combinations that cannot mean anything are usage errors (exit 124)
   rather than silent precedence rules: [--json] and [--stream] both own stdout
   and disagree on its shape; a handoff writes no session entries, so it has no
   stream to render. *)
let command (opts : opts) =
  match (opts.set_kind, opts.list_kind, opts.handoff, opts.session_id) with
  | _ when opts.json && opts.stream ->
      usage_error "--json and --stream are mutually exclusive"
  | _ when opts.session_id <> None && opts.from_session <> None ->
      usage_error "--fromSession and --sessionId are mutually exclusive"
  | _, _, true, None -> usage_error "--handoff requires --sessionId"
  | _, _, true, _ when opts.stream ->
      usage_error "--handoff and --stream are mutually exclusive"
  | Some kind, _, _, _ ->
      Ok (Set { kind; json = opts.json; value = opts.prompt })
  | None, Some kind, _, _ ->
      Ok
        (List
           {
             kind;
             json = opts.json;
             limit = opts.limit;
             session_id = opts.session_id;
             stray_prompt = Option.is_some opts.prompt;
           })
  | None, None, true, Some session_id ->
      Ok
        (Handoff
           {
             mode = output opts;
             session_id;
             parent_id = opts.parent_id;
             model_spec = opts.model_spec;
             prompt_file = opts.prompt_file;
             prompt = opts.prompt;
           })
  | None, None, false, _ ->
      Ok
        (Run
           {
             mode = output opts;
             session_id = opts.session_id;
             parent_id = opts.parent_id;
             from_session = opts.from_session;
             model_spec = opts.model_spec;
             prompt_file = opts.prompt_file;
             max_steps = opts.max_steps;
             tools = opts.tools;
             profile = opts.profile;
             images = opts.images;
             context = opts.context;
             prompt = opts.prompt;
           })

let command_term =
  Term.(
    term_result ~usage:true
      (const command
      $ (const v $ set_arg $ list_arg $ limit_arg $ session_id_arg
       $ parent_id_arg $ from_session_arg $ handoff_arg $ model_arg
       $ prompt_file_arg $ max_steps_arg $ tools_arg $ profile_arg $ json_arg
       $ stream_arg $ image_arg $ context_arg $ prompt_arg)))

let man =
  [
    `S Manpage.s_description;
    `P
      "op runs a single prompt through an agentic loop and exits — there is no \
       interactive mode. By default the run is rendered to stdout as a \
       human-readable transcript (each assistant turn, tool call and result as \
       it happens, then a summary of the run). The machine views are opt-in \
       and share the same run: $(b,--stream) emits the session's \
       newline-delimited JSON entries (what a frontend such as $(b,hop) \
       reads), and $(b,--json) reduces stdout to the result envelope alone. \
       Warnings and diagnostics go to stderr.";
    `P
      "The prompt is taken from $(b,--prompt) $(i,FILE), else from the \
       $(i,PROMPT) argument, else from stdin.";
    `P
      "Every run is appended to a resumable session log under \
       $(b,~/.op_global/sessions/). On exit op prints a ready-to-paste resume \
       command on stderr.";
    `S "SESSIONS";
    `P
      "A session is a tree: each prompt, reply and tool result is an entry \
       linked to its parent. Resume with $(b,--sessionId) to continue from the \
       session's single leaf. A branched session has several leaves, so op \
       cannot guess which to use — pass $(b,--parentId) to continue from (and \
       fork at) a specific entry. $(b,op --list branches) lists one tip per \
       branch, which is what continuing takes; $(b,op --list messages) lists \
       every entry and marks fork points with $(b,*), which is where a new \
       branch is started from. Add $(b,--json) to either for parent ids.";
    `S "CONTEXT AND HANDOFF";
    `P
      "When models.json gives the run's model a $(b,contextWindow), every \
       result envelope carries a $(b,context) object — $(b,{tokens, window, \
       pct}) — and the run stops once $(b,pct) reaches $(b,contextLimit) \
       (settings.json, default 0.85), with $(b,stopReason) $(i,context_limit) \
       and exit 1. The check runs between turns on the usage the provider last \
       reported, so it is a stopping threshold, not a cap on the next request. \
       The history is complete at that point, so nothing is lost.";
    `P
      "$(b,--handoff) is the way forward from there: instead of compacting the \
       thread in place, it writes a prompt that starts a $(i,fresh) one. Give \
       it the next goal and it reads the session — never appending to it — and \
       returns a brief carrying the goal, progress, decisions, next steps and \
       the files touched. Review it, then feed it back with $(b,--fromSession) \
       so the new session records where it came from.";
    `S Manpage.s_examples;
    `Pre
      "Run a prompt, or pipe one in:\n\
      \  op \"summarise README.md\"\n\
      \  echo \"explain this stack trace\" | op\n\
      \  op --prompt task.txt";
    `Pre
      "Browse history:\n\
      \  op --list sessions\n\
      \  op --list messages --sessionId ID\n\
      \  op --list branches --sessionId ID";
    `Pre
      "Resume, or branch from a chosen entry:\n\
      \  op --sessionId ID \"now add tests\"\n\
      \  op --sessionId ID --parentId ENTRY \"try another approach\"";
    `Pre
      "Override the model for one run:\n  op --model anthropic/claude-x \"...\"";
    `Pre
      "Change the saved default model:\n  op --set default anthropic/claude-x";
    `Pre
      "Inject project context files ahead of the prompt:\n\
      \  op --context CLAUDE.md \"add a --verbose flag\"";
    `Pre
      "Script-friendly bounded run (result envelope only):\n\
      \  op --json --max-steps 3 \"summarise the failing test\"";
    `Pre
      "Hand a full session off to a fresh one:\n\
      \  op --handoff --sessionId ID \"execute phase one of the plan\"\n\
      \  op --handoff --sessionId ID \"…\" | op --fromSession ID";
    `S "SESSION OPTIONS";
    `S Manpage.s_options;
  ]

let exits =
  [
    Cmd.Exit.info 0
      ~doc:"on completed prompt runs, successful listings, help, or version.";
    Cmd.Exit.info 1
      ~doc:
        "on op runtime failures, including hard errors, step-cap truncation \
         and a full context window.";
    Cmd.Exit.info 124 ~doc:"on command line parsing errors.";
    Cmd.Exit.info 125 ~doc:"on unexpected internal errors (bugs).";
  ]

let cmd run =
  let doc = "Opinionated one-shot agentic loop" in
  let info = Cmd.info "op" ~version:"0.2.0" ~doc ~man ~exits in
  Cmd.v info Term.(const run $ command_term)
