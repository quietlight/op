let default_timeout = 120.0
let max_timeout = 600
let max_output_bytes = 30_000

type modules = { names : string list; include_path : string }
type plugin_command = { command : string; summary : string }

type plugins = {
  names : string list;
  registry : string;
  stub_dir : string;
  commands : (string * plugin_command list) list;
}

type error = Spawn_error of string | Timeout of float * string

(* Shared by [run] and [ast] so gated parser state matches execution.
   [--no-config-file] disables plugins, so plugin profiles use empty config
   stubs + [--plugin-config]. *)
let base_flags ?modules ?plugins () =
  [ "--table-mode"; "none" ]
  @ (match plugins with
    | None -> [ "--no-config-file" ]
    | Some p ->
        [
          "--env-config";
          Filename.concat p.stub_dir "env.nu";
          "--config";
          Filename.concat p.stub_dir "config.nu";
          "--plugin-config";
          p.registry;
        ])
  @
  match modules with
  | Some m -> [ "--include-path"; m.include_path ]
  | None -> []

let err fmt = Fmt.kstr (fun s -> Error s) fmt

(* Allowlisted env for [ast]/[register_plugin]/[plugin_commands] (same
   discipline as {!Nu_sandbox.child_env} so API keys never leak to plugin
   binaries). Replaces matching names rather than appending because nu and C
   [getenv] disagree on duplicate entries. *)
let ast_env_allow =
  [
    "HOME"; "USER"; "LOGNAME"; "LANG"; "LC_ALL"; "TERM"; "TMPDIR"; "TZ"; "PATH";
  ]

let env_with overrides =
  let inherited =
    Unix.environment () |> Array.to_list
    |> List.filter (fun kv ->
        match String.index_opt kv '=' with
        | None -> false
        | Some i ->
            let name = String.sub kv 0 i in
            List.mem name ast_env_allow && not (List.mem_assoc name overrides))
  in
  Array.of_list
    (inherited @ List.map (fun (name, value) -> name ^ "=" ^ value) overrides)

let pp_error = function
  | Spawn_error msg -> "failed to run nu: " ^ msg
  | Timeout (t, output) ->
      if output = "" then Fmt.str "nu script timed out after %.0fs" t
      else
        Fmt.str "nu script timed out after %.0fs\nPartial output:\n%s" t output

(* Bounded tail sink (<= 2 * max_output_bytes in memory); nu errors land at the
   end. *)
module Tail = struct
  type t = { buf : Buffer.t; mutable total : int }

  let single_write t bufs =
    List.iter (fun c -> Buffer.add_string t.buf (Cstruct.to_string c)) bufs;
    let n = Cstruct.lenv bufs in
    t.total <- t.total + n;
    let len = Buffer.length t.buf in
    if len > 2 * max_output_bytes then begin
      let tail = Buffer.sub t.buf (len - max_output_bytes) max_output_bytes in
      Buffer.clear t.buf;
      Buffer.add_string t.buf tail
    end;
    n

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let tail_sink =
  let ops = Eio.Flow.Pi.sink (module Tail) in
  fun t -> Eio.Resource.T (t, ops)

let truncate_output (t : Tail.t) =
  let s = Buffer.contents t.buf in
  let n = String.length s in
  if t.total <= max_output_bytes then s
  else
    let kept = min n max_output_bytes in
    Fmt.str "... (output truncated, showing last %d of %d bytes)\n%s" kept
      t.total
      (String.sub s (n - kept) kept)

(* Empty stdin gives immediate EOF so scripts reading stdin fail fast. *)
let spawn_capture ~proc_mgr ~clock ~timeout ?env ~executable ~stdout ~stderr
    args =
  try
    Eio.Time.with_timeout clock timeout (fun () ->
        Eio.Switch.run (fun sw ->
            let proc =
              Eio.Process.spawn ~sw proc_mgr ?env ~executable
                ~stdin:(Eio.Flow.string_source "")
                ~stdout ~stderr args
            in
            Ok (Eio.Process.await proc)))
  with
  | Eio.Io (e, _) -> Error (`Spawn (Fmt.str "%a" Eio.Exn.pp_err e))
  | Failure msg -> Error (`Spawn msg)

let run ~proc_mgr ~clock ?(timeout = default_timeout) ?modules ?plugins ~env
    ~executable ~script () =
  let tmp = Filename.temp_file "op-nu" ".nu" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ())
    (fun () ->
      Out_channel.with_open_bin tmp (fun oc ->
          Out_channel.output_string oc script);
      let tail = { Tail.buf = Buffer.create 1024; total = 0 } in
      let sink = tail_sink tail in
      let status =
        spawn_capture ~proc_mgr ~clock ~timeout ~env ~executable ~stdout:sink
          ~stderr:sink
          (("nu" :: base_flags ?modules ?plugins ()) @ [ tmp ])
      in
      let output () = truncate_output tail in
      match status with
      | Ok (`Exited n) -> Ok (output (), n)
      | Ok (`Signaled n) -> Ok (output (), 128 + n)
      | Error `Timeout -> Error (Timeout (timeout, truncate_output tail))
      | Error (`Spawn msg) -> Error (Spawn_error msg))

let ast_timeout = 30.0

(* Extract flattened JSON token stream via [--flatten] (so nested closure/block
   bodies are emitted as walkable tokens, unlike [get block]). *)
let ast ~proc_mgr ~clock ?modules ?plugins ~executable script =
  let tmp = Filename.temp_file "op-nu-ast" ".nu" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ())
    (fun () ->
      Out_channel.with_open_bin tmp (fun oc ->
          Out_channel.output_string oc script);
      let out = Buffer.create 4096 and errbuf = Buffer.create 256 in
      (* Pass path via env rather than interpolating into [-c] source. *)
      let command = {|ast (open --raw $env.OP_AST_FILE) --json --flatten|} in
      let env = env_with [ ("OP_AST_FILE", tmp) ] in
      let status =
        spawn_capture ~proc_mgr ~clock ~timeout:ast_timeout ~env ~executable
          ~stdout:(Eio.Flow.buffer_sink out)
          ~stderr:(Eio.Flow.buffer_sink errbuf)
          (("nu" :: base_flags ?modules ?plugins ()) @ [ "-c"; command ])
      in
      match status with
      | Ok (`Exited 0) -> (
          match Yojson.Safe.from_string (Buffer.contents out) with
          | json -> Ok json
          | exception Yojson.Json_error msg ->
              Error ("nu ast returned invalid JSON: " ^ msg))
      | Ok _ ->
          let e = String.trim (Buffer.contents errbuf) in
          Error (if e = "" then "nu could not parse the script" else e)
      | Error `Timeout -> Error "nu ast timed out"
      | Error (`Spawn msg) -> Error ("failed to run nu: " ^ msg))

let register_plugin ~proc_mgr ~clock ~executable ~registry ~binary =
  let out = Buffer.create 256 in
  let sink = Eio.Flow.buffer_sink out in
  let command =
    {|plugin add --plugin-config $env.OP_PLUGIN_REGISTRY $env.OP_PLUGIN_BINARY|}
  in
  let env =
    env_with [ ("OP_PLUGIN_REGISTRY", registry); ("OP_PLUGIN_BINARY", binary) ]
  in
  match
    spawn_capture ~proc_mgr ~clock ~timeout:ast_timeout ~env ~executable
      ~stdout:sink ~stderr:sink
      [ "nu"; "--no-config-file"; "-c"; command ]
  with
  | Ok (`Exited 0) -> Ok ()
  | Ok _ ->
      err "plugin add %s failed: %s" binary (String.trim (Buffer.contents out))
  | Error `Timeout -> err "plugin add %s timed out" binary
  | Error (`Spawn msg) -> err "plugin add %s: %s" binary msg

let version ~proc_mgr ~clock ~executable =
  let out = Buffer.create 32 in
  match
    spawn_capture ~proc_mgr ~clock ~timeout:ast_timeout ~env:(env_with [])
      ~executable ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink (Buffer.create 64))
      [ "nu"; "--version" ]
  with
  | Ok (`Exited 0) -> (
      match String.trim (Buffer.contents out) with "" -> None | v -> Some v)
  | Ok _ | Error _ -> None

let command_of_json = function
  | `Assoc fields -> (
      match
        (List.assoc_opt "name" fields, List.assoc_opt "description" fields)
      with
      | Some (`String command), Some (`String summary) ->
          Some { command; summary }
      | Some (`String command), _ -> Some { command; summary = "" }
      | _ -> None)
  | _ -> None

let plugin_of_json = function
  | `Assoc fields -> (
      match
        (List.assoc_opt "name" fields, List.assoc_opt "commands" fields)
      with
      | Some (`String name), Some (`List commands) ->
          Some (name, List.filter_map command_of_json commands)
      | _ -> None)
  | _ -> None

let plugin_commands ~proc_mgr ~clock ~executable ~registry =
  let out = Buffer.create 4096 and errbuf = Buffer.create 256 in
  let command =
    {|plugin list --plugin-config $env.OP_PLUGIN_REGISTRY | select name commands | to json --raw|}
  in
  match
    spawn_capture ~proc_mgr ~clock ~timeout:ast_timeout
      ~env:(env_with [ ("OP_PLUGIN_REGISTRY", registry) ])
      ~executable ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink errbuf)
      [ "nu"; "--no-config-file"; "-c"; command ]
  with
  | Ok (`Exited 0) -> (
      match Yojson.Safe.from_string (Buffer.contents out) with
      | `List plugins -> Ok (List.filter_map plugin_of_json plugins)
      | _ -> err "plugin list: expected a JSON list"
      | exception Yojson.Json_error msg ->
          err "plugin list: invalid JSON: %s" msg)
  | Ok _ -> err "plugin list failed: %s" (String.trim (Buffer.contents errbuf))
  | Error `Timeout -> err "plugin list timed out"
  | Error (`Spawn msg) -> err "plugin list: %s" msg

(* Inline up to this many subcommands; larger plugins (e.g. polars) show a count. *)
let max_listed_commands = 8

let plugin_line ~commands name =
  let cmds = Option.value ~default:[] (List.assoc_opt name commands) in
  let main, rest = List.partition (fun c -> c.command = name) cmds in
  let summary =
    match main with
    | { summary; _ } :: _ when summary <> "" -> " " ^ summary
    | _ -> ""
  in
  let names = List.map (fun c -> c.command) rest in
  let entry c =
    if c.summary = "" then Fmt.str "\n  - `%s`" c.command
    else Fmt.str "\n  - `%s`: %s" c.command c.summary
  in
  let listing =
    match names with
    | [] -> ""
    | names when List.length names <= max_listed_commands ->
        String.concat "" (List.map entry rest)
    | names when main <> [] ->
        Fmt.str " %d commands; `help %s` lists them." (List.length names) name
    | names ->
        Fmt.str " %d commands, e.g. %s." (List.length names)
          (String.concat ", " (List.filteri (fun i _ -> i < 4) names))
  in
  Fmt.str "\n- %s:%s%s" name summary listing

let plugin_note = function
  | None | Some { names = []; _ } -> ""
  | Some p ->
      Fmt.str
        " Plugins (load one with a bare `plugin use <name>`; other `plugin` \
         subcommands are denied; once loaded, `help <command>` shows its flags \
         and examples):%s"
        (String.concat "" (List.map (plugin_line ~commands:p.commands) p.names))

let description ~version ~externals ~(modules : modules option)
    ~(plugins : plugins option) =
  let allowed =
    match externals with [] -> "none" | l -> String.concat ", " l
  in
  let module_note =
    match modules with
    | None -> ""
    | Some m ->
        let current =
          match m.names with
          | [] -> "none currently"
          | names -> "currently: " ^ String.concat ", " names
        in
        Fmt.str
          " Module dirs (%s) are rescanned at each call — save a `.nu` file \
           there and `use <name>` in your next call; it is gate-vetted fresh \
           from disk (%s)."
          m.include_path current
  in
  Fmt.str
    "Run a Nushell%s script; returns combined stdout+stderr and the exit code. \
     Raw Nushell source, not bash -- write real pipelines. #1 reflex to \
     unlearn: don't reach for Unix tools or `^cmd`; only %s run as externals, \
     everything else needs a builtin (echo->print, cat->open --raw, \
     grep->where, 2>&1->| complete). Other externals and the network/exec \
     builtins (http, run-external, exec, start, source, overlay use) are \
     gate-denied. Concurrency: `par-each` and `job spawn`/`job recv` work; \
     background jobs die at script exit, so join before the end. Authoring .nu \
     files: `nu-check --debug (\"file.nu\" | path expand)` parse-checks a \
     script or module without running it (the `path expand` is required: bare \
     relative paths resolve against the temp dir the script runs from). The \
     active profile is introspectable via $env.OP_NU_PROFILE, \
     $env.OP_NU_EXTERNALS, and $env.OP_NU_MODULES (comma-separated).%s Full \
     playbook: read ./skills/practical-nu.md at the project root.%s"
    (match version with Some v -> Fmt.str " (v%s)" v | None -> "")
    allowed module_note (plugin_note plugins)

let tool ~proc_mgr ~clock ?version ?modules ?plugins ~env ~executable ~externals
    () : Tool.t =
  {
    name = "nu";
    description = description ~version ~externals ~modules ~plugins;
    parameters =
      `Assoc
        [
          ("type", `String "object");
          ( "properties",
            `Assoc
              [
                ( "script",
                  `Assoc
                    [
                      ("type", `String "string");
                      ( "description",
                        `String "The Nushell script to run (raw source text)" );
                    ] );
                ( "timeout",
                  `Assoc
                    [
                      ("type", `String "integer");
                      ( "description",
                        `String "Timeout in seconds (default 120, max 600)" );
                    ] );
              ] );
          ("required", `List [ `String "script" ]);
        ];
    run =
      (fun args ->
        match Tool.required_string args "script" with
        | Error e -> Error e
        | Ok script -> (
            let timeout =
              match Tool.optional_int args "timeout" with
              | Some n when n > 0 -> Some (float_of_int (min n max_timeout))
              | _ -> None
            in
            match
              run ~proc_mgr ~clock ?timeout ?modules ?plugins ~env:(env ())
                ~executable ~script ()
            with
            | Ok (output, 0) -> Fmt.kstr Tool.text "%s\n(exit code: 0)" output
            | Ok (output, code) -> err "%s\n(exit code: %d)" output code
            | Error e -> Error (pp_error e)));
  }

module For_testing = struct
  let env_with = env_with
  let plugin_note = plugin_note
end
