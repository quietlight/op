let ( let* ) = Result.bind
let max_script_chars = 32_768

(* Network/exec builtins matched on first word; [job] is allowed because job
   closures are flattened and die on script exit. *)
let denied_builtins = [ "http"; "run-external"; "exec"; "start" ]

let first_word s =
  match String.index_opt s ' ' with Some i -> String.sub s 0 i | None -> s

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* Strip leading "export " so [export use]/[export extern]/[export module] are
   judged identically to their non-exported forms. *)
let unexported s =
  let p = "export " in
  if starts_with ~prefix:p s then
    String.sub s (String.length p) (String.length s - String.length p)
  else s

(* Null-safe lookup: AST nodes contain many [`Null] fields. *)
let member key = function
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some v -> v | None -> `Null)
  | _ -> `Null

let string_member key json =
  match member key json with `String s -> Some s | _ -> None

let token_content t = string_member "content" t
let token_shape t = string_member "shape" t

(* [Policy] maps to [Gate.Deny]; [Syntax] maps to [Gate.Unparsed]. *)
type refusal = Policy of string | Syntax of string

let refusal_message = function Policy s | Syntax s -> s

let map_refusal f = function
  | Policy s -> Policy (f s)
  | Syntax s -> Syntax (f s)

let err_deny fmt = Fmt.kstr (fun s -> Error (Policy s)) fmt
let err_syntax fmt = Fmt.kstr (fun s -> Error (Syntax s)) fmt

let text_tool_hints =
  [
    ( [ "grep"; "egrep"; "fgrep"; "rg"; "ag" ],
      "`open --raw FILE | lines | enumerate | where item =~ 'PAT'`; across a \
       tree, `ls **/*.ml | get name | par-each {|f| ... }`" );
    ([ "cat" ], "`open --raw FILE`");
    ([ "head" ], "`open --raw FILE | lines | first N`");
    ([ "tail" ], "`open --raw FILE | lines | last N`");
    ([ "wc" ], "`open --raw FILE | lines | length`");
    ([ "find" ], "builtin `ls **/*` (skips dotfiles), or `glob '**/*.ml'`");
    ( [ "sed" ],
      "`str replace -a` (`-r` for regex), with `| save -f FILE` for in-place" );
    ([ "awk"; "cut" ], "`parse`, `split column`, `get`, `each {|row| ... }`");
    ([ "tr" ], "`str replace -a`");
    ([ "xargs" ], "`each {|it| ... }`");
    ([ "echo" ], "`print`");
  ]

let text_tool_hint name =
  List.find_map
    (fun (names, hint) -> if List.mem name names then Some hint else None)
    text_tool_hints

(* Path-form dispatch bypasses the toolbox PATH and is always denied. *)
let check_external ~externals name =
  if String.contains name '/' || String.contains name '\\' then
    let base = Filename.basename name in
    if List.mem base externals then
      err_deny
        "external command `%s` is denied: path-form dispatch bypasses the nu \
         toolbox. `%s` is in the toolbox — call it bare (`^%s`); the toolbox \
         entry resolves through the host PATH at exec time, so a just-rebuilt \
         artifact behind that name is what runs."
        name base base
    else
      err_deny
        "external command `%s` is denied: path-form dispatch bypasses the nu \
         toolbox. Call the bare command name so it resolves through the \
         toolbox."
        name
  else if List.mem name externals then Ok ()
  else
    let allowed =
      match externals with [] -> "none" | l -> String.concat ", " l
    in
    match text_tool_hint name with
    | Some hint ->
        err_deny
          "external command `%s` is not in the nu toolbox (allowed: %s). Use \
           the builtin instead: %s. More translations: \
           ./skills/practical-nu.md at the project root."
          name allowed hint
    | None ->
        err_deny
          "external command `%s` is not in the nu toolbox (allowed: %s). For \
           network work, delegate via a nested `op --json \"prompt\"` call and \
           parse it with `from json`."
          name allowed

(* First non-flag token after [use] (e.g. [use --reload std] -> "std"). *)
let rec use_module = function
  | t :: rest -> (
      match token_shape t with
      | Some "shape_flag" -> use_module rest
      | _ -> token_content t)
  | [] -> None

let is_flag t = token_shape t = Some "shape_flag"

(* Require bare [plugin use NAME] with no flags so [--plugin-config] cannot
   redirect to an unvetted registry. *)
let bare_plugin_use ~plugins = function
  | name :: next ->
      (not (is_flag name))
      && Option.fold ~none:false
           ~some:(fun p -> List.mem p plugins)
           (token_content name)
      && not (match next with t :: _ -> is_flag t | [] -> false)
  | [] -> false

(* Inline [module NAME { ... }] is flattened and walked; file [module ...] is not. *)
let inline_module = function
  | _ :: body :: _ -> token_shape body = Some "shape_block"
  | _ -> false

let check_plugin_use ~plugins rest =
  if bare_plugin_use ~plugins rest then Ok ()
  else
    err_deny
      "`plugin use` may only load %s; op supplies the plugin registry, so call \
       it as a bare `plugin use <name>`."
      (match plugins with
      | [] -> "nothing in this profile (no plugins configured)"
      | l -> String.concat ", " l)

let check_use ~modules rest =
  match use_module rest with
  (* Exact "std" or "std/..." only (not e.g. "stdfoo.nu"). *)
  | Some m when m = "std" || starts_with ~prefix:"std/" m -> Ok ()
  (* Scanned module names carry no path separators. *)
  | Some m when List.mem m modules -> Ok ()
  | _ ->
      err_deny
        "`use` may only import the standard library (`use std`)%s; module dirs \
         are rescanned at each call, so create a `.nu` file there and import \
         its bare name on the next call."
        (match modules with
        | [] -> " or a module found in the profile's module dirs at this call"
        | l ->
            " or a module found in the profile's module dirs at this call ("
            ^ String.concat ", " l ^ ")")

let check_command ~modules ~plugins ~rest content =
  let w = first_word content in
  if List.mem w denied_builtins then
    err_deny
      "`%s` is denied in the nu tool: it reaches the network or runs programs \
       outside the toolbox. Delegate network work via `op --json \"prompt\"` \
       and parse with `from json`."
      w
  else if w = "extern" then
    (* Calls to an [extern]-declared command flatten as [shape_internalcall]
       with no [shape_external], bypassing [check_external]; deny the decl. *)
    err_deny
      "`extern` is denied: it declares an external command the gate cannot see \
       at the call site, which would run a program outside the toolbox. Call \
       the external directly by its literal name instead."
  else if w = "module" && not (inline_module rest) then
    err_deny
      "`module <file>` is denied: it would run code from a file the gate never \
       inspected. Write the module inline (`module name { ... }`) or put it in \
       a profile module dir."
  else if starts_with ~prefix:"overlay use" content then
    err_deny
      "`overlay use` is denied: it would run code from a file the gate never \
       inspected. Inline the definitions into this script instead."
  else if w = "source" || w = "source-env" then
    err_deny
      "`%s` is denied: it would run code from a file the gate never inspected. \
       Inline the code into this script instead."
      w
  else if starts_with ~prefix:"plugin use" content then
    check_plugin_use ~plugins rest
  else if w = "plugin" then
    err_deny
      "`plugin` subcommands other than `plugin use` are denied: they mutate \
       the plugin registry or execute arbitrary binaries."
  else if w = "use" then check_use ~modules rest
  else Ok ()

(* Additional denied commands; [test_nu] pins the reviewed builtin catalogue so
   new Nushell releases force an explicit decision here. *)
let denied_commands =
  [
    ( "run",
      "it runs a script file the gate never inspected. Inline the code into \
       this script instead." );
    ( "run-internal",
      "it calls a builtin by a runtime name the gate cannot check. Call the \
       builtin by its literal name." );
    ( "history",
      "shell history often holds pasted secrets. Ask the user for the command \
       instead." );
    ("config nu", "it runs $env.EDITOR, a program outside the toolbox.");
    ("config env", "it runs $env.EDITOR, a program outside the toolbox.");
    ("config reset", "it rewrites the user's own Nushell config files.");
    ("version check", "it reaches the network.");
    ( "generate completions",
      "it runs the named program (with --help) outside the gate's view." );
  ]

(* [%$cmd] / [%(expr)] flattens to a single token starting with '%'; static
   [%print] flattens to plain "print". *)
let denied_command content =
  if starts_with ~prefix:"%" content then
    Some
      ( "dynamic dispatch (`%$cmd`)",
        "it calls a builtin by a runtime name the gate cannot check. Call the \
         builtin by its literal name." )
  else
    let named n =
      Option.map (fun r -> (n, r)) (List.assoc_opt n denied_commands)
    in
    match named content with
    | Some d -> Some d
    | None -> named (first_word content)

let check_internalcall ~modules ~plugins ~rest content =
  let content = unexported content in
  match denied_command content with
  | Some (name, reason) -> err_deny "`%s` is denied: %s" name reason
  | None -> check_command ~modules ~plugins ~rest content

(* Detect unknown subcommands like [polars scan-parquet], which flatten to
   [shape_internalcall] followed by [shape_garbage]. *)
let generic_garbage near =
  Fmt.str
    "the gate could not parse part of this script%s. That is usually a Nushell \
     syntax error (a hyphenated variable/parameter name — nu wants snake_case, \
     a stray `$` on a custom-command call, an unbalanced paren), or an \
     external with a computed name, which is denied. Fix the syntax or call a \
     literal toolbox command name."
    near

let subcommand_word s =
  s <> ""
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       s

let unknown_subcommand ~prev garbage =
  match prev with
  | Some p when token_shape p = Some "shape_internalcall" -> (
      match token_content p with
      | Some group when subcommand_word group && subcommand_word garbage ->
          Some group
      | _ -> None)
  | _ -> None

let garbage_reason ~prev t =
  match token_content t with
  | Some c when c <> "" -> (
      match unknown_subcommand ~prev c with
      | Some group ->
          err_syntax
            "the gate could not parse part of this script near `%s`: `%s %s` \
             is not a command. List the real `%s` subcommands with `help \
             commands | where name starts-with '%s ' | get name`, then use one \
             of those."
            c group c group group
      | None -> err_syntax "%s" (generic_garbage (" near `" ^ c ^ "`")))
  | _ -> err_syntax "%s" (generic_garbage "")

let rec walk ?prev ~externals ~modules ~plugins = function
  | [] -> Ok ()
  | t :: rest ->
      let* () =
        match token_shape t with
        | Some "shape_external" -> (
            match token_content t with
            | Some name -> check_external ~externals name
            | None -> Ok ())
        | Some "shape_internalcall" -> (
            match token_content t with
            | Some content -> check_internalcall ~modules ~plugins ~rest content
            | None -> Ok ())
        | Some "shape_garbage" -> garbage_reason ~prev t
        | _ -> Ok ()
      in
      walk ~prev:t ~externals ~modules ~plugins rest

(* Env vars a script may read via [$env.NAME] but never set or hide: forging
   OP_DEPTH/OP_HOST_PATH breaks recursion/PATH handshakes; rewriting PATH lets
   any binary run under an allowlisted external's name. *)
let protected_env = [ "OP_DEPTH"; "OP_HOST_PATH"; "PATH"; "Path" ]

(* Strip single/double/backtick/raw quotes from nu string token content. *)
let strip_quotes s =
  let n = String.length s in
  let quoted q = n >= 2 && s.[0] = q && s.[n - 1] = q in
  if quoted '"' || quoted '\'' || quoted '`' then String.sub s 1 (n - 2)
  else if n >= 3 && s.[0] = 'r' && (s.[1] = '#' || s.[1] = '\'') then
    match (String.index_opt s '\'', String.rindex_opt s '\'') with
    | Some i, Some j when j > i -> String.sub s (i + 1) (j - i - 1)
    | _ -> s
  else s

let assign_ops = [ "="; "+="; "-="; "*="; "/="; "++=" ]

(* Skip cell-path segments ([.0]) after [$env.NAME] to check for assignment. *)
let rec assigned = function
  | t :: rest
    when match token_shape t with
         | Some ("shape_int" | "shape_string") -> true
         | _ -> false ->
      assigned rest
  | t :: _ ->
      token_shape t = Some "shape_operator"
      && Option.fold ~none:false
           ~some:(fun op -> List.mem op assign_ops)
           (token_content t)
  | [] -> false

let env_read ~prev rest =
  (match prev with
    | Some p ->
        token_shape p = Some "shape_variable" && token_content p = Some "$env"
    | None -> false)
  && not (assigned rest)

(* Deny protected env names anywhere except a plain [$env.NAME] read (catches
   assignments, [with-env]/[load-env] record keys, [hide-env], etc.). *)
let check_env_writes tokens =
  let rec go prev = function
    | [] -> Ok ()
    | t :: rest ->
        let* () =
          match (token_shape t, token_content t) with
          | Some ("shape_string" | "shape_raw_string"), Some c
            when List.mem (strip_quotes c) protected_env
                 && not (env_read ~prev rest) ->
              let name = strip_quotes c in
              err_deny
                "`%s` may only be read (`$env.%s`) in the nu tool; setting or \
                 hiding it is denied: %s"
                name name
                (if starts_with ~prefix:"OP_" name then
                   "a nested op would inherit the forged value, resetting op's \
                    recursion guard or its restored PATH."
                 else
                   "the gate vets externals by name, so a rewritten PATH could \
                    run any binary under an allowed name. A nested op restores \
                    the host PATH itself.")
          | _ -> Ok ()
        in
        go (Some t) rest
  in
  go None tokens

let analyze_tokens ~externals ~modules ~plugins ast =
  match ast with
  | `List tokens ->
      let* () = check_env_writes tokens in
      walk ~externals ~modules ~plugins tokens
  | _ -> Ok ()

let analyze_ast ~externals ~modules ~plugins ast =
  Result.map_error refusal_message
    (analyze_tokens ~externals ~modules ~plugins ast)

let used_modules ~modules ast =
  let rec collect = function
    | [] -> []
    | t :: rest -> (
        let tail = collect rest in
        match (token_shape t, token_content t) with
        | Some "shape_internalcall", Some c
          when first_word (unexported c) = "use" -> (
            match use_module rest with
            | Some m when List.mem m modules -> m :: tail
            | _ -> tail)
        | _ -> tail)
  in
  match ast with `List tokens -> collect tokens | _ -> []

(* Catch computed external heads ([^$cmd], [^(...)]) in source text: flatten
   tags a declared variable as inert [shape_variable]. Anchor to command
   positions so regex anchors like ['^(\d+)'] do not match. *)
let computed_head_re = Re.Perl.compile_pat {|(^|[\s|;({\[])\^[$(]|}

let check_computed_head text =
  if Re.execp computed_head_re text then
    err_deny
      "a computed external head (`^$cmd`, `^(...)`) is denied in the nu tool: \
       the gate can only verify literal command names. Call the toolbox \
       command by its bare name."
  else Ok ()

(* Layer 2: dot-file writes via [save] or redirections, alongside
   {!Gate.sensitive_path_re}. *)
let save_dotfile_re =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|\bsave\b\s+(--?\w[\w-]*\s+)*('|"|)?(~/\.|\.[A-Za-z]|[^\s|;&'"]*/\.)|}

let redirect_dotfile_re =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|(\bo|\be|\bout|\berr|\bo\+e)?>>?\s*('|"|)?(~/\.|\.[A-Za-z]|[^\s|;&'"]*/\.)|}

let sensitive_match text =
  List.find_map
    (fun re -> Option.map (fun g -> Re.Group.get g 0) (Re.exec_opt re text))
    [ Gate.sensitive_path_re; save_dotfile_re; redirect_dotfile_re ]

let analyze_source ~ast ~externals ~modules ~plugins text =
  let* () = check_computed_head text in
  match ast text with
  | Error e ->
      err_syntax
        "could not obtain a Nushell AST for the script (%s); fix the syntax \
         and retry"
        e
  | Ok json ->
      let* () = analyze_tokens ~externals ~modules ~plugins json in
      Ok (used_modules ~modules json)

let sensitive_advice =
  "secret-bearing paths and dot-file writes are denied in the nu tool. If the \
   task really needs that file, stop and tell the user."

let sensitive_verdict ~in_script ~sensitive_modules =
  match (in_script, sensitive_modules) with
  | None, [] -> Gate.Allow
  | Some hit, _ ->
      Fmt.kstr
        (fun s -> Gate.Deny s)
        "the nu script names a sensitive path (matched `%s`): %s" hit
        sensitive_advice
  | None, (name, hit) :: _ ->
      Fmt.kstr
        (fun s -> Gate.Deny s)
        "module `%s`, imported by the nu script, names a sensitive path \
         (matched `%s`): %s"
        name hit sensitive_advice

(* Recursively vet imported modules (Layer 1 + Layer 2). The in-progress
   [Ok ()] cache placeholder handles mutual imports. *)
let vet_modules ~analyze ~modules names =
  let cache : (string, (unit, refusal) result) Hashtbl.t = Hashtbl.create 8 in
  let sensitive = ref [] in
  let rec vet_all names =
    List.fold_left
      (fun acc m ->
        let* () = acc in
        vet m)
      (Ok ()) names
  and vet name =
    match Hashtbl.find_opt cache name with
    | Some r -> r
    | None ->
        Hashtbl.replace cache name (Ok ());
        let r =
          match List.assoc_opt name modules with
          | None ->
              err_deny
                "module `%s` was not found in the profile's module dirs at \
                 this call; create the .nu file there and import it on the \
                 next call"
                name
          | Some source when String.length source > max_script_chars ->
              (* Enforce the length cap here too (independent of
                 {!Nu_sandbox.scan_module_sources}) since [scan_modules] is
                 injected. *)
              err_deny
                "module `%s` too long (%d chars > %d): the anti-runaway bound \
                 applies to modules too — split it into smaller modules"
                name (String.length source) max_script_chars
          | Some source ->
              Option.iter
                (fun hit -> sensitive := !sensitive @ [ (name, hit) ])
                (sensitive_match source);
              Result.map_error
                (map_refusal (Fmt.str "vetting module `%s` failed: %s" name))
                (let* uses = analyze source in
                 vet_all uses)
        in
        Hashtbl.replace cache name r;
        r
  in
  (* Sequence explicitly: OCaml tuple evaluation is right-to-left. *)
  let result = vet_all names in
  (result, !sensitive)

let verdict_of_refusal = function
  | Policy reason -> Gate.Deny reason
  | Syntax reason -> Gate.Unparsed reason

let verdict ~ast ~externals ~scan_modules ~plugins =
 fun ~script ->
  if String.length script > max_script_chars then
    Fmt.kstr
      (fun s -> Gate.Deny s)
      "nu script too long (%d chars > %d): split the work into smaller steps"
      (String.length script) max_script_chars
  else
    let modules = scan_modules () in
    let module_names = List.map fst modules in
    let analyze =
      analyze_source ~ast ~externals ~modules:module_names ~plugins
    in
    match analyze script with
    | Error refusal -> verdict_of_refusal refusal
    | Ok uses -> (
        let result, sensitive_modules = vet_modules ~analyze ~modules uses in
        match result with
        | Error refusal -> verdict_of_refusal refusal
        | Ok () ->
            sensitive_verdict ~in_script:(sensitive_match script)
              ~sensitive_modules)
