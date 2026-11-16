(* The static AST gate for the [nu] tool (see gate_nu.mli). Layer 1 walks the
   flattened Nushell token stream to enforce the capability boundary (externals
   subset of the toolbox; no network/exec/dispatch builtins). Layer 2 routes
   secret-path and dot-file writes to the shared judge. The token stream is
   obtained through an injected closure so the walk stays pure and unit-testable
   on canned JSON.

   The stream comes from [ast … --json --flatten]: a flat JSON array of tokens,
   each an object [{"content": "curl", "shape": "shape_external", …}]. Flatten is
   what makes the walk closure-safe — it emits the contents of every closure and
   block body ([each { }], [do { }], [if { }], [def f [] { }]) as ordinary
   shape-tagged tokens rather than the opaque integer block-ids that [get block]
   would leave behind. So a denied builtin nested in a closure surfaces as a
   plain [shape_internalcall] token and is caught like any top-level call. *)

let ( let* ) = Result.bind

(* An anti-runaway bound, not a slop cap: nu scripts carrying SQL are
   legitimately long. *)
let max_script_chars = 32_768

(* Builtins that grant a capability the toolbox PATH cannot contain: network
   and arbitrary-program execution. Matched on the first word of a call's head
   so subcommands (`http get`) are covered. `job` is deliberately absent: job
   closures are flattened and walked like any other code, and nu kills
   background jobs when the script exits, so `job spawn` adds concurrency but
   no capability and cannot outlive the tool call. *)
let denied_builtins = [ "http"; "run-external"; "exec"; "start" ]

let first_word s =
  match String.index_opt s ' ' with Some i -> String.sub s 0 i | None -> s

let starts_with ~prefix s =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix

(* Null-safe object lookup: [member] raises on a non-object
   (including [`Null]), and the AST is full of nulls (empty [pipe], absent
   [redirection]), so a raising accessor would crash the generic walk. *)
let member key = function
  | `Assoc fields -> (
      match List.assoc_opt key fields with Some v -> v | None -> `Null)
  | _ -> `Null

let string_member key json =
  match member key json with `String s -> Some s | _ -> None

(* A flattened token's [content] (the source text of this span, e.g. "curl",
   "http get") and [shape] (nu's classification, e.g. "shape_external"). *)
let token_content t = string_member "content" t
let token_shape t = string_member "shape" t
let err_deny fmt = Fmt.kstr (fun s -> Error s) fmt

(* The classic Unix text-tool reflexes, mapped to their nu-builtin
   translations. The deny is the teachable moment: a model reaching for grep
   should get the pipeline to paste into its next call, not the
   network-delegation hint below (right for curl, wrong for grep). *)
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

(* A [shape_external] token: the literal name of an external command. It must be
   a bare toolbox command — a path form (`^/usr/bin/x`, `./x`) bypasses the
   toolbox PATH, and a name outside the toolbox is denied. Computed heads
   (`^$cmd`) never reach here: they flatten to [shape_garbage], handled below.

   A path whose basename is itself allowlisted gets a steering hint: the usual
   reason for dispatching into a build dir is smoke-testing a just-built
   artifact, and the toolbox symlink (resolved through the host PATH at exec
   time) already reaches a rebuilt binary behind the bare name. *)
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

(* The module named by a `use` (or plugin named by a `plugin use`): the first
   non-flag token after it (`use --reload std` -> "std"). None if there is
   nothing to import (a parse error nu would already have rejected, but we fail
   closed). *)
let rec use_module = function
  | t :: rest -> (
      match token_shape t with
      | Some "shape_flag" -> use_module rest
      | _ -> token_content t)
  | [] -> None

(* A [shape_internalcall] token: a builtin/command invocation, e.g. "ls", "http
   get", "overlay use", "use". [rest] is the tokens after it, for `use`'s module
   lookahead. Matched on the first word so subcommands (`http get`)
   are covered. [modules]/[plugins] are the profile's vetted module and plugin
   names. *)
let check_internalcall ~modules ~plugins ~rest content =
  let w = first_word content in
  if List.mem w denied_builtins then
    err_deny
      "`%s` is denied in the nu tool: it reaches the network or runs programs \
       outside the toolbox. Delegate network work via `op --json \"prompt\"` \
       and parse with `from json`."
      w
  else if w = "extern" || starts_with ~prefix:"export extern" content then
    (* `extern` declares a signature for an external, and the call site then
       flattens to a bare shape_internalcall with *no* shape_external token
       anywhere — so the toolbox allowlist in [check_external] never sees the
       name, while nu still runs the program. It is the one construct that
       executes outside the toolbox with nothing for the walk to catch, so deny
       the declaration itself. (Matched on the whole content as well as the
       first word: [first_word "export extern"] is "export".) *)
    err_deny
      "`%s` is denied: it declares an external command the gate cannot see at \
       the call site, which would run a program outside the toolbox. Call the \
       external directly by its literal name instead."
      (if w = "extern" then "extern" else "export extern")
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
    (* Loading a profile plugin is fine (op supplies the registry); loading
       anything else — or naming a registry by hand — is not. *)
    match use_module rest with
    | Some p when List.mem p plugins -> Ok ()
    | _ ->
        err_deny
          "`plugin use` may only load %s; op supplies the plugin registry, so \
           call it as a bare `plugin use <name>`."
          (match plugins with
          | [] -> "nothing in this profile (no plugins configured)"
          | l -> String.concat ", " l)
  else if w = "plugin" then
    err_deny
      "`plugin` subcommands other than `plugin use` are denied: they mutate \
       the plugin registry or execute arbitrary binaries."
  else if w = "use" then
    match use_module rest with
    (* The built-in standard library — [std] itself or a [std/...] submodule.
       A bare [std]-prefixed *file* ([use stdfoo.nu]) would still source unseen
       code, so match [std] exactly or under a '/'. *)
    | Some m when m = "std" || starts_with ~prefix:"std/" m -> Ok ()
    (* A vetted profile module, by exact bare name (scanned names carry no
       path separator, so path forms can never match). *)
    | Some m when List.mem m modules -> Ok ()
    | _ ->
        err_deny
          "`use` may only import the standard library (`use std`)%s; module \
           dirs are rescanned at each call, so create a `.nu` file there and \
           import its bare name on the next call."
          (match modules with
          | [] -> " or a module found in the profile's module dirs at this call"
          | l ->
              " or a module found in the profile's module dirs at this call ("
              ^ String.concat ", " l ^ ")")
  else Ok ()

(* Walk the flat token stream, checking each token that carries a command
   identity and short-circuiting on the first offender. Only three shapes
   matter; every other shape (literals, pipes, closures delimiters, operators)
   is inert and skipped. Because flatten emits closure/block bodies as ordinary
   tokens, a call nested any depth deep is checked exactly like a top-level one. *)
let rec walk ~externals ~modules ~plugins = function
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
        (* Anything nu's parser could not classify flattens to garbage: a
           computed external head (`^$cmd`, `^$"(...)"`) the gate cannot
           verify, but far more often a plain syntax error in the script —
           a hyphenated positional parameter, a stray `$` on a custom-command
           call, an unbalanced paren. Deny either way (never trust the runtime
           PATH lock alone), but say where and steer to the common cause:
           dogfooding showed agents burning whole runs treating their own
           typos as gate policy. *)
        | Some "shape_garbage" ->
            let near =
              match token_content t with
              | Some c when c <> "" -> Fmt.str " near `%s`" c
              | _ -> ""
            in
            err_deny
              "the gate could not parse part of this script%s. That is usually \
               a Nushell syntax error (a hyphenated variable/parameter name — \
               nu wants snake_case, a stray `$` on a custom-command call, an \
               unbalanced paren), or an external with a computed name, which \
               is denied. Fix the syntax or call a literal toolbox command \
               name."
              near
        | _ -> Ok ()
      in
      walk ~externals ~modules ~plugins rest

(* op's cross-process handshake vars: [OP_DEPTH] bounds op->nu->op recursion and
   [OP_HOST_PATH] carries the real PATH a nested op restores (bin/main). A nu
   script that *sets* either — [with-env {OP_DEPTH: "0"} { ^op … }] — hands the
   nested op a forged value, resetting the recursion guard (a cost/DoS runaway)
   or redirecting its PATH. Reading them ([$env.OP_HOST_PATH], to pass PATH
   through) is legitimate and untouched. *)
let protected_handshake = [ "OP_DEPTH"; "OP_HOST_PATH" ]

(* A record/list key strips to its bare name: nu reports a quoted key's
   [content] with the quotes ([{"OP_DEPTH": …}] -> ["OP_DEPTH"]). *)
let strip_quotes s =
  let n = String.length s in
  if
    n >= 2
    && ((s.[0] = '"' && s.[n - 1] = '"') || (s.[0] = '\'' && s.[n - 1] = '\''))
  then String.sub s 1 (n - 2)
  else s

(* Deny a write to a protected handshake var. The write vs. read distinction is
   the preceding token's shape: a record/list *key* follows a [shape_record]
   ({, :) or [shape_list] ([, space) delimiter, whereas a read [$env.OP_DEPTH]
   has the name follow a [shape_variable]. A key whose name is computed
   ([{($"OP_(x)"): …}]) is a residual — it is no longer a literal string token —
   consistent with the gate leaning on Layer 1 for hard guarantees. *)
let check_handshake_writes tokens =
  let is_delim = function
    | Some "shape_record" | Some "shape_list" -> true
    | _ -> false
  in
  let rec go prev_shape = function
    | [] -> Ok ()
    | t :: rest ->
        let* () =
          match (token_shape t, token_content t) with
          | Some "shape_string", Some c
            when is_delim prev_shape
                 && List.mem (strip_quotes c) protected_handshake ->
              err_deny
                "setting the op handshake variable `%s` is denied: a nested op \
                 would inherit the forged value, resetting op's op->nu->op \
                 recursion guard (or its restored PATH). Remove it from the \
                 env record."
                (strip_quotes c)
          | _ -> Ok ()
        in
        go (token_shape t) rest
  in
  go None tokens

let analyze_ast ~externals ~modules ~plugins ast =
  match ast with
  | `List tokens ->
      let* () = check_handshake_writes tokens in
      walk ~externals ~modules ~plugins tokens
  | _ -> Ok ()

(* The vetted-module names a clean token stream imports, for the vetting
   recursion. Only names the walk already allowed can appear (anything else was
   denied), so the membership filter is belt-and-braces. *)
let used_modules ~modules ast =
  let rec collect = function
    | [] -> []
    | t :: rest -> (
        let tail = collect rest in
        match (token_shape t, token_content t) with
        | Some "shape_internalcall", Some c when first_word c = "use" -> (
            match use_module rest with
            | Some m when List.mem m modules -> m :: tail
            | _ -> tail)
        | _ -> tail)
  in
  match ast with `List tokens -> collect tokens | _ -> []

(* A caret head applied to a variable or subexpression (`^$cmd`, `^(...)`,
   `^$"..."`). When the variable is declared, flatten tags it [shape_variable]
   — an inert shape the walk skips — so this must be caught in the source text.
   (Only the *undeclared* case flattens to [shape_garbage].) A computed head
   evades the name check entirely and, when it holds a path, the toolbox PATH
   too. The caret must sit at a command position (start, or after
   whitespace/pipe/semicolon/paren): a regex anchor like [parse --regex
   '^(\d+)'] has a quote before its caret and must not trip this. *)
let computed_head_re = Re.Perl.compile_pat {|(^|[\s|;(])\^[$(]|}

let check_computed_head text =
  if Re.execp computed_head_re text then
    err_deny
      "a computed external head (`^$cmd`, `^(...)`) is denied in the nu tool: \
       the gate can only verify literal command names. Call the toolbox \
       command by its bare name."
  else Ok ()

(* Layer 2, reusing {!Gate}'s shared sensitive-path pattern and judge routing.
   A `save` or redirect
   whose target text is a dot-path is caught alongside the secret-path names —
   nu builtins read/write files with no external call, so the AST walk alone
   would pass them. *)
let save_dotfile_re =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|\bsave\b\s+(--?\w[\w-]*\s+)*('|"|)?(~/\.|\.[A-Za-z]|[^\s|;&'"]*/\.)|}

let redirect_dotfile_re =
  Re.Perl.compile_pat ~opts:[ `Caseless ]
    {|(\bo|\be|\bout|\berr|\bo\+e)?>>?\s*('|"|)?(~/\.|\.[A-Za-z]|[^\s|;&'"]*/\.)|}

let touches_sensitive_path script =
  Re.execp Gate.sensitive_path_re script
  || Re.execp save_dotfile_re script
  || Re.execp redirect_dotfile_re script

(* Layer 1 over one source text: the deterministic checks shared by the
   top-level script and each vetted module — computed-head text scan, AST
   extraction, token walk — returning the clean stream's own module imports for
   the vetting recursion. *)
let analyze_source ~ast ~externals ~modules ~plugins text =
  let* () = check_computed_head text in
  match ast text with
  | Error e ->
      err_deny
        "could not obtain a Nushell AST for the script (%s); fix the syntax \
         and retry"
        e
  | Ok json ->
      let* () = analyze_ast ~externals ~modules ~plugins json in
      Ok (used_modules ~modules json)

(* The judge adjudicates source, so when the sensitive path is in an imported
   module the script alone is not enough material: `use secrets.nu *; peek`
   looks inert. Append the offending module sources under a header so the judge
   sees what `peek` actually does. *)
let judge_command ~script ~sensitive_modules =
  match sensitive_modules with
  | [] -> script
  | mods ->
      let one (name, source) = Fmt.str "# module %s\n%s" name source in
      Fmt.str "%s\n\n# --- imported module sources under review ---\n%s" script
        (String.concat "\n" (List.map one mods))

(* [in_script] and [sensitive_modules] are the two places layer 2 can fire; the
   reason names whichever tripped so the deny the model reads points at the file
   to change. *)
let adjudicate_sensitive ~judge ~window ~script ~in_script ~sensitive_modules =
  let command = judge_command ~script ~sensitive_modules in
  let names = String.concat ", " (List.map fst sensitive_modules) in
  match (in_script, sensitive_modules) with
  | false, [] -> Gate.Allow
  | true, [] ->
      Gate.adjudicate ~judge ~window ~command
        "the nu script reads or writes a sensitive path"
  | false, _ ->
      Fmt.kstr
        (Gate.adjudicate ~judge ~window ~command)
        "module %s, imported by the nu script, reads or writes a sensitive path"
        names
  | true, _ ->
      Fmt.kstr
        (Gate.adjudicate ~judge ~window ~command)
        "the nu script and module %s read or write a sensitive path" names

(* Vet the transitive closure of the [names]'d modules, each re-read from disk
   for this call so the bytes Layer 1 audits are the bytes nu will parse. The
   per-call [cache] memoises repeated imports and its in-progress [Ok ()]
   placeholder guards mutually-importing modules. Returns the vetting result and
   the module sources that tripped Layer 2, which the judge payload carries —
   because a top-level [use secrets.nu *; peek] shows the judge nothing on its
   own.

   Layer 2 runs over module sources as well as the top-level script. Module dirs
   are not the user-authored-only artifacts they read as: the nu tool can [save]
   into them (they are non-dot paths, so [save_dotfile_re] does not fire), so a
   script with no tool but [nu] can write a module and import it. Scanning only
   the top level made the verdict depend on *where* a line of nu was written
   rather than what it did. *)
let vet_modules ~analyze ~modules names =
  let cache : (string, (unit, string) result) Hashtbl.t = Hashtbl.create 8 in
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
              (* The anti-runaway bound is the top-level script's; a module is a
                 script you import, so it obeys the same cap. Without this the
                 gate feeds an unbounded module to `nu ast` and appends an
                 unbounded body to the judge payload — module dirs are
                 run-writable, so the size is attacker-chosen. Production also
                 skips oversized files before they are read (bin/main's
                 [scan_module_sources]); this is the gate's own invariant, since
                 [scan_modules] is injected and not trusted to bound its input. *)
              err_deny
                "module `%s` too long (%d chars > %d): the anti-runaway bound \
                 applies to modules too — split it into smaller modules"
                name (String.length source) max_script_chars
          | Some source ->
              if touches_sensitive_path source then
                sensitive := !sensitive @ [ (name, source) ];
              Result.map_error
                (fun e -> Fmt.str "vetting module `%s` failed: %s" name e)
                (let* uses = analyze source in
                 vet_all uses)
        in
        Hashtbl.replace cache name r;
        r
  in
  (* [let]-sequence, not a tuple: a tuple evaluates right-to-left, sampling
     [sensitive] before [vet_all] has populated it. *)
  let result = vet_all names in
  (result, !sensitive)

let verdict ~ast ~externals ~scan_modules ~plugins ~judge =
 fun ~window ~script ->
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
    | Error reason -> Gate.Deny reason
    | Ok uses -> (
        let result, sensitive_modules = vet_modules ~analyze ~modules uses in
        match result with
        | Error reason -> Gate.Deny reason
        | Ok () ->
            adjudicate_sensitive ~judge ~window ~script
              ~in_script:(touches_sensitive_path script)
              ~sensitive_modules)
