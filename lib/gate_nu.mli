(** Static AST gate for the [nu] tool.

    Inspects Nushell's flattened token stream ([ast ... --json --flatten])
    before execution. [--flatten] is required because [get block] leaves
    closure/block bodies as opaque integer block-ids, whereas flatten emits
    nested bodies as ordinary shape-tagged tokens.

    Checks:
    - {b Layer 1 (deterministic)}: every [shape_external] must be a bare name in
      the profile's [externals] (no path dispatch); computed heads ([^$cmd],
      [^(...)]) are caught by a source-text scan because flatten tags
      declared-variable heads as inert [shape_variable]. Denied builtins
      ([http], [run-external], [exec], [start], [source], [source-env],
      [overlay use], [extern], [run], [run-internal], [%$cmd] dispatch,
      [config nu|env|reset], [version check], [generate completions], [history],
      file-loading [module], [plugin] other than bare
      [plugin use <profile-plugin>], and [use] other than [std]/[std/*] or
      vetted profile modules) are blocked, with [export] prefixes stripped
      first. [job] is allowed because job closures are flattened and die at
      script exit. Protected env vars ([OP_DEPTH], [OP_HOST_PATH], [PATH],
      [Path]) may only be read as [$env.NAME].
    - {b Module vetting}: profile module dirs are rescanned per call and
      imported modules are recursively vetted (both Layer 1 and Layer 2) with an
      intra-call cycle cache. Module dirs are run-writable (non-dot paths), so
      modules must pass the same checks and length bound as top-level scripts.
    - {b Layer 2 (best-effort text match)}: denies scripts or imported modules
      matching {!Gate.sensitive_path_re} or writing dot-files via [save] /
      redirections. Runtime-assembled paths evade this check.
    - {b Parse errors}: [shape_garbage] and AST failures return {!Gate.Unparsed}
      (blocked, but not counted toward escalation). *)

val max_script_chars : int
(** Maximum character length for a top-level script or vetted module source. *)

val analyze_ast :
  externals:string list ->
  modules:string list ->
  plugins:string list ->
  Yojson.Safe.t ->
  (unit, string) result
(** [analyze_ast ~externals ~modules ~plugins ast] runs the Layer 1 token walk,
    returning [Error reason] on the first offending token. *)

val verdict :
  ast:(string -> (Yojson.Safe.t, string) result) ->
  externals:string list ->
  scan_modules:(unit -> (string * string) list) ->
  plugins:string list ->
  script:string ->
  Gate.verdict
(** [verdict ~ast ~externals ~scan_modules ~plugins ~script] runs length cap,
    computed-head check, AST walk, recursive module vetting, and Layer 2
    sensitive-path checks. *)
