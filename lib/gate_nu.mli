(** The static AST gate for the [nu] tool.

    op runs Nushell scripts under a per-run toolbox PATH holding only a small
    set of allowed external commands. Nushell parses statically (no [eval]) and
    every command appears in its flattened token stream ([ast … --flatten]) as a
    shape-tagged token, so a deterministic walk can decide — {e before} the
    script runs — whether it stays within the toolbox and never reaches the
    network or an out-of-toolbox program. That walk is the capability boundary;
    a secondary text check denies the file reads/writes the AST cannot classify.
    The gate is deterministic end to end: no model is consulted.

    The stream is flattened rather than the structured [get block] tree on
    purpose: [get block] serializes closure/block bodies as opaque integer
    block-ids, so a denied command nested in [each { }], [do { }], [if { }] or
    [def f [] { }] would be invisible to the walk. Flatten emits every nested
    body as ordinary tokens, so a call at any depth is checked like a top-level
    one.

    Two layers:
    - {b Layer 1 — token walk} (deterministic): every [shape_external] token
      must name a command in the toolbox allowlist and must not be a path (path
      dispatch bypasses the toolbox PATH); a computed external head ([^$cmd],
      [^(...)]) is denied — via a source-text scan, because flatten tags a
      {e declared}-variable head as inert [shape_variable] (only the undeclared
      case surfaces as [shape_garbage]); and every [shape_internalcall] whose
      first word is a network/exec/dispatch builtin ([http], [run-external],
      [exec], [start], [source], [source-env], [overlay use], [plugin] except
      [plugin use] of a profile plugin, or [use] of anything but the standard
      library or a vetted profile module) is denied outright. [job] is allowed:
      job closures are flattened and walked like any other code, and nu kills
      background jobs at script exit, so they add no capability and cannot
      outlive the tool call.
    - {b Module vetting}: module dirs are scanned on every call, and a [use] of
      a profile module is allowed only after its freshly-read source passes the
      same Layer-1 checks (recursively, for modules importing modules). The
      audited bytes are therefore the bytes nu parses, including modules
      authored during the run; a per-call cache handles repeated imports and
      cycles.
    - {b Layer 2 — sensitive-path text check}: nu's [open]/[save] read and write
      files with no external call, so a script naming a secret-bearing path
      (shared with {!Gate.sensitive_path_re}) or writing a dot-file is denied
      rather than passing on a clean AST walk alone. Applied to the top-level
      script {e and} to every vetted module source; the deny names whichever
      tripped it and quotes the matching text.

    {b Module dirs are a trust boundary the run can write to.} They are not the
    user-authored-only artifacts they read as: they are ordinary non-dot paths,
    so the nu tool can [save] a module into one and import it on the next call
    without the [write] tool being enabled at all — excluding [write] from
    [--tools] does not contain this. That is why Layer 2 covers module sources:
    scanning only the top level made the verdict depend on {e where} a line of
    nu was written rather than on what it did.

    Layer 2's limits are {!Gate}'s: it is a substring match over source text, so
    a runtime-assembled path evades it in a module exactly as it does at the top
    level. Covering modules removes an asymmetry; it does not make the check
    sound.

    Every deny names the offending command and says what to do instead: the
    persona has no confirmation UI, so the message {e is} the steering. *)

val max_script_chars : int
(** The anti-runaway character bound. It caps the top-level script and, applied
    identically, each vetted module source — a module is a script you import, so
    it obeys the same limit. Module dirs are run-writable, so an unbounded
    module would otherwise let a runaway script inflate the [nu ast] input past
    the bound the top-level script respects. *)

val analyze_ast :
  externals:string list ->
  modules:string list ->
  plugins:string list ->
  Yojson.Safe.t ->
  (unit, string) result
(** [analyze_ast ~externals ~modules ~plugins ast] is layer 1's token walk:
    return [Ok ()] when every external command is in [externals] and the script
    uses no denied builtin (at any nesting depth) — [modules] and [plugins]
    being the profile's vetted module and plugin names that [use] / [plugin use]
    may import — or [Error reason] naming the first offending command. Pure; the
    committed flatten fixtures under [test/fixtures/nu] drive its tests. *)

val verdict :
  ast:(string -> (Yojson.Safe.t, string) result) ->
  externals:string list ->
  scan_modules:(unit -> (string * string) list) ->
  plugins:string list ->
  script:string ->
  Gate.verdict
(** [verdict ~ast ~externals ~scan_modules ~plugins ~script] is the full gate
    closure injected into {!Gate.v}. For every call, [scan_modules ()] supplies
    fresh module-name/source pairs; imported modules are vetted recursively with
    an intra-call cache, ensuring the gate audits the bytes nu will parse. It
    also caps runaway scripts, denies computed external heads, obtains the AST
    via [ast], runs {!analyze_ast}, and denies a script or imported module that
    names a sensitive path. *)
