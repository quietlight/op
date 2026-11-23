(** A single global permission gate for op's tool calls.

    op is one-shot with no interactive UI, so the gate's verdicts are shaped for
    an agent that reads and adapts rather than a human who confirms: a {!Deny}
    is a hard block the model sees and can route around, a {!Warn} is steering
    appended to an otherwise-normal result, and a {!Code_red} stops the whole
    run. In practice nearly everything is {!Allow}.

    The policy here is pure and self-contained: path rules for the file tools
    over a small sliding window of recent calls, plus dispatch of [nu] scripts
    to the injected {!type-nu_gate} (see {!Gate_nu} — the static AST gate that
    allowlists externals and denies scripts its layer-2 text check flags). A
    [nu] call with no gate injected fails closed: nothing executable is ever
    allowed by default. Every verdict is deterministic; there is no model in the
    gate.

    This is a cheap, best-effort guardrail for a mostly-cooperative model, not a
    security boundary against a malicious or prompt-injected one — that needs a
    sandbox, which the gate deliberately is not.

    Residual gaps in v1, accepted by design (closing them means a sandbox, not
    more regexes):
    - {b symlink traversal}: paths are normalized textually (no [realpath]), so
      a symlink under an allowed directory pointing at a protected one is not
      caught.
    - {b no working-directory containment}: the path rules only deny
      dot-prefixed segments; [read]/[write]/[edit] on any other path outside
      [cwd] are allowed, so a confused or injected model can touch non-dot files
      anywhere the process itself can reach.
    - {b no per-tool scoping}: the gate is global, not per-agent-persona.
    - {b layer 2 is a substring match}: {!sensitive_path_re} is applied to
      script {e source text}, so it only sees paths written literally. A path
      assembled at runtime — string interpolation, [path join], a variable —
      names no matching literal and is not denied
      ([let a = "mod"; let b = "els"; open $"…/($a)($b).json"] reads the same
      file that the literal spelling routes). Making this sound would mean
      evaluating arbitrary nu statically. It raises the cost of an
      {e incidental} secret read; it does not stop a deliberate one. Layer 1's
      deterministic AST walk is the part to rely on. *)

type verdict =
  | Allow  (** run the tool as normal *)
  | Warn of string
      (** run the tool, then append the warning to its result content *)
  | Deny of string
      (** do not run the tool; return an [is_error] result carrying the reason
      *)
  | Unparsed of string
      (** do not run the tool, exactly as {!Deny}: the nu script could not be
          parsed, so the gate cannot vouch for it. But it is a syntax error, not
          a reach for something blocked, so it is {e not} held against the
          agent: it does not count toward the repeated-denial escalation and
          raises no flagged-call reminder. *)
  | Code_red of string
      (** kill the run: answer every pending call with an error and stop *)

type nu_gate = script:string -> verdict
(** The static AST gate for the [nu] tool (see {!Gate_nu}), applied to the
    current Nushell script. It must never raise. *)

val sensitive_path_re : Re.re
(** The compiled "references a secret-bearing path" pattern (ssh keys, cloud
    credentials, [.env], …). Exposed so {!Gate_nu}'s layer-2 text check denies a
    nu script that names such a path. *)

type t
(** A gate value holding the (mutable) sliding window of recent calls. *)

val v : ?nu:nu_gate -> cwd:string -> unit -> t
(** [v ?nu ~cwd ()] builds a gate rooted at [cwd] (used to resolve relative
    paths). Without a [nu] gate, every [nu] tool call is denied. *)

val check : t -> Llm.tool_call -> verdict
(** [check t tc] computes the verdict for [tc] {e and} records the call in the
    window (so escalation and the one-shot flagged-call reminder see it). *)
