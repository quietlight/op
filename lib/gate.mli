(** Deterministic permission gate for tool calls.

    Enforces path rules for file tools over a sliding window of recent calls,
    and delegates [nu] scripts to the injected {!type-nu_gate} ({!Gate_nu}).
    Without a [nu] gate, [nu] calls fail closed.

    This is a guardrail for a cooperative model, not a sandbox against a hostile
    or prompt-injected one. Accepted v1 gaps:
    - {b Symlinks}: paths are normalized textually (no [realpath]).
    - {b No cwd containment}: file tools may access non-dot, non-sensitive paths
      outside [cwd].
    - {b Global scope}: no per-tool or per-persona policy.
    - {b Substring Layer 2}: {!sensitive_path_re} matches script source text, so
      runtime-assembled paths evade it; Layer 1's AST walk is the deterministic
      boundary. *)

type verdict =
  | Allow  (** Run the tool normally. *)
  | Warn of string  (** Run the tool and append the warning to its output. *)
  | Deny of string  (** Block execution and return an error result. *)
  | Unparsed of string
      (** Block execution like {!Deny}, but do not count toward repeated-denial
          escalation or trigger reminders (syntax error, not policy violation).
      *)
  | Code_red of string
      (** Terminate the run and answer all pending calls with errors. *)

type nu_gate = script:string -> verdict
(** Static AST gate for [nu] scripts ({!Gate_nu}); must not raise. *)

val sensitive_path_re : Re.re
(** Pattern matching secret-bearing paths (ssh keys, cloud credentials, [.env],
    [/proc], [~/.op_global], etc.), shared by file-tool path rules and
    {!Gate_nu} Layer 2. *)

type t
(** Gate state holding the sliding window of recent calls. *)

val v : ?nu:nu_gate -> cwd:string -> unit -> t
(** [v ?nu ~cwd ()] creates a gate rooted at [cwd]. Omitting [nu] denies all
    [nu] calls. *)

val check : t -> Llm.tool_call -> verdict
(** [check t tc] evaluates [tc] and records its base verdict in the window. *)
