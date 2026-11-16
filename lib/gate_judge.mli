(** The LLM fallback for {!Gate}: builds a {!Gate.judge} closure from config.

    This is the only I/O-bearing piece of the gate. It is a last resort by
    design — only the rare suspicious-bash command reaches the judge — so its
    cost stays near zero. The judge fails closed: any transport error, parse
    failure, or unknown verdict blocks the command. It reports those as
    {!Gate.constructor-Fault} rather than {!Gate.constructor-Deny}, so the gate
    can tell "the judge did not decide" from "the judge said no" — see
    {!Gate.type-verdict}. An off-schema reply is retried once before it becomes
    a fault; a transport error is not. It never raises. When a {!Gate_log.t} is
    supplied, every judge call — payload, raw response, final verdict — is
    recorded there for post-hoc audit. *)

type resolution =
  | Not_configured  (** no [gateModel] set; the gate fails closed silently *)
  | Unusable of string
      (** [gateModel] is set but cannot be used; the string says why *)
  | Ready of Gate.judge  (** a usable judge *)

val resolve :
  ?log:Gate_log.t -> Config.t -> post:Transport.post_fn -> resolution
(** [resolve ?log config ~post] interprets the [gateModel] spec in [config]. It
    distinguishes an unset gate ({!constructor-Not_configured}) from a
    set-but-broken one ({!constructor-Unusable} — bad spec, unknown provider, or
    missing [apiKey]) so the caller can warn instead of silently failing closed,
    which is otherwise indistinguishable at the CLI. [log], when given, receives
    every call the judge makes. *)

val of_config :
  ?log:Gate_log.t -> Config.t -> post:Transport.post_fn -> Gate.judge option
(** [of_config ?log config ~post] is {!val-resolve} reduced to [Some judge] when
    {!constructor-Ready} and [None] otherwise (unset, missing/unknown provider,
    or no [apiKey]). When [None], the gate fails closed on suspicious commands.
*)

(** Internal hooks for parser-focused tests. *)
module For_testing : sig
  val parse_verdict : string -> Gate.verdict
  (** [parse_verdict text] maps a judge model's response to a verdict,
      tolerating surrounding prose / markdown code fences; unparseable or
      unknown output becomes {!Gate.constructor-Fault}. *)
end
