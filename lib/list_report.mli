(** Pure JSON renderers for [op --list --json].

    Each function returns a single compact JSON line for stdout. Empty results
    are a success with an empty array — never an error; callers signal failures
    with {!error}. [bin/main.ml] owns data fetching, the human-readable text
    path, stderr diagnostics, and exit codes, so these renderers are
    side-effect-free and unit-testable.

    Field names are camelCase to match the [--json] prompt-run object (see
    [Run_report]): [sessionId], [parentId], [isFork], etc. *)

val error : ?hint:string -> string -> string
(** [error ?hint msg] is the failure envelope [{"error":msg,"isError":true}],
    with an optional human hint. *)

val sessions : Session_list.summary list -> string
(** [sessions xs] renders the [sessions] envelope; [xs] may be empty. *)

val messages : session_id:string -> Session_list.entry_summary list -> string
(** [messages ~session_id xs] renders the [messages] envelope for one session;
    [xs] may be empty. *)

val branches : session_id:string -> Session_list.branch_tip list -> string
(** [branches ~session_id xs] renders the [branches] envelope for one session,
    one [{tip,role,preview,when}] per branch tip. [when] is the tip entry's
    timestamp; [tip] is the id to pass as [--parentId]. *)

val models : (string * string) list -> string
(** [models pairs] renders the [models] envelope, one [{provider,model}] per
    [(provider, model)] pair, in the order given (caller flattens and applies
    [--limit] so the JSON and text paths agree). *)

val default : provider:string -> model:string -> string
(** [default ~provider ~model] renders the [default] envelope. The
    no-default-configured case is the caller's responsibility (use {!error}). *)
