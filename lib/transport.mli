(** HTTPS transport and shared JSON parsing helpers for provider adapters.

    Provides:
    - HTTPS POST transport (TLS, 120s timeout) via [post]
    - JSON parsing helpers shared across adapters *)

type error =
  | Json_error of string
  | Api_error of string
  | Missing_response_field of string * Yojson.Safe.t
  | Invalid_response_field of string * Yojson.Safe.t
  | Unexpected_response of Yojson.Safe.t
  | Http_error of int * string
  | Transport_error of string
  | File_error of string

val pp_error : error -> string
(** [pp_error e] Return a human-readable description of [e]. *)

type post_fn =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (string, error) result
(** [post_fn] A configured HTTP POST. Adapters receive {!val-post} as a closure
    of this type so they stay free of TLS/Eio concerns;
    [Transport.post ~tls_config ~net ~clock] partially applied has this type. *)

(** {2 Parsing helpers shared with the provider adapters} *)

val json_of_string : string -> (Yojson.Safe.t, error) result
(** [json_of_string s] Parse a JSON string, mapping syntax errors to
    {!constructor-Json_error}. *)

val check_error : Yojson.Safe.t -> (unit, error) result
(** [check_error json] Surface a top-level [{ "error": ... }] from a provider
    response. *)

val list_field : string -> Yojson.Safe.t -> (Yojson.Safe.t list, error) result
(** [list_field field json] Read [field] as a JSON array, distinguishing missing
    from malformed. *)

val post :
  tls_config:Tls.Config.client ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (string, error) result
(** [post ~tls_config ~net ~clock ~url ~headers ~body] Shared HTTPS POST
    transport (TLS, 120s timeout). The response body is returned on success;
    non-2xx becomes an {!constructor-Http_error}. Rate limits (429) and server
    errors (5xx) are retried up to 3 attempts with capped exponential backoff,
    honouring a numeric [Retry-After] header when present. The [tls_config]
    should be built once at startup via [tls_config] and reused across calls.
    Callers typically partially apply [tls_config], [net], and [clock] to obtain
    a {!type-post_fn}, which is then handed to adapters. *)

val strip_trailing_slash : string -> string
(** [strip_trailing_slash s] Drop a single trailing ['/'] so [base_url] joins
    cleanly with a path. *)

val tls_config : unit -> (Tls.Config.client, error) result
(** [tls_config ()] Build a TLS client config from the system CA bundle. Exposed
    so callers can cache it across requests. *)

(** Internal hooks for retry-policy tests (no network involved). *)
module For_testing : sig
  val retryable : error -> bool
  (** [retryable e] is [true] when [e] is worth retrying (429s and 5xx), [false]
      for client and transport errors. *)

  val retry_delay : hint:float option -> int -> float
  (** [retry_delay ~hint n] is the backoff delay (seconds) before attempt [n]: a
      server [Retry-After] [hint] when present, else exponential backoff,
      capped. *)

  val with_retries :
    sleep:(float -> unit) ->
    max_attempts:int ->
    (unit -> (string, error) result * float option) ->
    (string, error) result
  (** [with_retries ~sleep ~max_attempts f] runs [f] up to [max_attempts] times,
      calling [sleep] between retryable failures. *)

  val read_body : limit:int -> _ Eio.Flow.source -> (string, int) result
  (** [read_body ~limit flow] reads [flow] to end of input, or [Error limit] if
      it would exceed [limit] bytes. *)

  val max_response_bytes : int
  (** Cap on a successful provider response body. *)

  val max_error_body_bytes : int
  (** Cap on an error response body — much smaller, since these are short by
      construction and get propagated into results. *)
end
