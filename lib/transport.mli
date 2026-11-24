(** HTTPS POST transport (TLS, 300s timeout) and shared JSON parsing helpers for
    provider adapters. *)

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
(** [pp_error e] formats [e] for display. *)

type post_fn =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (string, error) result
(** Configured HTTP POST closure passed to adapters. *)

(** {2 Parsing helpers} *)

val json_of_string : string -> (Yojson.Safe.t, error) result
(** [json_of_string s] parses JSON, mapping syntax errors to
    {!constructor-Json_error}. *)

val check_error : Yojson.Safe.t -> (unit, error) result
(** [check_error json] fails with {!constructor-Api_error} if [json] has a
    top-level ["error"] field. *)

val list_field : string -> Yojson.Safe.t -> (Yojson.Safe.t list, error) result
(** [list_field field json] reads [field] as a JSON array, distinguishing
    missing from malformed. *)

val post :
  tls_config:Tls.Config.client ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (string, error) result
(** [post ~tls_config ~net ~clock ~url ~headers ~body] performs an HTTPS POST
    (300s timeout). Retries 429 and 5xx up to 3 attempts with capped exponential
    backoff, honoring numeric [Retry-After]. *)

val strip_trailing_slash : string -> string
(** [strip_trailing_slash s] drops a single trailing ['/']. *)

val tls_config : unit -> (Tls.Config.client, error) result
(** [tls_config ()] builds a TLS client config from the system CA bundle. *)

(** Internal hooks for retry and body-limit tests. *)
module For_testing : sig
  val retryable : error -> bool
  (** [retryable e] is [true] for 429 and 5xx HTTP errors. *)

  val retry_delay : hint:float option -> int -> float
  (** [retry_delay ~hint n] returns backoff delay in seconds before attempt [n],
      preferring [hint] and capped. *)

  val with_retries :
    sleep:(float -> unit) ->
    max_attempts:int ->
    (unit -> (string, error) result * float option) ->
    (string, error) result
  (** [with_retries ~sleep ~max_attempts f] runs [f] up to [max_attempts] times,
      calling [sleep] between retryable failures. *)

  val read_body : limit:int -> _ Eio.Flow.source -> (string, int) result
  (** [read_body ~limit flow] reads [flow] up to [limit] bytes or returns
      [Error limit]. *)

  val max_response_bytes : int
  (** Byte cap on a successful response body. *)

  val max_error_body_bytes : int
  (** Byte cap on an error response body. *)
end
