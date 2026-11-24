type error =
  | Json_error of string
  | Api_error of string
  | Missing_response_field of string * Yojson.Safe.t
  | Invalid_response_field of string * Yojson.Safe.t
  | Unexpected_response of Yojson.Safe.t
  | Http_error of int * string
  | Transport_error of string
  | File_error of string

let pp_error = function
  | Json_error msg -> "invalid JSON response: " ^ msg
  | Api_error msg -> "API error: " ^ msg
  | Missing_response_field (field, _) ->
      Fmt.str "API response is missing field %s" field
  | Invalid_response_field (field, _) ->
      Fmt.str "API response field %s has an unexpected shape" field
  | Unexpected_response json ->
      Fmt.str "Unexpected API response: %s" (Yojson.Safe.to_string json)
  | Http_error (status, body) ->
      Fmt.str "HTTP request failed with status %d: %s" status body
  | Transport_error msg -> "HTTP transport error: " ^ msg
  | File_error msg -> msg

let err_file_error fmt = Fmt.kstr (fun s -> Error (File_error s)) fmt
let err_transport_error fmt = Fmt.kstr (fun s -> Error (Transport_error s)) fmt

type post_fn =
  url:string ->
  headers:(string * string) list ->
  body:string ->
  (string, error) result

let list_field field json =
  let open Yojson.Safe.Util in
  match member field json with
  | `Null -> Error (Missing_response_field (field, json))
  | `List values -> Ok values
  | _ -> Error (Invalid_response_field (field, json))

let check_error json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Null -> Ok ()
  | err ->
      let err_msg =
        match err |> member "message" with
        | `String s -> s
        | _ -> Yojson.Safe.to_string err
      in
      Error (Api_error err_msg)

let json_of_string json_str =
  match Yojson.Safe.from_string json_str with
  | json -> Ok json
  | exception Yojson.Json_error msg -> Error (Json_error msg)

let ( let* ) = Result.bind

(* ------------------------------------------------------------------ *)
(* Transport                                                          *)
(* ------------------------------------------------------------------ *)

let strip_trailing_slash s =
  if String.ends_with ~suffix:"/" s then String.sub s 0 (String.length s - 1)
  else s

let tls_authenticator () =
  match Ca_certs.authenticator () with
  | Ok authenticator -> Ok authenticator
  | Error (`Msg msg) ->
      err_file_error
        "could not load a system CA bundle for HTTPS certificate validation: %s"
        msg

let tls_config () =
  let* authenticator = tls_authenticator () in
  match Tls.Config.client ~authenticator () with
  | Ok config -> Ok config
  | Error (`Msg msg) -> Error (Transport_error msg)

let tls_host uri =
  match Option.map Domain_name.of_string (Uri.host uri) with
  | Some (Ok name) -> Domain_name.host name |> Result.to_option
  | Some (Error _) | None -> None

let https_wrapper tls_config uri flow =
  Tls_eio.client_of_flow tls_config ?host:(tls_host uri) flow

let request_timeout = 300.0

(* Translate *_PROXY env vars into a one-time cohttp-eio global proxy config
   (e.g. for sandboxes like srt with proxy-only egress). Proxy URL userinfo is
   lifted into Proxy-Authorization since cohttp-eio does not derive it. *)
let proxies_configured = Atomic.make false

let proxy_env names =
  List.find_map
    (fun n ->
      match Sys.getenv_opt n with
      | Some v when String.trim v <> "" -> Some (String.trim v)
      | _ -> None)
    names

let proxy_auth_header uris =
  List.find_map
    (fun u ->
      match Option.bind u Uri.userinfo with
      | None -> None
      | Some info ->
          let user, pass =
            match String.index_opt info ':' with
            | Some i ->
                ( String.sub info 0 i,
                  String.sub info (i + 1) (String.length info - i - 1) )
            | None -> (info, "")
          in
          Some
            (Cohttp.Header.init_with "Proxy-Authorization"
               (Cohttp.Auth.string_of_credential (`Basic (user, pass)))))
    uris

(* cohttp-eio only supports HTTP CONNECT proxies; warn on unsupported schemes
   rather than silently bypassing egress control. *)
let validate_proxy_scheme name u =
  match Uri.scheme u with
  | Some ("http" | "https") -> Some u
  | other ->
      Fmt.epr "warning: ignoring %s: unsupported proxy scheme %s@." name
        (Option.value other ~default:"(none)");
      None

let configure_proxies () =
  if Atomic.compare_and_set proxies_configured false true then begin
    let uri names =
      match (names, proxy_env names) with
      | name :: _, Some v -> validate_proxy_scheme name (Uri.of_string v)
      | _ -> None
    in
    let http_proxy = uri [ "HTTP_PROXY"; "http_proxy" ] in
    let https_proxy = uri [ "HTTPS_PROXY"; "https_proxy" ] in
    let all_proxy = uri [ "ALL_PROXY"; "all_proxy" ] in
    let no_proxy_patterns = proxy_env [ "NO_PROXY"; "no_proxy" ] in
    let proxy_headers =
      proxy_auth_header [ https_proxy; http_proxy; all_proxy ]
    in
    (* Drop consumed userinfo and pin [localhost] to 127.0.0.1: srt binds IPv4,
       and macOS resolves [localhost] to ::1 first without cohttp fallback. *)
    let pin_loopback u =
      match Uri.host u with
      | Some "localhost" -> Uri.with_host u (Some "127.0.0.1")
      | _ -> u
    in
    let clean = Option.map (fun u -> pin_loopback (Uri.with_userinfo u None)) in
    let all_proxy = clean all_proxy in
    let scheme_proxies =
      List.filter_map Fun.id
        [
          Option.map (fun p -> ("http", p)) (clean http_proxy);
          Option.map (fun p -> ("https", p)) (clean https_proxy);
        ]
    in
    if scheme_proxies <> [] || Option.is_some all_proxy then
      Cohttp_eio.Client.set_proxies ?no_proxy_patterns ?default_proxy:all_proxy
        ?proxy_headers ~scheme_proxies ()
  end

(* Cap response reads to prevent OOM from unbounded bodies. *)
let max_response_bytes = 32 * 1024 * 1024
let max_error_body_bytes = 256 * 1024

(* Allow 1 byte of headroom so an exact-[limit] body reaches EOF cleanly. *)
let read_body ~limit flow =
  let buf = Eio.Buf_read.of_flow ~max_size:(limit + 1) flow in
  match Eio.Buf_read.take_all buf with
  | s when String.length s > limit -> Error limit
  | s -> Ok s
  | exception Eio.Buf_read.Buffer_limit_exceeded -> Error limit

(* Single attempt; returns optional numeric Retry-After seconds on error. *)
let post_once ~tls_config ~net ~clock ~url ~headers ~body :
    (string, error) result * float option =
  try
    configure_proxies ();
    Eio.Switch.run @@ fun sw ->
    let client =
      Cohttp_eio.Client.make ~https:(Some (https_wrapper tls_config)) net
    in
    let uri = Uri.of_string url in
    let headers = Cohttp.Header.of_list headers in
    let request_body = Cohttp_eio.Body.of_string body in
    Eio.Time.with_timeout_exn clock request_timeout @@ fun () ->
    let response, response_body =
      Cohttp_eio.Client.post client ~sw ~headers ~body:request_body uri
    in
    let status =
      Cohttp.Response.status response |> Cohttp.Code.code_of_status
    in
    let ok = Cohttp.Code.is_success status in
    let limit = if ok then max_response_bytes else max_error_body_bytes in
    match read_body ~limit response_body with
    | Error limit ->
        ( err_transport_error
            "provider response body exceeded the %d-byte cap (HTTP %d)" limit
            status,
          None )
    | Ok response_body ->
        if ok then (Ok response_body, None)
        else
          let retry_after =
            (* HTTP-date form is rare from LLM providers; ignore non-numeric. *)
            Cohttp.Header.get (Cohttp.Response.headers response) "retry-after"
            |> Fun.flip Option.bind float_of_string_opt
          in
          (Error (Http_error (status, response_body)), retry_after)
  with
  | Eio.Time.Timeout ->
      (err_transport_error "request timed out after %.0fs" request_timeout, None)
  | Eio.Io (err, context) ->
      ( err_transport_error "%a: %s" Eio.Exn.pp_err err
          (Printexc.to_string (Eio.Io (err, context))),
        None )
  | Tls_eio.Tls_alert alert ->
      ( Error
          (Transport_error
             ("TLS alert: " ^ Tls.Packet.alert_type_to_string alert)),
        None )
  | Tls_eio.Tls_failure failure ->
      (err_transport_error "TLS failure: %a" Tls.Engine.pp_failure failure, None)
  | Failure msg -> (Error (Transport_error msg), None)

(* ------------------------------------------------------------------ *)
(* Retries                                                            *)
(* ------------------------------------------------------------------ *)

let max_attempts = 3
let max_delay = 30.0

let retryable = function
  | Http_error (429, _) -> true
  | Http_error (status, _) -> status >= 500
  | _ -> false

let retry_delay ~hint n =
  let backoff = Float.pow 2.0 (float_of_int n) in
  Float.min max_delay (Option.value hint ~default:backoff)

let with_retries ~sleep ~max_attempts f =
  let rec attempt n =
    match f () with
    | (Error e as result), hint ->
        if n + 1 >= max_attempts || not (retryable e) then result
        else (
          sleep (retry_delay ~hint n);
          attempt (n + 1))
    | (Ok _ as result), _ -> result
  in
  attempt 0

let post ~tls_config ~net ~clock ~url ~headers ~body =
  with_retries ~sleep:(Eio.Time.sleep clock) ~max_attempts (fun () ->
      post_once ~tls_config ~net ~clock ~url ~headers ~body)

module For_testing = struct
  let retryable = retryable
  let retry_delay = retry_delay
  let with_retries = with_retries
  let read_body = read_body
  let max_response_bytes = max_response_bytes
  let max_error_body_bytes = max_error_body_bytes
end
