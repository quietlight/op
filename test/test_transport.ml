let scripted_retries ?(max_attempts = 3) outcomes =
  let remaining = ref outcomes in
  let calls = ref 0 in
  let sleeps = ref [] in
  let f () =
    incr calls;
    match !remaining with
    | [] -> Alcotest.fail "retry loop called more times than scripted"
    | x :: rest ->
        remaining := rest;
        x
  in
  let sleep d = sleeps := d :: !sleeps in
  let result = Op.Transport.For_testing.with_retries ~sleep ~max_attempts f in
  (result, !calls, List.rev !sleeps)

let test_retry_eventually_succeeds () =
  let rate_limited =
    (Error (Op.Transport.Http_error (429, "slow down")), None)
  in
  let result, calls, sleeps =
    scripted_retries [ rate_limited; rate_limited; (Ok "done", None) ]
  in
  Alcotest.(check int) "three attempts" 3 calls;
  Alcotest.(check (list (float 0.001))) "backoff doubles" [ 1.0; 2.0 ] sleeps;
  match result with
  | Ok "done" -> ()
  | Ok s -> Alcotest.fail ("unexpected body: " ^ s)
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)

let test_retry_gives_up () =
  let server_error = (Error (Op.Transport.Http_error (503, "down")), None) in
  let result, calls, _ =
    scripted_retries [ server_error; server_error; server_error ]
  in
  Alcotest.(check int) "stops at max_attempts" 3 calls;
  match result with
  | Error (Op.Transport.Http_error (503, _)) -> ()
  | Error e -> Alcotest.fail (Op.Transport.pp_error e)
  | Ok _ -> Alcotest.fail "expected error after exhausting retries"

let test_retry_client_not_retried () =
  let bad_request = (Error (Op.Transport.Http_error (400, "bad")), None) in
  let result, calls, _ = scripted_retries [ bad_request ] in
  Alcotest.(check int) "single attempt" 1 calls;
  match result with
  | Error (Op.Transport.Http_error (400, _)) -> ()
  | _ -> Alcotest.fail "expected the 400 to surface unchanged"

let test_retry_honours_retry_after () =
  let rate_limited =
    (Error (Op.Transport.Http_error (429, "slow down")), Some 7.5)
  in
  let _, _, sleeps = scripted_retries [ rate_limited; (Ok "done", None) ] in
  Alcotest.(check (list (float 0.001))) "server hint wins" [ 7.5 ] sleeps

let test_retry_delay_capped () =
  Alcotest.(check (float 0.001))
    "hint capped" 30.0
    (Op.Transport.For_testing.retry_delay ~hint:(Some 3600.0) 0);
  Alcotest.(check (float 0.001))
    "backoff capped" 30.0
    (Op.Transport.For_testing.retry_delay ~hint:None 10)

let test_retry_transport_not_retried () =
  Alcotest.(check bool)
    "transport errors are not retryable" false
    (Op.Transport.For_testing.retryable (Op.Transport.Transport_error "reset"))

let read_body ~limit s =
  Op.Transport.For_testing.read_body ~limit (Eio.Flow.string_source s)

let test_read_body_under_limit () =
  match read_body ~limit:100 "hello" with
  | Ok s -> Alcotest.(check string) "body read whole" "hello" s
  | Error _ -> Alcotest.fail "expected a short body to be read"

let test_read_body_at_limit () =
  match read_body ~limit:5 "hello" with
  | Ok s -> Alcotest.(check string) "exact fit is not over" "hello" s
  | Error _ -> Alcotest.fail "expected an exact-fit body to be read"

let test_read_body_over_limit () =
  match read_body ~limit:4 "hello" with
  | Error limit -> Alcotest.(check int) "reports the cap" 4 limit
  | Ok _ -> Alcotest.fail "expected the cap to reject an oversize body"

let test_error_body_cap_is_tighter () =
  Alcotest.(check bool)
    "error bodies capped below success bodies" true
    (Op.Transport.For_testing.max_error_body_bytes
   < Op.Transport.For_testing.max_response_bytes)

let suite =
  ( "transport",
    [
      ("read_body under limit", `Quick, test_read_body_under_limit);
      ("read_body at limit", `Quick, test_read_body_at_limit);
      ("read_body over limit", `Quick, test_read_body_over_limit);
      ("error body cap is tighter", `Quick, test_error_body_cap_is_tighter);
      ("retry eventually succeeds", `Quick, test_retry_eventually_succeeds);
      ("retry gives up", `Quick, test_retry_gives_up);
      ("retry skips client errors", `Quick, test_retry_client_not_retried);
      ("retry honours retry-after", `Quick, test_retry_honours_retry_after);
      ("retry delay capped", `Quick, test_retry_delay_capped);
      ("retry skips transport errors", `Quick, test_retry_transport_not_retried);
    ] )
