open Chat

let ( let* ) = Lwt.bind

(** Equality function for headers *)
let equal_header (h1 : Protocol.Header.t) (h2 : Protocol.Header.t) =
  h1.request_method = h2.request_method && h1.size = h2.size

(** Alcotest testable for header *)
let header_testable = Alcotest.testable Protocol.Header.pp equal_header

let equal_error (e1 : Errors.t) (e2 : Errors.t) = e1 = e2
let errors_testable = Alcotest.testable Errors.pp equal_error
let result_header_testable = Alcotest.(result header_testable errors_testable)

let random_string length =
  let chars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@##$$%^&*()><?\"{}"
  in
  let chars_len = String.length chars in
  String.init length (fun _ -> chars.[Random.int chars_len])

let test_protocol_read expected_body _ () =
  let read () =
    let* () = Lwt_io.printf "%s\n" expected_body in
    (* Create test headers *)
    let header = Protocol.Header.init_msg_header 1 expected_body in
    let message = Protocol.Message.init header expected_body in

    let r, w = Lwt_io.pipe () in

    (* Sends message to input buffer*)
    let* () = Lwt_io.write w @@ Protocol.Message.marshal message in
    let* () = Lwt_io.close w in

    (* Test read *)
    let* result = Chat.Protocol.read r in
    match result with
    | Ok r -> Lwt.return r.body
    | Error e -> Lwt.return @@ Errors.to_string e
  in
  let* actual = read () in
  Alcotest.(check string) "should be equal" expected_body actual;
  Lwt.return ()

let test_protocol_invalid_read input expected_err _ () =
  let read () =
    let r, w = Lwt_io.pipe () in

    let* () = Lwt_io.write w input in
    let* () = Lwt_io.close w in

    let* result = Chat.Protocol.read r in

    match result with
    | Ok _ -> Lwt.return @@ Errors.ERR "Should not work"
    | Error e -> Lwt.return e
  in
  let* actual = read () in
  Alcotest.(check errors_testable) "should equal" expected_err actual;
  Lwt.return ()

(* Test Cases *)
let test_parse_headers_valid_input () =
  let input = "5 MSG 1\n12345" in
  let expected =
    Ok { Protocol.Header.request_method = MSG 1; Protocol.Header.size = 5 }
  in
  let actual = Protocol.Header.of_string input in
  Alcotest.check result_header_testable "Valid input" expected actual

let test_parse_headers_empty_input () =
  let input = "" in
  let expected = Error Errors.EOF in
  let actual = Protocol.Header.of_string input in
  Alcotest.check result_header_testable "Empty input" expected actual

[@@@ocamlformat "disable"]

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ Alcotest_lwt.run "Protocol Test"
       [
         ( "Protocol MSG - Valid reads",
           [
             test_case "Normal" `Quick (test_protocol_read "I love Ocaml");
             test_case "Empty String" `Quick (test_protocol_read "");
             test_case "Long string" `Quick (test_protocol_read (random_string 10000));
           ] 
          );
          
         ( "Protocol MSG - Invalid reads",
           [
              test_case "Invalid Request Method" `Quick (test_protocol_invalid_read "5 POST" (Errors.ERR "Invalid request method"));
              test_case "Invalid Request Method" `Quick (test_protocol_invalid_read "POST" Errors.MALFORMED);
              test_case "Size and body length mismatch" `Quick (test_protocol_invalid_read "5 MSG 1\n" Errors.MALFORMED);
              test_case "Invalid body - mismatched size" `Quick (test_protocol_invalid_read "5 MSG 2\nHi" Errors.MALFORMED);
              test_case "Empty string" `Quick (test_protocol_invalid_read "" Errors.EOF);
              test_case_sync "Valid input" `Quick test_parse_headers_valid_input;
              test_case_sync "Empty input" `Quick test_parse_headers_empty_input;
           ] );
       ]
