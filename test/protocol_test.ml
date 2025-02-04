open Chat

let ( let* ) = Lwt.bind

(** Equality function for headers *)
let equal_header (h1 : Protocol.Header.t) (h2 : Protocol.Header.t) =
  h1.request_method = h2.request_method && h1.size = h2.size

(** Alcotest testable for header *)
let header_testable = Alcotest.testable Protocol.Header.pp equal_header

let result_header_testable = Alcotest.(result header_testable string)

let test_protocol_read_message _ () =
  let read () =
    let r, w = Lwt_io.pipe () in
    let* () = Lwt_io.write w "5 MESSAGE tim\n12345" in
    let* () = Lwt_io.close w in
    let* result = Chat.Protocol.read r in
    match result with
    | Ok _ -> Lwt.return "ok"
    | Error e -> Lwt.return (Printf.sprintf "error: %s" e)
  in
  let* res = read () in
  Alcotest.(check string) "read_message should return 'ok'" "ok" res;
  Lwt.return ()

(* Test Cases *)
let test_parse_headers_valid_input () =
  let input = "0 START tim apple" in
  let expected =
    Ok
      {
        Protocol.Header.request_method = START ("tim", "apple");
        Protocol.Header.size = 0;
      }
  in
  let actual = Protocol.Header.of_string input in
  Alcotest.check result_header_testable "Valid input" expected actual

let test_parse_headers_empty_input () =
  let input = "" in
  let expected = Error "EOF" in
  let actual = Protocol.Header.of_string input in
  Alcotest.check result_header_testable "Empty input" expected actual

let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ Alcotest_lwt.run "Chat Server Tests"
       [
         ( "read",
           [
             test_case "Read message with correct size" `Quick
               test_protocol_read_message;
           ] );
         ( "parse_headers",
           [
             test_case_sync "Valid input" `Quick test_parse_headers_valid_input;
             test_case_sync "Empty input" `Quick test_parse_headers_empty_input;
           ] );
       ]
