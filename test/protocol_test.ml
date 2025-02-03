open Chat

(** Pretty-printing function for headers *)
let pp_request_method fmt = function
  | `START -> Format.fprintf fmt "`START"
  | `MESSAGE -> Format.fprintf fmt "`MESSAGE"
  | `ACK -> Format.fprintf fmt "`ACK"

let pp_header fmt { Protocol.request_method; size } =
  Format.fprintf fmt "{ request_method = %a; size = %d }" pp_request_method
    request_method size

(** Equality function for headers *)
let equal_header (h1 : Protocol.header) (h2 : Protocol.header) =
  h1.request_method = h2.request_method && h1.size = h2.size

(** Alcotest testable for header *)
let header_testable = Alcotest.testable pp_header equal_header

let result_header_testable = Alcotest.(result header_testable string)

(* Test Cases *)
let test_parse_headers_valid_input () =
  let input = "START 1024" in
  let expected =
    Ok { Protocol.request_method = `START; Protocol.size = 1024 }
  in
  let actual = Protocol.header_of_string input in
  Alcotest.check result_header_testable "Valid input" expected actual

let test_parse_headers_empty_input () =
  let input = "" in
  let expected = Error "EOF" in
  let actual = Protocol.header_of_string input in
  Alcotest.check result_header_testable "Empty input" expected actual

let () =
  Alcotest.run "parse_headers"
    [
      ( "parse_headers",
        [
          Alcotest.test_case "Valid input" `Quick test_parse_headers_valid_input;
          Alcotest.test_case "Empty input" `Quick test_parse_headers_empty_input;
        ] );
    ]
