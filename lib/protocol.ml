let ( let* ) = Result.bind

type header = {
  request_method : [ `START | `MESSAGE | `ACK ];
  size : int;
}

let request_method_of_string = function
  | "START" -> Ok `START
  | "MESSAGE" -> Ok `MESSAGE
  | "ACK" -> Ok `ACK
  | _ -> Error "Invalid method"

let header_of_string str : (header, string) result =
  let scan str =
    try
      Scanf.sscanf str "%s %d" (fun request_method_str size ->
          Ok (request_method_str, size))
    with
    | Scanf.Scan_failure msg -> Error ("Scan failure: " ^ msg)
    | Failure msg -> Error ("Scan failure: " ^ msg)
    | End_of_file -> Error "EOF"
  in
  let* request_method_str, size = scan str in
  let* request_method = request_method_of_string request_method_str in
  Ok { request_method; size }

type message = {
  header : header;
  body : bytes;
}
