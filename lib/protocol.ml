let ( let* ) = Result.bind
let ( let-? ) = Lwt.bind
let empty_body = ""

module RequestMethod = struct
  type t =
    | ACK of int
    | MSG of int
    | UNAVAIL

  let of_string s =
    match String.split_on_char ' ' s with
    | [ "ACK"; id ] -> Ok (ACK (int_of_string id))
    | [ "MSG"; id ] -> Ok (MSG (int_of_string id))
    | [ "UNAVAIL" ] -> Ok UNAVAIL
    | _ -> Error (Errors.ERR "Invalid request method")

  let to_string = function
    | MSG id -> Printf.sprintf "MSG %d" id
    | ACK id -> Printf.sprintf "ACK %d" id
    | UNAVAIL -> "UNAVAIL"

  let pp fmt = function
    | MSG id -> Format.fprintf fmt "MSG %d" id
    | ACK id -> Format.fprintf fmt "ACK %d" id
    | UNAVAIL -> Format.fprintf fmt "UNAVAIL"
end

(** Protocol Header *)
module Header = struct
  type t = {
    request_method : RequestMethod.t;
    size : int;
  }

  let init request_method size = { request_method; size }

  let init_msg_header id body =
    { request_method = MSG id; size = String.length body }

  let of_string str : (t, Errors.t) result =
    let scan str =
      try
        (* %[^\n] matches everything except new line*)
        Scanf.sscanf str "%d %[^\n]" (fun size request_method_str ->
            Ok (request_method_str, size))
      with
      | Scanf.Scan_failure msg -> Error (Errors.ERR ("Scan failure: " ^ msg))
      | Failure msg -> Error (Errors.ERR ("Scan failure: " ^ msg))
      | End_of_file -> Error EOF
    in
    let* request_method_str, size = scan str in
    let* request_method = RequestMethod.of_string request_method_str in
    Ok { request_method; size }

  let to_string hdr =
    Printf.sprintf "%d %s" hdr.size
    @@ RequestMethod.to_string hdr.request_method

  let pp fmt { request_method; size } =
    Format.fprintf fmt "{ request_method = %a; size = %d }" RequestMethod.pp
      request_method size
end

module Message = struct
  type t = {
    header : Header.t;
    body : string;
  }

  let init header body = { header; body }

  let marshal message =
    Printf.sprintf "%s\n%s" (Header.to_string message.header) message.body
end

let read_header input =
  let-? line_opt = Lwt_io.read_line_opt input in
  match line_opt with
  | None -> Lwt.return_error Errors.EOF
  | Some header_str -> (
      match Header.of_string header_str with
      | Error err -> Lwt.return_error err
      | Ok header -> Lwt.return_ok header)

let read_body reader (header : Header.t) =
  match header.size with
  | 0 -> Lwt.return_ok ""
  | size -> (
      let buffer = Bytes.create size in
      let-? read = Lwt_io.read_into reader buffer 0 size in
      match read with
      | 0 -> Lwt.return_error @@ Errors.ERR "Malformed"
      | _ -> Lwt.return_ok @@ Bytes.to_string buffer)

let read reader =
  let-? header = read_header reader in
  match header with
  | Error err -> Lwt.return_error err
  | Ok header -> (
      let-? body = read_body reader header in
      match body with
      | Error err -> Lwt.return_error err
      | Ok body -> Lwt.return_ok @@ Message.init header body)

let write (writer : Lwt_io.output_channel) body request_method =
  let header = Header.init request_method (String.length body) in
  let message = Message.init header body in
  Lwt_io.write writer @@ Message.marshal message

let write_header (writer : Lwt_io.output_channel) request_method =
  let header = Header.init request_method (String.length empty_body) in
  let message = Message.init header empty_body in
  Lwt_io.write writer @@ Message.marshal message
