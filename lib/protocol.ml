open Infix
(*
  Protocol follows a text framing format that consists of a header separated by a 
  line feed (lf) followed by the body. 

  Example of a full message - 
  13 MSG 1\nHello, OCaml!

  Header format is as follows
  | size              int32  | 
  | method            string |
  | id                int32  |
*)

(** Protocol Method Defines the messaging protocol methods used.
    - [`ACK id`] acknowledges a message with the given ID.
    - [`MSG id`] a message with an ID.
    - [`UNAVAIL`] indicates that the receiver cannot handle the connection. *)
module Method = struct
  type t =
    | ACK of int
    | MSG of int
    | UNAVAIL

  let of_string s =
    match String.split_on_char ' ' s with
    | [ "ACK"; id ] -> Ok (ACK (int_of_string id))
    | [ "MSG"; id ] -> Ok (MSG (int_of_string id))
    | [ "UNAVAIL" ] -> Ok UNAVAIL
    | _ -> Error (Errors.ERR "Invalid method")

  let to_string = function
    | MSG id -> Printf.sprintf "MSG %d" id
    | ACK id -> Printf.sprintf "ACK %d" id
    | UNAVAIL -> "UNAVAIL"

  let pp fmt = function
    | MSG id -> Format.fprintf fmt "MSG %d" id
    | ACK id -> Format.fprintf fmt "ACK %d" id
    | UNAVAIL -> Format.fprintf fmt "UNAVAIL"
end

(** Protocol header of a message.
    - [`request_method`] : Method used for the message such as `MSG` or `ACK`.
    - [`size`] : The size of the message body in bytes. *)
module Header = struct
  type t = {
    request_method : Method.t;
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
      | Scanf.Scan_failure _ -> Error Errors.MALFORMED
      | Failure _ -> Error Errors.MALFORMED
      | End_of_file -> Error EOF
    in
    let-- request_method_str, size = scan str in
    let-- request_method = Method.of_string request_method_str in
    Ok { request_method; size }

  let to_string hdr =
    Printf.sprintf "%d %s" hdr.size @@ Method.to_string hdr.request_method

  let pp fmt { request_method; size } =
    Format.fprintf fmt "{ request_method = %a; size = %d }" Method.pp
      request_method size
end

(** Full message in the communication protocol.
    - [`header`] : Metadata about the message such as method and size.
    - [`body`] : The actual content of the message. *)
module Message = struct
  type t = {
    header : Header.t;
    body : string;
  }

  let init header body = { header; body }

  let marshal_header (header : Header.t) =
    Printf.sprintf "%s\n" @@ Header.to_string header

  (** Marshals [t] into protocol string compliant format *)
  let marshal (message : t) =
    Printf.sprintf "%s\n%s" (Header.to_string message.header) message.body
end

let empty_body = ""

(** Reads first line in tcp stream to get header *)
let read_header reader =
  let* line_opt = Lwt_io.read_line_opt reader in
  match line_opt with
  | None -> Lwt.return_error Errors.EOF
  | Some header_str -> (
      match Header.of_string header_str with
      | Error err -> Lwt.return_error err
      | Ok header -> Lwt.return_ok header)

(** Reads the body of the protocol. Should be called after reading the header *)
let read_body reader size =
  (* Recursively reads exactly [remaining] bytes into buffer *)
  let rec read_exactly reader buffer offset remaining =
    if remaining = 0 then
      Lwt.return_ok ()
    else
      let* read = Lwt_io.read_into reader buffer offset remaining in
      if read = 0 then
        Lwt.return_error Errors.MALFORMED
      else
        read_exactly reader buffer (offset + read) (remaining - read)
  in
  if size = 0 then
    Lwt.return_ok ""
  else
    let buffer = Bytes.create size in
    let* result = read_exactly reader buffer 0 size in
    match result with
    | Error e -> Lwt.return_error e
    | Ok () -> Lwt.return_ok (Bytes.to_string buffer)

(** Reads a completed Protocol message from TCP Stream *)
let read reader : (Message.t, Errors.t) result Lwt.t =
  let* header = read_header reader in
  match header with
  | Error err -> Lwt.return_error err
  | Ok header -> (
      let* res = read_body reader header.size in
      match res with
      | Error err -> Lwt.return_error err
      | Ok body -> Lwt.return_ok @@ Message.init header body)

let write_in_chunks
    (writer : Lwt_io.output_channel)
    (message : Message.t)
    (chunk : int) : unit Lwt.t =
  let* () = Lwt_io.write writer @@ Message.marshal_header message.header in
  let rec write offset =
    if offset >= message.header.size then
      Lwt.return_unit
    else
      let remaining = message.header.size - offset in
      let size =
        if remaining > chunk then
          chunk
        else
          remaining
      in
      let* size = Lwt_io.write_from_string writer message.body offset size in
      let* () = Lwt_io.printf "written %d \n" size in
      write (offset + size)
  in
  write 0

(** writes a Protocol compliant message into TCP stream
    @param body message body to be sent *)
let write ?(chunk = 4096) (writer : Lwt_io.output_channel) body request_method =
  let body_len = String.length body in
  let header = Header.init request_method body_len in
  let message = Message.init header body in
  let* () = Lwt_io.printf "body %d\n" body_len in
  if body_len <= chunk then
    Lwt_io.write writer @@ Message.marshal message
  else
    let* () = write_in_chunks writer message chunk in
    Lwt_io.printl "done writing"

let write_from (writer : Lwt_io.output_channel) filepath id chunk =
  let* fc = Lwt_io.open_file ~mode:Lwt_io.Input filepath in
  (* write header *)
  let* stats = Lwt_unix.stat filepath in
  let file_size = stats.Lwt_unix.st_size in
  let header = Header.init (Method.MSG id) file_size in
  let* () = Lwt_io.write writer @@ Message.marshal_header header in

  let rec read () =
    let* s = Lwt_io.read ~count:chunk fc in
    match s with
    | "" -> Lwt.return_unit
    | s ->
        let* () = Lwt_io.write writer s in
        read ()
  in
  let* () = read () in
  Lwt_io.close fc

(** writes a Protocol compliant empty message into TCP stream *)
let write_empty_body (writer : Lwt_io.output_channel) request_method =
  let header = Header.init request_method (String.length empty_body) in
  let message = Message.init header empty_body in
  Lwt_io.write writer @@ Message.marshal message
