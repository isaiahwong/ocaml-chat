let ( let* ) = Lwt.bind

let read_message ic =
  let read_header_str input =
    let* line_opt = Lwt_io.read_line_opt input in
    match line_opt with
    | None -> Lwt.return_error "Malformed"
    | Some header_str -> (
        match Protocol.header_of_string header_str with
        | Error err -> Lwt.return_error err
        | Ok header -> Lwt.return_ok header)
  in

  let read_body ic (header : Protocol.header) =
    let buffer = Bytes.create header.size in
    let* read = Lwt_io.read_into ic buffer 0 header.size in
    match read with
    | 0 -> Lwt.return_error "Malformed"
    | _ -> Lwt.return_ok buffer
  in

  let* header = read_header_str ic in

  match header with
  | Error _ as err -> Lwt.return err
  | Ok header -> (
      let* body = read_body ic header in
      match body with
      | Error _ as err -> Lwt.return err
      | Ok body -> Lwt.return_ok { Protocol.header; Protocol.body })
