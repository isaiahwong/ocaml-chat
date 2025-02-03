let ( let* ) = Lwt.bind

let echo_client host port =
  let* addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in

  let* addr =
    match addr_info with
    | [] -> Lwt.fail_with "Failed to resolve host"
    | addr :: _ -> Lwt.return addr.Unix.ai_addr
  in

  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  let* () = Lwt_unix.connect socket addr in

  let input = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let rec send_messages () =
    let* message = Lwt_io.read_line Lwt_io.stdin in
    let* () = Lwt_io.write_line output message in

    let* response = Lwt_io.read_line_opt input in
    match response with
    | None ->
        let* () = Lwt_io.printl "Server disconnected. Exiting..." in
        let* () = Lwt_io.close input in
        let* () = Lwt_io.close output in
        let* () = Lwt_unix.close socket in
        Lwt.return_unit
    | Some msg ->
        let* () = Lwt_io.printf "Server Response: %s\n" msg in
        send_messages ()
  in
  send_messages ()
