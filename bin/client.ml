let ( let* ) = Lwt.bind

let get_addr host port =
  let* addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in

  match addr_info with
  | [] -> Lwt.fail_with "Failed to resolve host"
  | addr :: _ -> Lwt.return addr.Unix.ai_addr

let connect addr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket addr in
  Lwt.return socket

let start_client host port =
  let* addr = get_addr host port in
  let* socket = connect addr in

  let ic = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let context = Chat.Handler.Context.init ic oc in

  let* () = Chat.Handler.session_loop context in
  Lwt.return_unit

let () = Lwt_main.run (start_client "0.0.0.0" 8080)
