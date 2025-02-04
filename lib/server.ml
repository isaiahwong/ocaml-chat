let ( let* ) = Lwt.bind

let rec accept_conn server_socket =
  let* client_socket, _addr = Lwt_unix.accept server_socket in

  let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in

  let* hostname = Lwt_unix.gethostname () in

  let* () = Lwt_io.printl "New Connection " in

  let context = Handler.Context.init hostname input output in

  Lwt.async (fun () -> Handler.session_loop context);
  accept_conn server_socket
