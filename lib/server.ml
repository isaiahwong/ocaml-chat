let ( let* ) = Lwt.bind
let occupied = ref false

let rec accept_conn server_socket =
  let* incoming_socket, _addr = Lwt_unix.accept server_socket in

  let ic = Lwt_io.of_fd ~mode:Lwt_io.input incoming_socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output incoming_socket in

  let* () = Lwt_io.printl "New Connection " in

  let context = Handler.Context.init ic oc in

  let start_session =
    let* () = Handler.session_loop context in
    let* () = Lwt_unix.close incoming_socket in
    occupied := false;
    Lwt.return ()
  in

  Lwt.async (fun () -> start_session);
  accept_conn server_socket

let serve port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in

  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;

  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;

  let* () = Lwt_io.printlf "Server Started on port %d" port in
  accept_conn server_socket
