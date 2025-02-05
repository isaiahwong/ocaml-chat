let ( let* ) = Lwt.bind
let occupied = ref false

let reject_session socket (context : Handler.Context.t) =
  let* () = Protocol.write_header context.writer UNAVAIL in

  (* Give client some time to process *)
  let* () = Lwt_unix.sleep 1.0 in
  let* () = Lwt_unix.close socket in
  Lwt.return ()

let start_session socket (context : Handler.Context.t) =
  occupied := true;
  let* () = Lwt_io.printl "New Connection " in

  (* Runs blocking chat_loop *)
  let* () = Handler.chat_loop context in
  let* () = Lwt_unix.close socket in
  occupied := false;
  Lwt.return ()

let rec accept_conn server_socket =
  let* socket, _addr = Lwt_unix.accept server_socket in

  let reader = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let writer = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let context = Handler.Context.init reader writer in

  let fn =
    match !occupied with
    | true -> reject_session socket context
    | false -> start_session socket context
  in

  Lwt.async (fun () -> fn);
  accept_conn server_socket

let serve port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_any, port)) in

  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;

  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;

  let* () = Lwt_io.printlf "Server Started on port %d" port in
  accept_conn server_socket
