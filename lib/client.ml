open Infix

let parse_host_port str =
  match String.split_on_char ':' str with
  | [ host; port ] -> Lwt.return (host, int_of_string port)
  | _ -> Lwt.fail_with @@ Printf.sprintf "failed to parse %s" str

let get_addr host port =
  let* addr_info =
    Lwt_unix.getaddrinfo host (string_of_int port) [ Unix.(AI_FAMILY PF_INET) ]
  in
  match addr_info with
  | [] -> Lwt.fail_with "Failed to resolve host"
  | addr :: _ -> Lwt.return addr.Unix.ai_addr

(** Creates a socket connection to remote *)
let connect addr =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket addr in
  Lwt.return socket

let loop context = Handler.chat_loop context

let send_file (context : Handler.Context.t) filepath =
  Protocol.write_from context.writer filepath 1 4096

(** Establishes connection with remote chat server *)
let connect_to_server ~f host_port_str =
  let* host, port = parse_host_port host_port_str in
  let* addr = get_addr host port in
  let* socket = connect addr in

  let* () = Lwt_io.printl "Connected to server" in
  let reader = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let writer = Lwt_io.of_fd ~mode:Lwt_io.output socket in

  let context = Handler.Context.init reader writer in

  let* () = f context in

  Lwt.return_unit

(** Start the chat client application - blocking call *)
let start_chat host_port_str =
  Lwt_main.run (connect_to_server ~f:loop host_port_str)

let start_send_file host_port_str filepath =
  Lwt_main.run
    (connect_to_server ~f:(fun ctx -> send_file ctx filepath) host_port_str)
