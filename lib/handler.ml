open Infix

(** Context that stores reader and writer for a given session *)
module Context = struct
  type t = {
    reader : Lwt_io.input_channel;
    writer : Lwt_io.output_channel;
  }

  let init reader writer = { reader; writer }
end

(** stores round trip time for each session *)
let rtt = Round_trip.create 16

(** Prints message to stdout and sends ACK to sender *)
let handle_message (context : Context.t) id body =
  let* () = Lwt_io.printf "> %s\n" body in
  let* () = Protocol.write_empty_body context.writer (ACK id) in
  Lwt.return_ok ()

(** Handles ack and prints acknowledgement to stdout with RTT time *)
let handle_ack id =
  let ack_msg =
    match Round_trip.get_trace_delta_micro ~hashtbl:rtt ~id with
    | Some delta -> Printf.sprintf "[Message ack - took %Ldμs]\n" delta
    | None -> Printf.sprintf "ACK %d not found\n" id
  in
  let* () = Lwt_io.printl ack_msg in
  Lwt.return_ok ()

let handle_unavail () = Lwt.return_error Errors.UNAVAILABLE

(** Multiplexs message based on the header's request_method *)
let multiplex (context : Context.t) (message : Protocol.Message.t) =
  match message.header.request_method with
  | ACK id -> handle_ack id
  | MSG id -> handle_message context id message.body
  | UNAVAIL -> handle_unavail ()

(** Constantly reads from tcp stream *)
let read_loop (context : Context.t) =
  let rec loop () =
    let* res = Protocol.read context.reader in
    match res with
    | Error err -> Lwt.return_error err
    | Ok m -> (
        let* m = multiplex context m in
        match m with
        | Ok _ -> loop ()
        | Error e -> Lwt.return_error e)
  in
  loop ()

(** Listen's to stdin inputs and sends it over tcp stream *)
let send_loop (context : Context.t) =
  (* Local monotonic increasing id *)
  let rec loop id =
    (* Reads from user's stdin *)
    let* body = Lwt_io.read_line Lwt_io.stdin in
    let header = Protocol.Header.init_msg_header id body in
    let message =
      Protocol.Message.init header body |> Protocol.Message.marshal
    in
    (* Start tracing round trip *)
    let () = Round_trip.trace ~hashtbl:rtt ~id in
    let* () = Lwt_io.write context.writer message in
    loop (id + 1)
  in
  loop 0

(** Error handler for common errors *)
let handle_error err =
  match err with
  | Errors.EOF -> Lwt_io.printl "Remote Closed"
  | Errors.UNAVAILABLE -> Lwt_io.printl "Remote is unavailable"
  | Errors.MALFORMED -> Lwt_io.printl "Malformed request"
  | Errors.ERR err ->
      let* () = Lwt_io.printlf "Error: %s\n" err in
      Lwt.return_unit

(** Chat loop runs with [Handler.context] which consists of readers and writers
    to be written to TCP stream. This is a blocking call that is managed by Lwt
    concurrently.
    @param context Context that contains readers and writers *)
let chat_loop (context : Context.t) =
  let main () =
    let* r = Lwt.pick [ read_loop context; send_loop context ] in
    match r with
    | Ok _ -> Lwt_io.printl "Ending..."
    | Error e -> handle_error e
  in
  let cleanup () =
    let () = Round_trip.clear ~hashtbl:rtt in
    Lwt.return_unit
  in
  Lwt.finalize main cleanup
