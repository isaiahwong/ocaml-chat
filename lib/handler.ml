let ( let* ) = Lwt.bind

module Context = struct
  type t = {
    reader : Lwt_io.input_channel;
    writer : Lwt_io.output_channel;
  }

  let init reader writer = { reader; writer }
end

let handle_message (context : Context.t) id body =
  let* () =
    Lwt_io.printf "[size %d] Message: %s \n" (String.length body) body
  in
  let* () = Protocol.write context.writer Protocol.empty_body (ACK id) in
  Lwt.return_ok ()

let handle_ack id =
  let ack_msg =
    match Trace.RoundTrip.get_trace_delta id with
    | Some delta -> Printf.sprintf "ACK %d received [%Ld ms]" id delta
    | None -> Printf.sprintf "ACK %d not found\n" id
  in
  let* () = Lwt_io.printl ack_msg in
  Lwt.return_ok ()

let multiplex (context : Context.t) (message : Protocol.Message.t) =
  match message.header.request_method with
  | ACK id -> handle_ack id
  | MSG id -> handle_message context id message.body

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
    let () = Trace.RoundTrip.trace id in
    let* () = Lwt_io.write context.writer message in

    loop (id + 1)
  in
  loop 0

let session_loop (context : Context.t) =
  let loop () =
    let* r = Lwt.pick [ read_loop context; send_loop context ] in
    match r with
    | Ok _ -> Lwt_io.printl "Ending..."
    | Error e -> Lwt.fail_with e
  in
  Lwt.catch loop (fun exp ->
      let* () = Lwt_io.printlf "closing %s" (Printexc.to_string exp) in
      Lwt.return ())
