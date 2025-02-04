let ( let* ) = Lwt.bind

module Context = struct
  type t = {
    mutable sender : string option;
    hostname : string;
    reader : Lwt_io.input_channel;
    writer : Lwt_io.output_channel;
  }

  let init hostname reader writer = { sender = None; hostname; reader; writer }
end

(** Creates a new session *)
let handle_start (context : Context.t) (sender, receiver) =
  match Session.find_session sender with
  | Some _ -> Lwt.fail_with "Connection"
  | None ->
      Session.add_session sender receiver context.writer;
      context.sender <- Some sender;
      Lwt.return ()

let handle_message (context : Context.t) receiver body =
  let* sender =
    match context.sender with
    | None -> Lwt.fail_with "Sender nil"
    | Some s -> Lwt.return s
  in

  let* receiver_session =
    match Session.find_session receiver with
    | None -> Lwt.fail_with "Sender not found"
    | Some s -> Lwt.return s
  in

  if not (sender = receiver_session.receiver) then Lwt.fail_with "Not sender"
  else Protocol.write receiver_session.output body (MESSAGE sender)

let mux (context : Context.t) (message : Protocol.Message.t) =
  match message.header.request_method with
  | START (_ as t) -> handle_start context t
  | ACK -> Lwt.return ()
  | MESSAGE receiver -> handle_message context receiver message.body

let clean_session (context : Context.t) =
  match context.sender with
  | Some sender -> Session.remove_session sender
  | None -> ()

let session_loop (context : Context.t) =
  let rec loop () =
    let* res = Protocol.read context.reader in
    match res with
    | Ok m ->
        let* () = mux context m in
        loop ()
    | Error err -> Lwt.fail_with err
  in
  Lwt.catch loop (fun _ ->
      let* () = Lwt_io.close context.reader in
      let* () = Lwt_io.close context.writer in
      clean_session context;
      Lwt.return ())
