open Cmdliner

let server_cmd f =
  let doc = "Start the chat server." in
  let info = Cmd.info "server" ~doc in
  let port = Arg.(value & pos 0 string "8080" & info [] ~docv:"PORT") in
  Cmd.v info Term.(const f $ (Term.const int_of_string $ port))

let client_cmd f =
  let doc = "Start the chat client." in
  let info = Cmd.info "client" ~doc in
  let address =
    Arg.(value & pos 0 string "0.0.0.0:8080" & info [] ~docv:"ADDRESS")
  in
  Cmd.v info Term.(const f $ address)

let client_file_cmd f =
  let doc = "Start the chat client." in
  let info = Cmd.info "client-file" ~doc in
  let address =
    Arg.(value & pos 0 string "0.0.0.0:8080" & info [] ~docv:"ADDRESS")
  in
  let filepath =
    Arg.(
      value
      & pos 0 string (Filename.concat (Sys.getcwd ()) "data/large_msg")
      & info [] ~docv:"filepath")
  in
  Cmd.v info Term.(const f $ address $ filepath)

let execute () =
  let cmd =
    let doc = "1 on 1 chat application" in
    let info = Cmd.info "chat" ~doc in
    Cmd.group info
      [
        server_cmd Chat.Server.serve_chat;
        client_cmd Chat.Client.start_chat;
        client_file_cmd Chat.Client.start_send_file;
      ]
  in
  exit (Cmd.eval cmd)
