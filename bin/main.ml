open Chat

let () =
  let port = 8080 in
  Lwt_main.run (Server.serve port)
