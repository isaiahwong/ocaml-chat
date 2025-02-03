open Alcotest_lwt

let ( let* ) = Lwt.bind

let test_read_message _ () =
  let read () =
    let r, w = Lwt_io.pipe () in
    let* () = Lwt_io.write_line w "START s5" in
    let* () = Lwt_io.write w "12345" in
    let* () = Lwt_io.close w in
    let* result = Chat.Server.read_message r in
    match result with
    | Ok _ -> Lwt.return "ok"
    | Error e -> Lwt.return (Printf.sprintf "error: %s" e)
  in
  let* res = read () in
  Alcotest.(check string) "read_message should return 'ok'" "ok" res;
  Lwt.return ()

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Chat Server Tests"
       [
         ( "read_message",
           [
             test_case "Read message with correct size" `Quick test_read_message;
           ] );
       ]
