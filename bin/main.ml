open Chat

let () =
  let port = 8080 in
  Lwt_main.run (Server.serve port)

(* open Lwt_io

let ( let* ) = Lwt.bind

(* Task 1: Blocking task that reads from stdin *)
let rec read_input () =
  let* line = read_line stdin in
  let* () = printf "You entered: %s\n" line in
  if line = "quit" then Lwt.return () else read_input ()

(* Task 2: Non-blocking task that does some work *)
let rec do_work () =
  let* () = printf "Working...\n" in
  let* () = Lwt_unix.sleep 1.0 in
  (* Simulate work with a delay *)
  do_work ()

(* Run both tasks concurrently *)
let main () = Lwt.join [ read_input (); do_work () ]

(* Entry point *)
let () = Lwt_main.run (main ()) *)
