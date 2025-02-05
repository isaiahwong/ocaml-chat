module IntHashTbl = Hashtbl.Make (Int)

type t = { rtt_hashtbl : int64 IntHashTbl.t }

let current_time_nanos () = Mtime_clock.now () |> Mtime.to_uint64_ns
let current_time_micro () = Int64.div (current_time_nanos ()) 1_000L
let create n = { rtt_hashtbl = IntHashTbl.create n }
let clear ~hashtbl = IntHashTbl.clear hashtbl.rtt_hashtbl

let trace ~hashtbl ~id =
  IntHashTbl.add hashtbl.rtt_hashtbl id (current_time_micro ())

let get_trace ~hashtbl ~id = IntHashTbl.find_opt hashtbl.rtt_hashtbl id

let get_trace_delta_micro ~hashtbl ~id =
  match get_trace ~hashtbl ~id with
  | None -> None
  | Some start ->
      let now = current_time_micro () in
      Some (Int64.sub now start)
