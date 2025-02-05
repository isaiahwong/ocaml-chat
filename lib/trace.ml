module IntHashTbl = Hashtbl.Make (Int)

let ( let* ) = Option.bind

module RoundTrip = struct
  let rtt_traces : int64 IntHashTbl.t = IntHashTbl.create 16
  let current_time_nanos () = Mtime_clock.now () |> Mtime.to_uint64_ns
  let current_time_micro () = Int64.div (current_time_nanos ()) 1_000L
  let trace id = IntHashTbl.add rtt_traces id (current_time_micro ())
  let get_trace id = IntHashTbl.find_opt rtt_traces id

  let get_trace_delta id =
    let* start = get_trace id in
    let now = current_time_micro () in
    Some (Int64.sub now start)
end
