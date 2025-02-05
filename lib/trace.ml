open Infix
module IntHashTbl = Hashtbl.Make (Int)

module type S = sig
  val trace : int -> unit
  val get_trace : int -> int64 option
  val get_trace_delta : int -> int64 option
end

module type IntHashable = sig
  val tbl : int64 IntHashTbl.t
end

module Make (H : IntHashable) : S = struct
  let current_time_nanos () = Mtime_clock.now () |> Mtime.to_uint64_ns
  let current_time_micro () = Int64.div (current_time_nanos ()) 1_000L
  let trace id = IntHashTbl.add H.tbl id (current_time_micro ())
  let get_trace id = IntHashTbl.find_opt H.tbl id

  let get_trace_delta id =
    let-? start = get_trace id in
    let now = current_time_micro () in
    Some (Int64.sub now start)
end
