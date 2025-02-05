(** Round trip hashtbl type *)
type t

(** Create a new instance of the RoundTrip *)
val create : int -> t

(** Clears the hashtbl *)
val clear : hashtbl:t -> unit

(** Stores the current microsecond timestamp for the given [id] *)
val trace : hashtbl:t -> id:int -> unit

(** Retrieves the timestamp for the given [id], if it exists *)
val get_trace : hashtbl:t -> id:int -> int64 option

(** Returns the time delta since the trace for [id] *)
val get_trace_delta_micro : hashtbl:t -> id:int -> int64 option
