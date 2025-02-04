module StringHashtbl = Hashtbl.Make (String)

(** Record that represents a session*)
type session = {
  receiver : string;
  output : Lwt_io.output_channel;
}

(** Sessions in-mem hash table that keeps track of all active session *)
let key_to_sessions : session StringHashtbl.t ref =
  ref (StringHashtbl.create 16)

(** Adds a new session that keep tracks *)
let add_session key receiver output =
  let session = { receiver; output } in
  StringHashtbl.add !key_to_sessions key session

let remove_session key = StringHashtbl.remove !key_to_sessions key
let find_session key = StringHashtbl.find_opt !key_to_sessions key
