type t =
  | EOF
  | UNAVAILABLE
  | MALFORMED
  | ERR of string

let to_string = function
  | EOF -> "EOF"
  | UNAVAILABLE -> "UNAVAILABLE"
  | MALFORMED -> "MALFORMED"
  | ERR s -> s

let pp fmt = function
  | EOF -> Format.fprintf fmt "EOF"
  | UNAVAILABLE -> Format.fprintf fmt "UNAVAILABLE"
  | MALFORMED -> Format.fprintf fmt "MALFORMED"
  | ERR err -> Format.fprintf fmt "ERR %s" err
