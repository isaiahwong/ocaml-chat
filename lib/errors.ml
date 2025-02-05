type t =
  | EOF
  | UNAVAILABLE
  | ERR of string

let to_string = function
  | EOF -> "EOF"
  | UNAVAILABLE -> "UNAVAILABLE"
  | ERR s -> s

let pp fmt = function
  | EOF -> Format.fprintf fmt "EOF"
  | UNAVAILABLE -> Format.fprintf fmt "UNAVAILABLE"
  | ERR err -> Format.fprintf fmt "ERR %s" err
