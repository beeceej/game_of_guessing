type action = Yes | No | Guess of int | What

val try_parse_int : string -> int option

val parse : string -> action
