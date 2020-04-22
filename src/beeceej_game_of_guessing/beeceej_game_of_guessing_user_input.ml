type action = Yes | No | Guess of int | What

let try_parse_int n = try Some (int_of_string n) with _ -> None

let parse input =
  let is_yes input = input = "y" || input = "yes" in
  let is_no input = input = "n" || input = "no" in
  if String.lowercase_ascii input |> is_yes then Yes
  else if String.lowercase_ascii input |> is_no then No
  else match try_parse_int input with Some n -> Guess n | None -> What
