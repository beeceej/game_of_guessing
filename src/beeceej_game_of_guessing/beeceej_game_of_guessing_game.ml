module UserInput = Beeceej_game_of_guessing_user_input

let () = Random.self_init ()

type result = Win | Lose | Quit

let welcome_question =
  "Welcome to the guessing game, would you like to play? (y/n)"

let guess_a_number current_tries max_tries =
  "Guess a number" ^ "["
  ^ string_of_int current_tries
  ^ "/" ^ string_of_int max_tries ^ "]"

let ask text =
  let () = print_string ("\n" ^ text ^ "\n") in
  read_line () |> UserInput.parse

let i_dont_understand = "I didn't understand, try again"

let rec game tries max_tries target =
  if tries > max_tries then
    let () = print_int tries in
    Lose
  else
    let response = ask (guess_a_number tries max_tries) in
    match response with
    | Guess n -> if n == target then Win else game (tries + 1) max_tries target
    | Yes ->
        let _ = ask i_dont_understand in
        game tries max_tries target
    | No ->
        let _ = ask i_dont_understand in
        game tries max_tries target
    | What ->
        let _ = ask i_dont_understand in
        game tries max_tries target

let play () =
  let rec pose_question (r : UserInput.action) =
    match r with
    | Yes -> game 0 10 (Random.int 10)
    | No -> Quit
    | Guess _ -> ask i_dont_understand |> pose_question
    | What -> ask i_dont_understand |> pose_question
  in
  let result = ask welcome_question |> pose_question in
  match result with
  | Win -> print_string "you win, nice\n"
  | Lose -> print_string "you lose\n"
  | Quit -> print_string "goodbye\n"
