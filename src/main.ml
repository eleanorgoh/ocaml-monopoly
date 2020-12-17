open Newboard 
open State

exception Empty
exception PlayerException of string 

let process_json json = Newboard.from_json json

let sample_board = Yojson.Basic.from_file "board.json" |> process_json

let opening_message = "\n\n 
███╗   ███╗ ██████╗ ███╗   ██╗ ██████╗ ██████╗  ██████╗ ██╗  ██╗   ██╗██╗
████╗ ████║██╔═══██╗████╗  ██║██╔═══██╗██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝██║
██╔████╔██║██║   ██║██╔██╗ ██║██║   ██║██████╔╝██║   ██║██║   ╚████╔╝ ██║
██║╚██╔╝██║██║   ██║██║╚██╗██║██║   ██║██╔═══╝ ██║   ██║██║    ╚██╔╝  ╚═╝
██║ ╚═╝ ██║╚██████╔╝██║ ╚████║╚██████╔╝██║     ╚██████╔╝███████╗██║   ██╗
╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝ ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝
                                                                         \n"

let start_message = "\n\n" ^ 
                    "███████╗████████╗ █████╗ ██████╗ ████████╗██╗███╗   ██╗ ██████╗      ██████╗  █████╗ ███╗   ███╗███████╗
██╔════╝╚══██╔══╝██╔══██╗██╔══██╗╚══██╔══╝██║████╗  ██║██╔════╝     ██╔════╝ ██╔══██╗████╗ ████║██╔════╝
███████╗   ██║   ███████║██████╔╝   ██║   ██║██╔██╗ ██║██║  ███╗    ██║  ███╗███████║██╔████╔██║█████╗  
╚════██║   ██║   ██╔══██║██╔══██╗   ██║   ██║██║╚██╗██║██║   ██║    ██║   ██║██╔══██║██║╚██╔╝██║██╔══╝  
███████║   ██║   ██║  ██║██║  ██║   ██║   ██║██║ ╚████║╚██████╔╝    ╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗
╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝╚═╝  ╚═══╝ ╚═════╝      ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝
                                                                                                        \n"

let end_message = "\n\n" ^ 
                  " ██████╗  █████╗ ███╗   ███╗███████╗     ██████╗ ██╗   ██╗███████╗██████╗ 
██╔════╝ ██╔══██╗████╗ ████║██╔════╝    ██╔═══██╗██║   ██║██╔════╝██╔══██╗
██║  ███╗███████║██╔████╔██║█████╗      ██║   ██║██║   ██║█████╗  ██████╔╝
██║   ██║██╔══██║██║╚██╔╝██║██╔══╝      ██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗
╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗    ╚██████╔╝ ╚████╔╝ ███████╗██║  ██║
 ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝     ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝
                                                                          \n"

let goodbye = "Woo! Hope you enjoyed Monopoly! ʕ•ᴥ•ʔ \n Exiting the game now.\n"



let split_str str = 
  let initial_lst = String.split_on_char ' ' str in 
  let rec clean_list result = function 
    | [] -> result
    | h :: t -> if h = "" then clean_list result t 
      else clean_list (result @ [h]) t in clean_list [] initial_lst 

let lowercase_capitalize str = 
  str |> String.lowercase_ascii |> String.capitalize_ascii

let parse_player_info info names markers = 
  let str_lst = split_str info in match str_lst with 
  | [] -> raise Empty
  | h :: [] -> 
    let name = lowercase_capitalize h in 
    if List.mem name names then 
      raise (PlayerException ("Error: The name '" ^ 
                              name 
                              ^ "' is already taken. \n Please choose another name. \n"))
    else 
      let marker = List.hd markers in (name, marker)
  | _ -> raise (PlayerException "Error: The name must be one word. \n")

let parse_int input = 
  let num = input |> String.trim |> int_of_string in 
  if num < 2 then raise (PlayerException 
                           "Error: Please enter a number greater than 1. \n")
  else if num > 4 then raise (PlayerException 
                                "Error: Please enter a number less than 5. \n")
  else num

let rec check_player_num feedback = 
  ANSITerminal.(print_string [default] 
                  ("" ^ feedback));
  print_string ("Choose the number of players in the game: there must be at least 2 players, and the max number is 4. \nIf you want 3 players, enter '3'. \n");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> raise Empty
  | input -> 
    try parse_int input
    with 
    | PlayerException s -> check_player_num s
    | Failure _ -> check_player_num 
                     ("Error: You did not enter a valid integer. \n") 


let rec check_player_info feedback player_num names markers = 
  ANSITerminal.(print_string [default] 
                  ("" ^ feedback));
  print_string ("Player " ^ string_of_int player_num 
                ^ ": enter your player name. \n Ex: If your name is Joe, enter 'Joe'. \n");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> raise Empty
  | input -> 
    try parse_player_info input names markers
    with 
    | PlayerException s -> check_player_info s player_num names markers 
    | Empty -> check_player_info 
                 ("Error: You entered an empty input. \n") 
                 player_num names markers 

let filter_options options = 
  if String.contains options 'R' 
  then let index_r = String.index options 'R' in 
    let left_index = index_r  in 
    let right_index = index_r + 6 in 
    let final_len = String.length options - right_index in 
    String.sub options 0 left_index ^ String.sub options right_index final_len 
  else options

let rec collect_player_info curr total acc names markers = 
  if curr > total then acc else 
    let player_info_tuple = check_player_info "" curr names markers in 
    let name = fst player_info_tuple in 
    collect_player_info (curr + 1) total (acc @ [player_info_tuple]) 
      (names @ [name]) (List.tl markers)

let end_game message = print_string message; Stdlib.exit 0

let print_info state = 
  print_string(current_tile state);
  print_string(view_options state);
  print_string("What would you like to do? \n")

let rec handle_turn feedback state = 
  ANSITerminal.(print_string [default] 
                  ("" ^ feedback));
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> raise Empty
  | input -> 
    try 
      let command = Command.parse input in 
      let is_valid = valid_command state command in 
      if not is_valid then 
        handle_turn (Command.to_string command 
                     ^ " is not a valid command for you at this moment in the game. Enter another command. \n") 
          state 
      else handle_valid_command state command
    with 
    | Command.Unparsable s -> handle_turn (s ^ "\n") state 

and handle_valid_command state command = 
  let state_after_command = handle_command state command in 
  if command = Command.End_Turn || command = Command.Forfeit 
     || command = Command.Quit then (state_after_command, command)
  else 
    let options = view_options state_after_command |> filter_options in 
    handle_turn ("\n" ^ current_tile state_after_command 
                 ^ player_stat state_after_command 
                 ^ "\n" ^ options 
                 ^ "What would you like to do next? \n\n") 
      state_after_command 


let rec play_game state = 
  match winner state with 
  | Some p -> end_game ("************************************************** \n" 
                        ^ end_message ^ "Congrats! " 
                        ^ Player.get_name p ^ 
                        " is the winner of this game! \n" ^ goodbye) 
  | None -> 
    print_string ("************************************************** \n" 
                  ^ "* Type 'quit' at any point to end the game. \n\n" ^ 
                  player_stat state ^ "\n");
    print_info state;
    let turn_result = handle_turn "" state in 
    let state_after_turn = fst turn_result in 
    let command_after_turn = snd turn_result in 
    let output = output_of_command command_after_turn 
    in print_string output;
    if command_after_turn = Command.Quit 
    then end_game end_message
    else play_game state_after_turn

and output_of_command (command : Command.t) : string = 
  match command with 
  | Command.Forfeit -> "You have forfeited the game. \n\n"
  | Command.End_Turn -> "You have ended your turn. \n\n"
  | _ -> ""


let init_game board = 
  let num_players = check_player_num "" in 
  let all_player_info = collect_player_info 1 num_players [] [] 
      ["Circle"; "Diamond"; "Square"; "Triangle"] in 
  let state = init_state board all_player_info in 
  print_string start_message;
  play_game state 

let start_game f =
  let file = String.trim f in 
  if file = "" then 
    let () = print_string ("You have chosen to use the default board.\n\n") in 
    Yojson.Basic.from_file "board.json" |> process_json |> init_game
  else if Sys.file_exists f then 
    let () = print_string ("You have chosen to use the custom board \"" 
                           ^ file ^ "\". \n\n" ) in 
    Yojson.Basic.from_file file |> process_json |> init_game
  else print_string ("The file \"" ^ 
                     file ^ "\" does not exist. Create this file or try using a different one.\n");
  flush stdout

let main () =
  ANSITerminal.(print_string [cyan] "");
  print_string opening_message;
  print_endline "To use a custom board, enter the name of the board file you want to load.\nTo use the default Cornell board, just press 'Enter'.";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start_game file_name

(* Execute the game engine. *)
let () = main ()
