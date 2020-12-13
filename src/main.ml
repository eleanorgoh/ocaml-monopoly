open Newboard 

let process_json json = Newboard.from_json json

let sample_board = Yojson.Basic.from_file "board.json" |> process_json

let play_game f =
  if Sys.file_exists f then ()
  else print_string ("The file \"" ^ f ^ "\" does not exist. Create this file 
  or try using a different one.\n");
  flush stdout

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Monopoly...for camels!\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
(* let () = main () *)
