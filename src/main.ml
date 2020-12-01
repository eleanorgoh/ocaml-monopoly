
let process_json input =
  failwith "TODO"

let play_game f =
  if Sys.file_exists f then Yojson.Basic.from_file f |> process_json
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
  | file_name -> failwith "TODO"

(* Execute the game engine. *)
let () = main ()