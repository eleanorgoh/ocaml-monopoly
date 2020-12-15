
type t = 
  | Start
  | Quit
  | Restart
  | End_Turn
  | Forfeit 
  | Player_Name of string
  | Roll 
  | Draw_Chance
  | Draw_Community 
  | Buy of string 
  | Sell of string 
  | Collect of (string * string * string)
  | Help

exception Unparsable of string

let split_str str = 
  let initial_lst = String.split_on_char ' ' str in 
  let rec clean_list result = function 
    | [] -> result
    | h :: t -> if h = "" then clean_list result t 
      else clean_list (result @ [h]) t in clean_list [] initial_lst 

let capitalize_property str1 str2 = 
  let cap_str1 = String.trim str1 |> String.capitalize_ascii in 
  let cap_str2 = String.trim str2 |> String.capitalize_ascii in 
  if cap_str1 <> "" && cap_str2 <> "" then cap_str1 ^ " " ^ cap_str2 
  else if cap_str1 = "" then cap_str2 
  else if cap_str2 = "" then cap_str1 
  else ""

let check_one_arg_input = function
  | "start" -> Start
  | "quit" -> Quit
  |"restart" -> Restart
  | "forfeit" -> Forfeit
  | "roll" -> Roll
  | "help" -> Help
  | _ -> raise (Unparsable "Please enter a valid command.")

let check_two_arg_input str1 str2 = 
  match str1 with 
  | "name" -> let new_p1 = String.trim str2 |> String.capitalize_ascii in 
    Player_Name new_p1
  | "end" -> if str2 = "turn" then End_Turn 
    else raise (Unparsable "Please enter a valid command. 
      Did you mean 'end turn'?")
  | "draw" -> if str2 = "chance" then Draw_Chance 
    else if str2 = "community" then Draw_Community 
    else raise (Unparsable "Please enter a valid command. 
        Did you mean 'draw chance' or 'draw community'?")
  | _ -> raise (Unparsable "Please enter a valid command.")

let check_buysell_input str1 prop_begin prop_end = 
  let prop = capitalize_property prop_begin prop_end in 
  match str1 with 
  | "buy" -> Buy prop
  | "sell" -> Sell prop
  | _ -> raise (Unparsable "Please enter a valid command.")

let check_collect_input str1 p1 p2 prop_begin prop_end = 
  let new_p1 = String.trim p1 |> String.capitalize_ascii in
  let new_p2 = String.trim p2 |> String.capitalize_ascii in
  match str1 with 
  | "collect" -> if new_p1 <> new_p2 && prop_begin <> prop_end && 
                    new_p1 <> prop_begin && new_p2 <> prop_begin 
    then let prop = capitalize_property prop_begin prop_end in 
      Collect (new_p1, new_p2, prop) 
    else raise (Unparsable "Please enter a valid command. If you want to 
  collect money from someone, enter the command as follows: 
  'collect <your username> <their username> <property name>'.")
  | _ -> raise (Unparsable "Please enter a valid command.")


let create_command = function 
  | [] -> raise (Unparsable "Please enter a non-empty command.")
  | h :: [] -> check_one_arg_input h
  | h :: t :: [] -> check_two_arg_input h t
  | h :: str1 :: str2 :: [] -> check_buysell_input h str1 str2 
  | h :: p1 :: p2 :: str1 :: str2 :: [] -> check_collect_input h p1 p2 str1 str2
  | _ -> raise (Unparsable "Please enter a valid command.")

let parse str =
  let lowercase_str = String.lowercase_ascii str in 
  let str_lst = split_str lowercase_str in match str_lst with 
  | [] -> raise (Unparsable "Please enter a non-empty command.")
  | h :: t -> create_command str_lst

let to_string commmand = 
  match commmand with 
  | Start -> "Start"
  | Quit -> "Quit"
  | Restart -> "Restart"
  | End_Turn -> "End turn"
  | Forfeit -> "Forfeit"
  | Player_Name s -> "Create player: " ^ s
  | Roll -> "Roll"
  | Draw_Chance -> "Draw a chance card"
  | Draw_Community -> "Draw a commmunity chest card"
  | Buy s  -> "Buy " ^ s
  | Sell s  -> "Sell " ^ s
  | Collect (s1, s2, s3) -> s1 ^ " is collecting " ^ s3 ^ " from " ^ s2
  | Help -> "Help"
