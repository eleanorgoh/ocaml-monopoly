
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

(** [split_str str] is an empty list or a list of strings in which every 
    element is a non-empty string. *)
let split_str str = 
  let initial_lst = String.split_on_char ' ' str in 
  let rec clean_list result = function 
    | [] -> result
    | h :: t -> if h = "" then clean_list result t 
      else clean_list (result @ [h]) t in clean_list [] initial_lst 

(** [capitalize_property str1 str2] is the string formed by capitalizing [str1], 
    capitalizing [str2], and concatenating them with a space in between. 

    Ex: [capitalize_property 'atlantic' 'ave'] -> "Atlantic Ave" *)
let capitalize_property str1 str2 = 
  let cap_str1 = String.capitalize_ascii str1 in 
  let cap_str2 = String.capitalize_ascii str2 in 
  cap_str1 ^ " " ^ cap_str2

(** [check_one_arg_input str] is either [Start], [Quit], [Restart], [Forfeit], 
    [Roll] or [Help] if [str] matches "start", "quit", "restart", "forfeit", 
    "roll" or "help", respectively. Otherwise, raises Unparsable exception. *)
let check_one_arg_input = function
  | "start" -> Start
  | "quit" -> Quit
  |"restart" -> Restart
  | "forfeit" -> Forfeit
  | "roll" -> Roll
  | "help" -> Help
  | _ -> raise (Unparsable "Please enter a valid command.")

(** [check_two_arg_input str1 str2] is [Player_Name str2] if [str1] = "name", 
    [End_turn] if [str1] = "end" and [str2] = "turn", 
    [Draw_Chance] if [str1] = "draw" and [str2] = "chance" 
    or [Draw_Community] if [str1] = "draw" and [str2] = "community". 
    Otherwise, raises Unparsable exception. *)
let check_two_arg_input str1 str2 = 
  match str1 with 
  | "name" -> Player_Name str2
  | "end" -> if str2 = "turn" then End_Turn 
    else raise (Unparsable "Please enter a valid command. 
      Did you mean 'end turn'?")
  | "draw" -> if str2 = "chance" then Draw_Chance 
    else if str2 = "community" then Draw_Community 
    else raise (Unparsable "Please enter a valid command. 
        Did you mean 'draw chance' or 'draw community'?")
  | _ -> raise (Unparsable "Please enter a valid command.")

(** [check_buysell_input str1 prop_begin prop_end] uses 
    [capitalize_property prop_begin prop_end] to obtain the full property name 
    [prop] and is [Buy prop] if [str1] = "buy" and 
    [Sell prop] if [str1] = "sell". 
    Otherwise, raises Unparsable exception. *)
let check_buysell_input str1 prop_begin prop_end = 
  let prop = capitalize_property prop_begin prop_end in 
  match str1 with 
  | "buy" -> Buy prop
  | "sell" -> Sell prop
  | _ -> raise (Unparsable "Please enter a valid command.")

(** [check_collect_input str1 p1 p2 prop_begin prop_end] uses 
    [capitalize_property prop_begin prop_end] to obtain the full property name 
    [prop] and is [Collect (p1, p2, prop)] if [str1] = "collect", [p1] is not 
    the same as [p2], and [p1] and [p2] are not equal to [prop]. 
    Otherwise, raises Unparsable exception. *)
let check_collect_input str1 p1 p2 prop_begin prop_end = 
  match str1 with 
  | "collect" -> if p1 <> p2 && prop_begin <> prop_end && p1 <> prop_begin 
                    && p2 <> prop_begin 
    then let prop = capitalize_property prop_begin prop_end in 
      Collect (p1, p2, prop) 
    else raise (Unparsable "Please enter a valid command. If you want to 
  collect money from someone, enter the command as follows: 
  'collect <your username> <their username> <property name>'.")
  | _ -> raise (Unparsable "Please enter a valid command.")


(** [create_command lst] is a command if [lst] matches the format of 
    a valid command. Otherwise, raises Unparsable exception. *)
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