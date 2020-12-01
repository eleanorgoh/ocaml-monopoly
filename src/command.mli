
(** The type of values representing a command a player can issue. 
    1. [Start]: start the game.
    2. [Quit]: quit the game.
    3. [Restart]: restart the game.
    4.[End_Turn]: end the current player's turn.
    5. [Forfeit]: forfeit the current player's standing.
    6. [Player_Name str]: [str] is the name of a new player where [str] is 
    the player's capitalized name.
    7. [Roll]: roll dice.
    8. [Draw_Chance]: draw from the deck of Chance cards.
    9. [Draw_Community]: draw from the deck of Community Chest cards.
    10. [Buy prop]: [prop] is the name of a property the current player 
    wants to buy.
    11. [Sell prop]: [prop] is the name of a property the current player 
    wants to sell.
    12. [Collect triple]: [triple] is a string triple in which the first 
    element is the name of the current player, the second element is the 
    name of the player from whom money is being collected, and the third 
    element is the name of the property associated with this transaction.
    The names of both players are capitalized. 
    13. [Help]: get instructions on what commands the current player can issue.
*)
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

(** Raised when a command cannot be parsed. *)
exception Unparsable of string

(** [split_str str] is an empty list or a list of strings in which every 
    element is a non-empty string resulting from splitting [str] into 
    individual words. *)
val split_str : string -> string list

(** [capitalize_property str1 str2] is the string formed by removing leading 
    and trailing whitespace from [str1] and [str2], capitalizing both of 
    them, and concatenating them with a space in between if 
    [str1] and [str2] are not empty strings.

    If [str1] is empty, then the result is just [str1] capitalized. 
    If [str2] is empty, then the result is just [str2] capitalized.
    If both are empty, then the result is just the empty string.

    Ex: [capitalize_property 'atlantic' 'ave'] -> "Atlantic Ave" *)
val capitalize_property : string -> string -> string 

(** [check_one_arg_input str] is either [Start], [Quit], [Restart], [Forfeit], 
    [Roll] or [Help] if [str] matches "start", "quit", "restart", "forfeit", 
    "roll" or "help", respectively. Otherwise, raises Unparsable exception. *)
val check_one_arg_input : string -> t

(** [check_two_arg_input str1 str2] is [Player_Name str2] if [str1] = "name", 
    [End_turn] if [str1] = "end" and [str2] = "turn", 
    [Draw_Chance] if [str1] = "draw" and [str2] = "chance" 
    or [Draw_Community] if [str1] = "draw" and [str2] = "community". 
    Otherwise, raises Unparsable exception. *)
val check_two_arg_input : string -> string -> t

(** [check_buysell_input str1 prop_begin prop_end] uses 
    [capitalize_property prop_begin prop_end] to obtain the full property name 
    [prop] and is [Buy prop] if [str1] = "buy" and 
    [Sell prop] if [str1] = "sell". 
    Otherwise, raises Unparsable exception. *)
val check_buysell_input : string -> string -> string -> t

(** [check_collect_input str1 p1 p2 prop_begin prop_end] uses 
    [capitalize_property prop_begin prop_end] to obtain the full property name 
    [prop] and is [Collect (p1, p2, prop)] if [str1] = "collect", [p1] is not 
    the same as [p2], and [p1] and [p2] are not equal to [prop]. 
    Otherwise, raises Unparsable exception. *)
val check_collect_input : string -> string -> string -> string -> string -> t

(** [create_command lst] is a command if [lst] matches the format of 
    a valid command. Otherwise, raises Unparsable exception. *)
val create_command : string list -> t

(** [parse str] is the command formed after parsing [str], the current 
    player's input. Given an input [str], commands are formed as follows using 
    the name "Stacy" as an example of the current player's name: 
    1. [parse "start"]: [Start]
    2. [parse "quit"]: [Quit]
    3. [parse "restart"]: [Restart]
    4. [parse "end turn"]: [End_Turn]
    5. [parse "forfeit"]: [Forfeit]
    6. [parse "name John"]: [Player_Name "John"]
    7. [parse "roll"]: [Roll]
    8. [parse "draw chance"]: [Draw_Chance]
    9. [parse "draw community"]: [Draw_Community]
    10. [parse "buy atlantic ave"]: [Buy "atlantic ave"]
    11. [parse "sell london ave"]: [Sell "london ave"]
    12. [parse "collect John atlantic ave"]: 
    [Collect ("Stacy", "John", "Atlantic Ave")]
    13. [parse "help"]: [Help]

    [parse] is not case-sensitive, e.g. [parse "name John"] results in the 
    same command as [parse "name john"] -> "[Player_Name "John"]; 
    "parse "sell londON AvE"] results in the same command as 
    [parse "Sell London Ave"] -> [Sell "london ave"].

    Requires: 
    (i) [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).
    (ii) Names of properties are assumed to be composed of two words.

    Raises Unparsable exception if input is empty, only contains space 
    characters, or is malformed.

*)
val parse : string -> t