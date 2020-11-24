
(** The type of values representing a command a player can issue. 
    1. [Start]: start the game.
    2. [Quit]: quit the game.
    3. [Restart]: restart the game.
    4.[End_Turn]: end the current player's turn.
    5. [Forfeit]: forfeit the current player's standing.
    6. [Player_Name str]: [str] is the name of a new player.
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