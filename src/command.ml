

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

exception Unparsable of string

let parse str = 
  failwith "TODO"