(* type tile_type = 
   | Property
   | Railroad
   | Utility
   | Tax
   | Chance_card
   | Community_chest
   | Free_parking
   | Go_to_jail
   | In_jail_just_visiting
   | Go *)

type tile = {
  position : int;
  tile_type : string;
  name : string;
  color : Property.color option;
  prices : (string * int) list option
}

type board = tile list
type t = board
open Yojson.Basic.Util

let match_color str = 
  match str with 
  | "" -> None
  | "Brown" -> Some Property.Brown
  | "Light Blue" -> Some Property.Light_Blue
  | "Dark Blue" -> Some Property.Brown
  | "Pink" -> Some Property.Brown
  | "Orange" -> Some Property.Brown
  | "Red" -> Some Property.Brown
  | "Yellow" -> Some Property.Brown
  | "Green" -> Some Property.Brown
  | _ -> failwith ("Error in JSON file, improper color.")

let rec reformat lst new_lst : (string * int) list = 
  match lst with
  | [] -> new_lst
  | h::t -> reformat t ((fst h, to_int (snd h))::new_lst)

let match_prices lst : (string * int) list option= 
  match lst with 
  | [] -> None
  | h::t -> Some (reformat (h::t) [])

let tile_json (j : Yojson.Basic.t) = {
  position = j |> member "position" |> to_int;
  tile_type = j |> member "type" |> to_string;
  name = j |> member "name" |> to_string;
  color = j |> member "color" |> to_string |> match_color;
  prices = j |> member "prices" |> to_assoc |> match_prices
}

let json_board (j : Yojson.Basic.t) : t = 
  j |> member "tiles" |> to_list |> List.map tile_json

let from_json (json : Yojson.Basic.t) : t = 
  try json_board json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec get_type (board : t) (position : int) : string =
  match board with
  | [] -> failwith ("Position does not exist.")
  | h::t -> if h.position = position then h.tile_type
    else get_type t position

let rec get_name (board : t) (position : int) : string =
  match board with
  | [] -> failwith ("Position does not exist.")
  | h::t -> if h.position = position then h.name
    else get_name t position

let rec get_color (board : t) (position : int) : Property.color option =
  match board with
  | [] -> failwith ("Position does not exist.")
  | h::t -> if h.position = position then h.color
    else get_color t position

let rec get_prices (board : t) (position : int) : (string * int) list option =
  match board with
  | [] -> failwith ("Position does not exist.")
  | h::t -> if h.position = position then h.prices
    else get_prices t position