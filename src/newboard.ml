open Property

type board = Property.t list
type t = board
open Yojson.Basic.Util

let match_color (str : string) : Property.color = 
  match str with
  | "Brown" -> Property.Brown
  | "Light Blue" -> Property.Light_Blue
  | "Pink" -> Property.Pink
  | "Orange" -> Property.Orange
  | "Red" -> Property.Red
  | "Yellow" -> Property.Yellow
  | "Green" -> Property.Green
  | "Dark Blue" -> Property.Dark_Blue
  | _ -> failwith "Color DNE."

let match_type (str : string) : Property.tile_type = 
  match str with
  | "Property" -> Property.Property
  | "Railroad" -> Property.Railroad
  | "Utility" -> Property.Utility
  | "Tax" -> Property.Tax
  | "Chance Card" -> Property.Chance_card
  | "Community Chest" -> Property.Community_chest
  | "Free Parking" -> Property.Free_parking
  | "Go To Jail" -> Property.Go_to_jail
  | "In Jail Just Visiting" -> Property.In_jail_just_visiting
  | "Go" -> Property.Go
  | _ -> failwith "Type DNE."


let tile_json (j : Yojson.Basic.t) = 
  let tile_type = j |> member "type" |> to_string |> match_type in
  match tile_type with
  | Property -> 
    let pos = j |> member "position" |> to_int in
    let name = j |> member "name" |> to_string in
    let color = j |> member "color" |> to_string |> match_color in
    let r0 = j |> member "rent_no_house" |> to_int in
    let r1 = j |> member "rent_1_house" |> to_int in
    let r2 = j |> member "rent_2_house" |> to_int in
    let r3 = j |> member "rent_3_house" |> to_int in
    let r4 = j |> member "rent_4_house" |> to_int in
    let rh = j |> member "rent_hotel" |> to_int in
    let bc = j |> member "building_cost" |> to_int in
    let p = j |> member "price" |> to_int in
    Property.init_property pos name color tile_type r0 r1 r2 r3 r4 rh bc p 0
  | Railroad ->
    let pos = j |> member "position" |> to_int in
    let name = j |> member "name" |> to_string in
    let color = Property.No_Color in
    let r0 = j |> member "rent_no_station" |> to_int in
    let rs = j |> member "rent_station" |> to_int in
    let bc = j |> member "building_cost" |> to_int in
    let p = j |> member "price" |> to_int in
    Property.init_property pos name color tile_type 0 0 0 0 r0 rs bc p 4
  | Utility ->
    let pos = j |> member "position" |> to_int in
    let color = Property.No_Color in
    let name = j |> member "name" |> to_string in
    let r = j |> member "rent_no_house" |> to_int in
    let p = j |> member "price" |> to_int in
    Property.init_property pos name color tile_type 0 0 0 0 0 r 0 p 5
  | Tax ->
    let pos = j |> member "position" |> to_int in
    let color = Property.No_Color in
    let name = j |> member "name" |> to_string in
    let tax = j |> member "tax" |> to_int in
    Property.init_property pos name color tile_type 0 0 0 0 0 tax 0 0 5
  | Chance_card
  | Community_chest
  | Free_parking
  | Go_to_jail
  | In_jail_just_visiting
  | Go ->
    let pos = j |> member "position" |> to_int in
    let name = j |> member "name" |> to_string in
    let color = Property.No_Color in
    Property.init_property pos name color tile_type 0 0 0 0 0 0 0 0 5

let json_board (j : Yojson.Basic.t) : t = 
  j |> member "tiles" |> to_list |> List.map tile_json

let from_json (json : Yojson.Basic.t) : t = 
  try json_board json
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let rec get_tile (board : t) (position : int) : Property.t = 
  match board with 
  | [] -> failwith ("Position does not exist.")
  | h::t -> if get_pos h = position then h
    else get_tile t position

let get_name (board : t) (position : int) : string =
  let tile = get_tile board position in get_name tile

let get_names (board : t) : string list = 
  List.map (fun x -> Property.get_name x) board

let get_color (board : t) (position : int) : color = 
  let tile = get_tile board position in get_color tile

let get_rent (board : t) (position : int) : int = 
  let tile = get_tile board position in get_rent_cost tile

let get_building_cost (board : t) (position : int) : int = 
  let tile = get_tile board position in get_building_cost tile

let get_price (board : t) (position : int) : int = 
  let tile = get_tile board position in get_price tile

let get_num_buildings (board : t) (position : int) : int = 
  let tile = get_tile board position in get_num_buildings tile

let get_type (board : t) (position : int) : tile_type = 
  let tile = get_tile board position in get_type tile

let rec reset_helper board (names : string list) newBoard = 
  match board with 
  | [] -> newBoard
  | h::t -> if List.mem (Property.get_name h) names
    then reset_helper t names (newBoard@[Property.reset_property h])
    else reset_helper t names (newBoard@[h])

let reset_properties (board : t) (player : Player.t) = 
  let properties = Player.get_properties player in 
  let names = List.map (fun x -> Property.get_name x) properties in 
  reset_helper board names []
