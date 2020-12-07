type color = 
  | Brown
  | Light_Blue
  | Pink
  | Orange
  | Red 
  | Yellow
  | Green
  | Dark_Blue
  | No_Color

type tile_type = 
  | Property
  | Railroad
  | Utility
  | Tax
  | Chance_card
  | Community_chest
  | Free_parking
  | Go_to_jail
  | In_jail_just_visiting
  | Go

type t = {
  position : int;
  name : string;
  color : color;
  tile_type : tile_type;
  rent_no_house : int;
  rent_1_house : int;
  rent_2_house : int;
  rent_3_house : int;
  rent_4_house : int;
  rent_hotel : int;
  building_cost : int;
  price : int;
  mutable num_buildings : int;
}

let init_property 
    (pos : int) (name : string) (color : color) (tile_type : tile_type)
    (rent_no_house : int) (rent_1_house : int) (rent_2_house : int)
    (rent_3_house : int) (rent_4_house : int) (rent_hotel : int)
    (building_cost : int) (price : int) (num_buildings : int)= {
  position = pos;
  name = name;
  color = color;
  tile_type = tile_type;
  rent_no_house = rent_no_house;
  rent_1_house = rent_1_house;
  rent_2_house = rent_2_house;
  rent_3_house = rent_3_house;
  rent_4_house = rent_4_house;
  rent_hotel = rent_hotel;
  building_cost = building_cost;
  price = price;
  num_buildings = num_buildings;
}

let get_name (property : t) : string = property.name 

let get_color (property : t) : color = property.color

let get_rent_cost (property : t) : int = 
  match property.num_buildings with
  | 0 -> property.rent_no_house
  | 1 -> property.rent_1_house
  | 2 -> property.rent_2_house
  | 3 -> property.rent_3_house
  | 4 -> property.rent_4_house
  | 5 -> property.rent_hotel
  | _ -> failwith ("Too many buildings.")

let get_building_cost (property : t) : int = property.building_cost

let get_price (property : t) : int = property.price

let get_num_buildings (property : t) : int = property.num_buildings

let get_type (property : t) : tile_type = property.tile_type

let get_pos (property : t) : int = property.position

let add_building (property : t) = 
  if property.num_buildings = 5 then failwith ("Too many buildings.")
  else property.num_buildings <- property.num_buildings + 1
