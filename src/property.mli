
(* Properties may be bought in one of 3 ways: landing on the property space and buying it, being the highest bidder in an auction for property, or buy it from 
   an opponent in a trade. Properties may also be received from bankrupted players, provided the Bank didn't bankrupt them.

   When a player buys or otherwise gains possession of a property, he or she receives the property's corresponding title deed, which lists all relevant information on the property.

   When a player owns all the properties in a color group (or 2/3 to 3/4 in the 
   mega version), she or he is said to have a monopoly, which allows the player to charge double rent or build it up with Houses and Hotels.

   Property costs and rents escalate as the player rounds the board. Properties range anywhere from $60 to $400, while rents can range from $2 to $2000!  *)

(** [t] is the abstract type of values rpepresent a property. *)
type t 

(** [color] is the color of the property. *)
type color = 
  | Brown
  | Light_Blue
  | Pink
  | Orange
  | Red 
  | Yellow
  | Green
  | Dark_Blue

(** [init_property string color r0 r1 r2 r3 r4 r5 bc pr nb] is a 
    Property with name [string], 
    color [color], rent_no_house [r0], rent_1_house [r1], rent_2_house [r2], 
    rent_3_house [r3], rent_4_house [r4], rent_house [r5], 
    building_cost [bc], price [pr], num_buildings [nb]. *)
val init_property : string -> color -> int -> int -> int -> 
  int -> int -> int -> int -> int -> int ->  t

(** [get_owner property] is either Some player that owns the card or None. *)
(* val get_owner : t -> Player.t option *)

(** [get_name property] is the name of [property]. *)
val get_name : t -> string

(** [get_color property] is the color of [property]. *)
val get_color : t -> color

(** [get_rent_cost property] is the rent price of [property]. *)
val get_rent_cost : t -> int

(** [get_building_cost property] is the cost of building a house/hotel on 
    [property].*)
val get_building_cost : t -> int

(** [get_price property property] is the purchase price of [property]. *)
val get_price : t -> int 

(** [get_num_buildings property] is the number of buildings on [property]. *)
val get_num_buildings : t -> int

(** [add_building prop] is [property] after adding a new building. 
    Requires: property has less than 5 buildings.*)
val add_building : t -> unit