open Player

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

(** [init_property] is a Property with owner [Player], name [string], 
    color [color], rent_no_house [int], rent_1_house [int], rent_2_house [int], rent_3_house [int], rent_4_house [int], rent_house [int], 
    building_cost [int], price [int], num_buildings [int]. *)
val init_property : Player.t -> string -> color -> int -> int -> int -> 
  int -> int -> int -> int -> int -> int ->  t

(** [get_owner property] is the Player that owns the card. *)
val get_owner : t -> Player.t

(** [get_name property] is the name of the property. *)
val get_name : t -> string

(** [get_color property] is the color of the property. *)
val get_color : t -> color

(** [get_rent_cost property] is the rent price of the property. *)
val get_rent_cost : t -> int

(** [get_building_cost] is the cost of building a house/hotel on the property.*)
val get_building_cost : t -> int

(** [get_price property] is the purchase price of the property. *)
val get_price : t -> int 

(** [get_num_buildings] is the number of buildings on the property. *)
val get_num_buildings : t -> int