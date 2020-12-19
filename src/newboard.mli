(** 
   Representation of static board data.

   This module represents the data stored in board files, including each tile's 
   position and what each tile is. It handles loading of that data from JSON
   as well as querying the data.
*)

(** The abstract type of values representing the board. *)
type t = Property.t list

(** [from_json j] is the board that [j] represents.
    Requires: [j] is a valid JSON board representation. *)
val from_json : Yojson.Basic.t -> t

(** [get_tile board position] is the tile at [position]. *)
val get_tile : t -> int -> Property.t

(** [get_name board position] is the name for the tile at [position]. *)
val get_name : t -> int -> string

(** [get_names board] is the list of names of all the tiles. *)
val get_names : t -> string list

(** [get_type board position] is the type for the tile at [position]. *)
val get_type : t -> int -> Property.tile_type

(** [get_color board position] is the color for the tile at [position]. *)
val get_color : t -> int -> Property.color

(** [get_rent board position] is the price of rent for the tile at [position].*)
val get_rent : t -> int -> int

(** [get_building_cost board position] is the building cost 
    of a house for the tile at [position]. *)
val get_building_cost : t -> int -> int

(** [get_num_buildings board position] is the number of buildings 
    on the tile at [position]. *)
val get_num_buildings : t -> int -> int

(** [reset_properties board p] resets all the tiles that player [p] owned. 
    This is for when a player forfeits and all their buildings are removed.*)
val reset_properties : t -> Player.t -> t