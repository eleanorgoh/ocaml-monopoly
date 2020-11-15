(** 
   Representation of static board data.

   This module represents the data stored in board files, including each tile's 
   position and what each tile is. It handles loading of that data from JSON
   as well as querying the data.
*)

(** The abstract type of values representing the board. *)
type t

(** [from_json j] is the board that [j] represents.
    Requires: [j] is a valid JSON board representation. *)
val from_json : Yojson.Basic.t -> t

(** [get_type board position] is the type for the tile at [position]. *)
val get_type : t -> int -> string

(** [get_name board position] is the name for the tile at [position]. *)
val get_name : t -> int -> string

(** [get_color board position] is the color for the tile at [position]. *)
val get_color : t -> int -> Property.color option

(** [get_prices board position] is the prices list for the tile at [position].*)
val get_prices : t -> int -> (string * int) list option