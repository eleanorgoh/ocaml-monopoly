(** The abstract type of values representing a player. *)
type t

(** [init_new_player name marker] initializes a new player with 
    name [name] and marker [marker]. *)
val init_new_player : string -> string -> t

(** [get_name p] is the name of player [p]. *)
val get_name : t -> string 

(** [get_money p] is the amount of money of player [p]. *)
val get_money : t -> int

(** [get_marker_type p] is the marker type of player [p]. *)
val get_marker_type : t -> string

(* TODO: get_properties once Property module is done *)

(** [get_properties p] is list of properties of player [p]. *)
(* val get_properties : t -> Property.t list *)

(** [set_name p name] sets the name of player [p] to [name]. *)
val set_name : t -> string -> t

(** [set_money p amt] sets the account of player [p] to [amt]. *)
val set_money : t -> int -> t

(** [set_marker_type p marker] sets the marker type of player [p] 
    to [marker]. *)
val set_marker_type : t -> string -> t