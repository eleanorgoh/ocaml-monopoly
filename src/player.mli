(** The abstract type of values representing a player. *)
type t

exception UpdateError of string 

(** [init_new_player name marker] initializes a new player with 
    name [name] and marker [marker]. *)
val init_new_player : string -> string -> t

(** [get_name p] is the name of player [p]. *)
val get_name : t -> string 

(** [get_money p] is the amount of money of player [p]. *)
val get_money : t -> int

(** [get_marker_type p] is the marker type of player [p]. *)
val get_marker_type : t -> string

(** [get_properties p] is list of properties of player [p]. *)
val get_properties : t -> Property.t list

(** [get_property_by_name p name] is Some property owned by player [p]
    with name [name] if [p] owns this property. Otherwise, None. *)
val get_property_by_name : t -> string -> Property.t option

(** [add_property p prop] is the player after adding property [prop] to the 
    player [p]'s list of properties if [p] does not own [prop] and UpdateError 
    otherwise. *)
val add_property : t -> Property.t -> t

(** [remove_property p prop] is the player after removing property [prop] from
    the player [p]'s list of properties or UpdateError if [p] does not own 
    [prop]. *)
val remove_property : t -> Property.t -> t

(** [set_name p name] sets the name of player [p] to [name]. *)
val set_name : t -> string -> t

(** [set_money p amt] sets the account of player [p] to [amt]. *)
val set_money : t -> int -> t

(** [set_marker_type p marker] sets the marker type of player [p] 
    to [marker]. *)
val set_marker_type : t -> string -> t