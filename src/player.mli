type t

(** [init_new_player name id marker] initializes a new player with 
    name [name], id [id] and marker [marker]. *)
val init_new_player : string -> int -> string -> t

(** [get_name p] is the name of player [p]. *)
val get_name : t -> string 

(** [get_account p] is the account id of player [p]. *)
val get_account : t -> int 

(** [get_marker_type p] is the marker type of player [p]. *)
val get_marker_type : t -> string

(** [set_name p name] sets the name of player [p] to [name]. *)
val set_name : t -> string -> t

(** [set_account_id p id] sets the account id of player [p] to [id]. *)
val set_account_id : t -> int -> t

(** [set_marker_type p marker] sets the marker type of player [p] 
    to [marker]. *)
val set_marker_type : t -> string -> t