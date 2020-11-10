type t

(** Initializes a new player with name [name], id [id] and marker [marker]. *)
val init_new_player : string -> int -> string -> t

(** The name of this player. *)
val get_name : t -> string 

(** The account id of this player. *)
val get_account : t -> int 

(** The marker type of this player. *)
val get_marker_type : t -> string

(** Sets the name of this player to [name]. *)
val set_name : t -> string -> t

(** Sets the account id of this player to [id]. *)
val set_account_id : t -> int -> t

(** Sets the marker type of this player to [marker]. *)
val set_marker_type : t -> string -> t