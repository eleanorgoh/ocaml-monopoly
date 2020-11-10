(** Defines Community Chest/Chance cards. *)

(** [t] is the abstract type of values representing a CC card. *)
type t

(** [category] defines the type of the card. *)
type category = 
  | Community
  | Chance

(** [action] defines the action the player can take with this card. 
    Players can: 
    (i) Pay others 
    (ii) Receive money from others
    (iii) Get out of jail for free
    (iv) Go to jail
    (v) Advance to another location on the board
*)
type action = 
  | Pay of int
  | Receive of int
  | OutJail
  | GoJail
  | Advance of string

(** [init_card ctg name act] is a card with category [ctg], name [name], 
    and action [act]. *)
val init_card : category -> string -> action -> t

(** [get_category card] is the category of [card]. *)
val get_category : t -> category

(** [get_action card] is the action of [card]. *)
val get_action : t -> category
