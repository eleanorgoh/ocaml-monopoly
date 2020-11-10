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