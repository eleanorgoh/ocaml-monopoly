
(** [t] is the abstract type of values representing an Action. *)
(* type t *)

(** [t] is the outcome of a player action and is one of the following:
    (i) Jail if the player rolls three sets of doubles and goes to jail
    (ii) Steps representing the total number of steps a player takes
    if the player doesn't go to jail.
    (iii) Draw_Chance if the player should draw from the deck of Chance cards.
    (iv) Draw_Community if the player should draw from the deck of Community 
    Chest cards.
*)

type t = 
  | Jail
  | Step of int 
  | Draw_Chance 
  | Draw_Community

(** [roll_dice] is the outcome of a player's roll in one turn. *)
val roll_dice : t

(** [draw_community] is the action of drawing from the 
    Community Chest deck. *)
val draw_community : t

(** [roll_dice] is the action of drawing from the 
    Chance deck.*)
val draw_chance : t