(** 
   Representation of dynamic game state.

   This module represents the state of a monopoly game as it is being played,
   including each player's current tile, player's balance, chance card 
   and community chest card order, and functions that cause the state to change.
*)

(** The abstract type of values representing the game state. *)
type t 

(** [init_state board names] is the initial state of the game when playing 
    monopoly game [board]. In that state, there are players with names [names],
    and all of them are on the Go tile with a balance of $1500. 
    No player owns any properties. *)
val init_state : Newboard.t -> (string * string) list ->  t

(** [current_pos game name] is the position of the board that the player's piece 
    is on. *)
val current_pos : t -> string -> int

(** [get_winner game] is the winner of the game if there is one.*)
val winner : t -> Player.t option

(** [handle_command board command] handles the command request from the player
    and updates the board. *)
val handle_command : t -> Command.t -> t

(** [handle_command board command] checks if the command is valid for the player
    at the current position. *)
val valid_command : t -> Command.t -> bool
