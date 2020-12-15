(** 
   Representation of dynamic game state.

   This module represents the state of a monopoly game as it is being played,
   including each player's current tile, player's balance, chance card 
   and community chest card order, and functions that cause the state to change.
*)

(** The abstract type of values representing the game state. *)
type t 

(** [init_state board lst] is the initial state of the game when playing 
    monopoly game [board]. In that state, there are players with names and 
    markers [(name,marker)], and all of them are on the Go tile with a balance 
    of $1500. No player owns any properties. *)
val init_state : Newboard.t -> (string * string) list ->  t

(** [get_player_pos game] is a list of all the players and their positions on 
    the board. Each entry in the list is formatted as (player name, position).*)
val get_player_pos : t -> (string * int) list

(** [get_board game] is the static board data of [game]. *)
val get_board : t -> Newboard.t

(** [current_pos game name] is the position of the board that the player's piece 
    is on. *)
val current_pos : t -> string -> int

(** [player_stat game] is the stat summary of the current player of the game. *)
val player_stat : t -> string

(** [player_stat game] is the name the tile that the current player is on.*)
val current_tile : t -> string

(** [get_winner game] is the winner of the game if there is one.*)
val winner : t -> Player.t option

(** [handle_command board command] handles the command request from the player
    and updates the board. *)
val handle_command : t -> Command.t -> t

(** [handle_command board command] checks if [command] is valid for the player
    at the current position. *)
val valid_command : t -> Command.t -> bool
