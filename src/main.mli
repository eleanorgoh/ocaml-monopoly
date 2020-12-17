
(** Raised if user input is empty. *)
exception Empty 

(** [process_json json] creates the adventure from the json input and
    initializes a starting state. *)
val process_json : Yojson.Basic.t -> Newboard.t

(** [check_player_num feedback] takes in user input from the 
    command line and attempts to parse it into an int indicating the number 
    of players in the game. 

    If the input cannot be parsed, [feedback] is printed, and the user 
    is prompted to enter another input. *)
val check_player_num : string -> int

(** [check_player_info feedback num names markers] takes in user input from the 
    command line and attempts to parse it into a string tuple in which the 
    first element is the player's name and the second element is their marker. 
    [names] is a list of names that have already been taken. 
    [markers] is a list of markers that have already been taken. 

    If: 
    (1) the input cannot be parsed
    (2) the input resolves to a name that has already been taken
    (3) the input resolves to a marker that has already been taken
    then [feedback] is printed, and the user is prompted to 
    enter another input.  *)
val check_player_info : string -> int -> string list -> string list -> string * string

(** [handle_turn feedback state] is a tuple in which the first element is 
    the state after processing a command from the user and the second element 
    is that command. 

    The user is continuously prompted to enter commands as long as they have
    not ended their turn.  *)
val handle_turn : string -> State.t -> State.t * Command.t

(** [play_game state] plays the game managed by [state]. *)
val play_game : State.t -> unit

(** [init_game board] initializes the game represented by [board]. *)
val init_game : Newboard.t -> unit 

(** [sample_board] is a sample board created from 'board.json'. *)
val sample_board : Newboard.t

(** [end_game message] prints [message] and ends the game. *)
val end_game : string -> unit

(** [start_game f] starts the game constructed in file [f]. *)
val start_game : string -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit
