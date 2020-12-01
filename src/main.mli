
(** [process_json json] creates the adventure from the json input and
    initializes a starting state. *)
val process_json : Yojson.Basic.t -> 'a

(** [play_game f] starts the game constructed in file [f]. *)
val play_game : string -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit