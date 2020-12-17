(** Responsible for rendering the graphics of the board. *)

(** Initalizes graphics window. *)
val init_window : unit

(** [draw_state st] renders the board, players, positions, and 
    buildings of the current game state [st]. *)
val draw_state : State.t -> unit
