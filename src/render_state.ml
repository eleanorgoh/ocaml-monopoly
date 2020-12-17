type rstate = {
  circle_pos : int option;
  diamond_pos : int option;
  square_pos : int option;
  triangle_pos : int option;
  board : Newboard.t;
}

let init_state board = {
  circle_pos = None;
  diamond_pos = None;
  square_pos = None;
  triangle_pos = None;
  board = board
}

let get_board (st : rstate) = st.board

let add_building_at_pos pos rstate = 
  let property_tile = Newboard.get_tile rstate.board pos in 
  match Property.get_type property_tile with 
  | Property -> 
    begin 
      try Property.add_building property_tile; rstate
      with Failure _ -> rstate
    end
  | _ -> rstate

let reset_buildings_at_pos pos rstate = 
  let property_tile = Newboard.get_tile rstate.board pos in 
  match Property.get_type property_tile with 
  | Property -> 
    begin 
      try Property.change_num_buildings property_tile 0; rstate
      with Failure _ -> rstate
    end
  | _ -> rstate

let move_circle pos rstate = {rstate with circle_pos = Some pos}

let move_diamond pos rstate = {rstate with diamond_pos = Some pos}

let move_square pos rstate = {rstate with square_pos = Some pos}

let move_triangle pos rstate = {rstate with triangle_pos = Some pos}

let lst_posns rstate = 
  [("Circle", rstate.circle_pos); ("Diamond", rstate.diamond_pos);
   ("Square", rstate.square_pos); ("Triangle", rstate.triangle_pos)]