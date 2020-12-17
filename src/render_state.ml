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
      try Property.add_building property_tile with Failure _ -> () 
    end
  | _ -> ()

let reset_buildings_at_pos pos rstate = 
  let property_tile = Newboard.get_tile rstate.board pos in 
  match Property.get_type property_tile with 
  | Property -> 
    begin 
      try Property.change_num_buildings property_tile 0 with Failure _ -> () 
    end
  | _ -> ()

let move_circle pos rstate = failwith "TODO"

let move_diamond pos rstate = failwith "TODO"

let move_square pos rstate = failwith "TODO"

let move_triangle pos rstate = failwith "TODO"