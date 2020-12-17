type rstate = {
  circle_pos : int option;
  circle_jail : bool;
  diamond_pos : int option;
  diamond_jail : bool;
  square_pos : int option;
  square_jail : bool;
  triangle_pos : int option;
  triangle_jail : bool;
  board : Newboard.t;
}

let init_state board = {
  circle_pos = None;
  circle_jail = false;
  diamond_pos = None;
  diamond_jail = false;
  square_pos = None;
  square_jail = false;
  triangle_pos = None;
  triangle_jail = false;
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

let change_circle_jail rstate b = {rstate with circle_jail = b}
let change_diamond_jail rstate b = {rstate with diamond_jail = b}
let change_square_jail rstate b = {rstate with square_jail = b}
let change_triangle_jail rstate b = {rstate with triangle_jail = b}

let lst_posns rstate = 
  [("Circle", rstate.circle_pos); ("Diamond", rstate.diamond_pos);
   ("Square", rstate.square_pos); ("Triangle", rstate.triangle_pos)]