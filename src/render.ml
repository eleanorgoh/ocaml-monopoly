open Graphics
open Property

let init_window = 
  open_graph ""; 
  set_window_title "Monopoly"; 
  resize_window 1000 801

(* Preset colors *)
let brown = rgb 139 69 19 
let light_blue = rgb 0 191 255 
let pink = rgb 255 105 180 
let orange = rgb 255 140 0 
let red = rgb 220 20 60 
let yellow = rgb 255 255 153 
let green = rgb 60 179 113
let dark_blue = rgb 0 0 139
let go_green = rgb 214 255 236
let gray = rgb 120 120 120
let dark_yellow = rgb 250 218 94
let dark_go_green = rgb 180 230 202

(* Other constants *)
let header_height = 25
let sq_dim = 100 
let padding = 10

(** Coordinates for the tiles of a 28-tile board. *)
let coords = [
  (0, 0); (0, 100); (0, 200); (0, 300); (0, 400); (0, 500); (0, 600); (0, 700);
  (100, 700); (200, 700); (300, 700); (400, 700); (500, 700); (600, 700);
  (700, 700); (700, 600); (700, 500); (700, 400); (700, 300); (700, 200);
  (700, 100); (700, 0); (600, 0); (500, 0); (400, 0); (300, 0); (200, 0); 
  (100, 0)
]

let prop_0 = init_property 1 "PSB" Light_Blue Property 0 1 2 3 4 5 10 20 0
let prop_1 = init_property 0 "PSB" Green Property 0 1 2 3 4 5 1 1 0
let prop_2 = init_property 0 "Home" Yellow Property 0 1 2 3 4 5 1 1 0
let prop_3 = init_property 0 "Slope" Dark_Blue Property 0 1 2 3 4 5 1 1 0
let prop_4 = init_property 0 "Line 92" Red Railroad 0 1 2 3 4 5 0 1 0
let prop_5 = init_property 0 "Mac's" Red Property 0 1 2 3 4 5 1 9999 0
let prop_6 = init_property 0 "Terrace" Pink Community_chest 1 1 1 1 1 1 1 9999 0
let prop_7 = init_property 0 "Sage" Brown Go_to_jail 1 1 1 1 1 1 1 10 0 
let prop_8 = init_property 
    0 "Expensive Rent" Green Property 9999 9999 9999 9999 9999 9999 9999 10 0
let prop_9 = init_property 0 "Olin" Pink Chance_card 1 1 1 1 1 1 1 10 0
let prop_10 = init_property 0 "Libe" Orange Property 0 0 0 1 1 1 0 0 0 

let go_prop = init_property 1 "GO" Light_Blue Go 0 1 2 3 4 5 10 20 0
let tax_prop = init_property 1 "GO" Light_Blue Tax 0 1 2 3 4 5 10 20 0
let in_jail_prop = init_property 1 "GO" Light_Blue In_jail_just_visiting 0 1 2 3 4 5 10 20 0

let prop_lst_28 = [
  go_prop; prop_0; prop_1; prop_2; prop_3; prop_4; prop_5; 
  prop_6; prop_7; prop_8; prop_9; prop_10; prop_0; prop_1; 
  in_jail_prop; prop_3; prop_4; prop_5; tax_prop; prop_7; prop_8; 
  prop_9; prop_10; prop_4; prop_1; prop_2; prop_3; prop_5
]

let convert_color = function 
  | Brown -> brown
  | Light_Blue -> light_blue
  | Pink -> pink
  | Orange -> orange
  | Red -> red
  | Yellow -> yellow
  | Green -> green
  | Dark_Blue -> dark_blue
  | No_Color -> background

let draw_tile_outline = function
  | (0, 0) | (0, 700) | (700, 700) | (700, 0) as tup -> 
    set_color black; draw_rect (fst tup) (snd tup) sq_dim sq_dim
  | (x, y) -> set_color black; draw_rect x y sq_dim sq_dim

let fill_header c coord = 
  let fill_header c = 
    let x1 = fst coord in 
    let y1 = snd coord + sq_dim - header_height in 
    set_color c; fill_rect x1 y1 sq_dim header_height; 
    set_color black; draw_rect x1 y1 sq_dim header_height 
  in fill_header c

type pos =  Left | Center | Right

let draw_string_in_tile pos coord str y_height size = 
  let (w, h) = text_size str in 
  let ty = snd coord + y_height in 
  (match pos with 
   | Center -> Graphics.moveto (fst coord + (sq_dim - w)/2) ty 
   | Right -> let tx = fst coord + sq_dim - w - padding in 
     Graphics.moveto tx ty 
   | Left -> let tx = fst coord + padding in Graphics.moveto tx ty);
  set_color black;
  set_text_size size;
  draw_string str

let draw_tile_property coord prop = 
  let color = prop |> Property.get_color |> convert_color in 
  set_color go_green; fill_rect (fst coord) (snd coord) sq_dim sq_dim;
  fill_header color coord;
  let prop_name = Property.get_name prop in 
  let price = prop |> Property.get_price |> string_of_int in 
  draw_string_in_tile Center coord prop_name (sq_dim / 2) 15;
  draw_string_in_tile Center coord ("Price: $"^price) (sq_dim / 7) 8

let generic_light_green_tile coord prop = 
  set_color go_green;
  fill_rect (fst coord) (snd coord) sq_dim sq_dim; 
  set_color black;
  draw_rect (fst coord) (snd coord) sq_dim sq_dim

let draw_go coord scale = 
  let x = fst coord in 
  let y = snd coord in 
  let x' = x + 2 in 
  let y' = y + 2 in
  let helper color x y = 
    set_color color; 
    fill_rect x y (scale*7) (scale*2);
    fill_rect x (y+scale*2) (scale*2) (scale*5); 
    fill_rect x (y+scale*7) (scale*7) (scale*2); 
    fill_rect (x+scale*5) (y+scale*2) (scale*2) (scale*1); 
    fill_rect (x+scale*3) (y+scale*3) (scale*4) (scale*2); 
    fill_rect (x+scale*9) y (scale*7) (scale*2);
    fill_rect (x+scale*9) (y+scale*7) (scale*7) (scale*2);
    fill_rect (x+scale*9) (y+scale*2) (scale*2) (scale*5);
    fill_rect (x+scale*14) (y+scale*2) (scale*2) (scale*5)
  in 
  helper black x y; helper red x' y'

let draw_tile_go coord prop = 
  let x = fst coord in 
  let y = snd coord in
  generic_light_green_tile coord prop;
  draw_go (x + sq_dim / 2 - 3 * 8, y + sq_dim / 2 - 3 * 4) 3;
  draw_string_in_tile Center coord "Collect $200" (sq_dim / 7) 8

let draw_train coord scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color black;
  fill_poly [|(x, y); (x+scale*3, y); (x+scale*3, y+scale*6)|];
  fill_poly [|(x+scale*2, y+scale*14); (x+scale*6, y+scale*14); 
              (x+scale*4, y+scale*8)|];
  fill_rect (x+scale*10) (y+scale*10) (scale*4) (scale*4);
  fill_rect (x+scale*2) (y+scale*4) (scale*12) (scale*6);
  fill_circle (x+scale*6) (y+scale*2) (scale*2);
  fill_circle (x+scale*12) (y+scale*2) (scale*2);
  fill_ellipse (x+scale*2) (y+scale*7) (scale*2) (scale*3)

let draw_tile_railroad coord prop = 
  let rr_name = Property.get_name prop in 
  let train_coord = (fst coord + sq_dim / 4, 
                     snd coord + sq_dim / 4 + padding / 2) in 
  let price = prop |> Property.get_price |> string_of_int in 
  generic_light_green_tile coord prop;
  draw_train train_coord 3;
  draw_string_in_tile Center coord ("RR: "^rr_name) (sq_dim - sq_dim / 5) 15;
  draw_string_in_tile Center coord ("Price: $"^price) (sq_dim / 7) 8

let draw_tax coord scale color = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color; 
  (* T *)
  fill_rect (x + 2 * scale) y (2 * scale) (7 * scale); 
  fill_rect x (y + 7 * scale) (6 * scale) (2 * scale);
  (* A *)
  fill_rect (x + 7 * scale) y (2 * scale) (5 * scale);
  fill_rect (x + 9 * scale) y scale (scale);
  fill_rect (x + 9 * scale) (y + 4 * scale) scale (scale);
  fill_rect (x + 10 * scale) y (2 * scale) (8 * scale); 
  fill_rect (x + 7 * scale) (y + 6 * scale) (3 * scale) (2 * scale);
  (* X *)
  fill_rect (x + 13 * scale) y (2 * scale) (2 * scale);
  fill_rect (x + 13 * scale) (y + 6 * scale) (2 * scale) (2 * scale);
  fill_rect (x + 14 * scale) (y + scale) (2 * scale) (2 * scale);
  fill_rect (x + 14 * scale) (y + 5 * scale) (2 * scale) (2 * scale);
  fill_rect (x + 16 * scale) (y + 3 * scale) (2 * scale) (2 * scale);
  fill_rect (x + 18 * scale) (y + scale) (2 * scale) (2 * scale);
  fill_rect (x + 18 * scale) (y + 5 * scale) (2 * scale) (2 * scale);
  fill_rect (x + 19 * scale) y (2 * scale) (2 * scale);
  fill_rect (x + 19 * scale) (y + 6 * scale) (2 * scale) (2 * scale)

let draw_tile_tax coord prop = 
  let x = fst coord in 
  let y = snd coord in
  generic_light_green_tile coord prop;
  draw_tax (x + 2 * padding - 1, y + sq_dim / 2 - 9) 3 gray;
  draw_tax (x + 2 * padding, y + sq_dim / 2 - 8) 3 black;
  draw_string_in_tile Center coord ("Pay $200") (sq_dim / 7) 8

let render_player_pos player = failwith "TODO"
(* Example animation

   let move_rect pos size speed n =
   let (x, y) = pos and (sx,sy) = size in
   let mem = ref (Graphics.get_image x y sx sy) in 
   let rec move_aux x y speed n =
    if n = 0 then Graphics.moveto x y
    else 
     let ((nx,ny),n_speed) = calc_pv (x,y) (sx,sy) speed 
     and old_mem = !mem in 
      mem := Graphics.get_image nx ny sx sy;
      Graphics.set_color Graphics.blue;
      Graphics.fill_rect nx ny sx sy;
      Graphics.draw_image (inv_image old_mem) x y;
      move_aux nx ny n_speed (n-1)
   in move_aux x y speed n *)

let draw_tile_utility coord prop = 
  generic_light_green_tile coord prop; 
  draw_string_in_tile Center coord ("Water and Electricity") 
    (sq_dim - sq_dim / 5) 15;
  draw_string_in_tile Center coord ("Price: $150") (sq_dim / 7) 8

let draw_question_mark coord scale color = 
  let x = fst coord in 
  let y = snd coord in
  set_color color; 
  fill_rect x y (2 * scale) (2 * scale);
  fill_rect x (y + 3 * scale) (2 * scale) (2 * scale);
  fill_rect x (y + 5 * scale) (5 * scale) (2 * scale);
  fill_rect (x + 3 * scale) (y + 7 * scale) (2 * scale) (3 * scale);
  fill_rect (x - scale) (y + 10 * scale) (6 * scale) (2 * scale);
  set_color black;
  draw_rect x y (2 * scale) (2 * scale);
  moveto x (y + 3 * scale); lineto x (y + 7 * scale); 
  lineto (x + 3 * scale) (y + 7 * scale); 
  lineto (x + 3 * scale) (y + 10 * scale); lineto (x - scale) (y + 10 * scale);
  lineto (x - scale) (y + 12 * scale); lineto (x + 5 * scale) (y + 12 * scale);
  lineto (x + 5 * scale) (y + 5 * scale); lineto (x + 2 * scale)(y + 5 * scale); 
  lineto (x + 2 * scale) (y + 3 * scale); lineto x (y + 3 * scale)     

let draw_tile_card coord prop = 
  let x' = fst coord + 4 * padding + 3 in 
  let y' = snd coord + 2 * padding - 3 in
  generic_light_green_tile coord prop;
  match Property.get_type prop with 
  | Chance_card -> 
    draw_string_in_tile Center coord ("CHANCE CARD") (sq_dim - sq_dim / 5) 15;
    draw_question_mark (x' - 1, y' - 1) 4 black; 
    draw_question_mark (x', y') 4 red
  | Community_chest -> 
    draw_string_in_tile Center coord ("COMMUNITY CHEST") 
      (sq_dim - sq_dim / 5) 15;
    draw_question_mark (x' - 1, y' - 1) 4 black; 
    draw_question_mark (x', y') 4 light_blue
  | _ -> failwith "Cannot draw: Property is not a Chance or Community Card"

let draw_tile_free_parking coord prop = 
  generic_light_green_tile coord prop;
  draw_string_in_tile Center coord ("FREE PARKING") 
    (sq_dim - sq_dim / 5) 15

let draw_tile_go_to_jail coord prop = 
  generic_light_green_tile coord prop;
  draw_string_in_tile Center coord ("GO TO JAIL") (sq_dim - sq_dim / 5) 15

let draw_jail_cell coord scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color white; 
  set_line_width 2;
  fill_rect x y (scale * 40) (scale * 20);
  set_color black;
  moveto (x + 4 * scale) y; lineto (x + 4 * scale) (y + scale * 20); 
  moveto (x + 8 * scale) y; lineto (x + 8 * scale) (y + scale * 20); 
  moveto (x + 12 * scale) y; lineto (x + 12 * scale) (y + scale * 20); 
  moveto (x + 16 * scale) y; lineto (x + 16 * scale) (y + scale * 20); 
  moveto (x + 20 * scale) y; lineto (x + 20 * scale) (y + scale * 20); 
  moveto (x + 24 * scale) y; lineto (x + 24 * scale) (y + scale * 20); 
  moveto (x + 28 * scale) y; lineto (x + 28 * scale) (y + scale * 20); 
  moveto (x + 32 * scale) y; lineto (x + 32 * scale) (y + scale * 20); 
  moveto (x + 36 * scale) y; lineto (x + 36 * scale) (y + scale * 20);
  draw_rect x y (scale * 40) (scale * 20);
  set_line_width 1

let draw_jail coord prop = 
  let x = fst coord in 
  let y = snd coord in
  generic_light_green_tile coord prop;
  set_color red;
  fill_rect x y sq_dim (sq_dim - header_height);
  set_color black;
  draw_tile_outline (x, y);
  draw_rect x (y + sq_dim - header_height) sq_dim header_height;
  draw_jail_cell (x + padding, y + padding) 2;
  draw_string_in_tile Center coord ("IN JAIL") (14 * sq_dim / 25) 15

let draw_buildings prop = failwith "TODO"

let draw_tile coord prop = match Property.get_type prop with 
  | Property -> draw_tile_property coord prop
  | Go -> draw_tile_go coord prop
  | Railroad -> draw_tile_railroad coord prop
  | Utility -> draw_tile_utility coord prop 
  | Tax -> draw_tile_tax coord prop 
  | Chance_card -> draw_tile_card coord prop
  | Community_chest -> draw_tile_card coord prop
  | Free_parking -> draw_tile_free_parking coord prop
  | Go_to_jail -> draw_tile_go_to_jail coord prop
  | In_jail_just_visiting -> draw_jail coord prop

let draw_board board = 
  let rec loop board coords = match board, coords with 
    | [], [] -> () 
    | h_board :: t_board, h_coords :: t_coords -> 
      draw_tile h_coords h_board; draw_tile_outline h_coords; 
      loop t_board t_coords 
    | _, _ -> failwith "Board size mismatch"
  in loop board coords

let draw_M x y color scale = 
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale);
  set_color white;
  fill_rect x y (3 * scale) (25 * scale);
  fill_rect (x + 2 * scale) (y + 23 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 3 * scale) (y + 22 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 4 * scale) (y + 20 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 4 * scale) (y + 18 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 6 * scale) (y + 10 * scale) (3 * scale) (8 * scale);
  fill_rect (x + 9 * scale) (y + 18 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 9 * scale) (y + 20 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 10 * scale) (y + 22 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 11 * scale) (y + 23 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 12 * scale) y (3 * scale) (25 * scale)

let draw_O x y color scale = 
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale);
  set_color white;
  fill_rect x y (15 * scale) (3 * scale);
  fill_rect x y (3 * scale) (25 * scale);
  fill_rect x (y + 22 * scale) (15 * scale) (3 * scale);
  fill_rect (x + 12 * scale) y (3 * scale) (25 * scale)

let draw_N x y color scale = 
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale);
  set_color white;
  fill_rect x y (3 * scale) (25* scale);
  fill_rect (x + 2 * scale) (y + 23 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 3 * scale) (y + 22 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 4 * scale) (y + 20 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 4 * scale) (y + 18 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 5 * scale) (y + 16 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 7 * scale) (y + 7 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 6 * scale) (y + 8 * scale) (3 * scale) (8 * scale);
  fill_rect (x + 8 * scale) (y + 5 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 8 * scale) (y + 3 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 9 * scale) (y + 1 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 11 * scale) y (3 * scale) (2 * scale); 
  fill_rect (x + 12 * scale) y (3 * scale) (25 * scale)

let draw_P x y color scale = 
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale);
  set_color white;
  fill_rect x (y + 10 * scale) (15 * scale) (3 * scale);
  fill_rect x y (3 * scale) (25 * scale);
  fill_rect x (y + 22 * scale) (15 * scale) (3 * scale);
  fill_rect (x + 12 * scale) (y + 10 * scale) (3 * scale) (15 * scale)

let draw_L x y color scale = 
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale);
  set_color white;
  fill_rect x y (15 * scale) (3 * scale);
  fill_rect x y (3 * scale) (25 * scale)

let draw_Y x y color scale = 
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale);
  set_color white;
  fill_rect x (y + 23 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 12 * scale) (y + 23 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 1 * scale) (y + 20 * scale) (3 * scale) (4 * scale); 
  fill_rect (x + 11 * scale) (y + 20 * scale) (3 * scale) (4 * scale);
  fill_rect (x + 2 * scale) (y + 16 * scale) (3 * scale) (4 * scale); 
  fill_rect (x + 10 * scale) (y + 16 * scale) (3 * scale) (4 * scale);
  fill_rect (x + 3 * scale) (y + 13 * scale) (3 * scale) (4 * scale); 
  fill_rect (x + 9 * scale) (y + 13 * scale) (3 * scale) (4 * scale);
  fill_rect (x + 4 * scale) (y + 12 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 8 * scale) (y + 12 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 5 * scale) (y + 11 * scale) (3 * scale) (2 * scale); 
  fill_rect (x + 7 * scale) (y + 11 * scale) (3 * scale) (2 * scale);
  fill_rect (x + 6 * scale) y (3 * scale) (13 * scale)

let draw_centerpiece ctx cty = 
  draw_M (ctx - 185) (cty + 100) red 2; 
  draw_O (ctx - 135) (cty + 100) orange 2;
  draw_N (ctx - 85) (cty + 100) dark_yellow 2;
  draw_O (ctx - 35) (cty + 100) dark_go_green 2;
  draw_P (ctx + 15) (cty + 100) green 2;
  draw_O (ctx + 65) (cty + 100) light_blue 2; 
  draw_L (ctx + 115) (cty + 100) dark_blue 2;
  draw_Y (ctx + 165) (cty + 100) brown 2

let () = init_window; 
  try draw_board prop_lst_28; draw_centerpiece 400 400;
    let rec loop x y  = 
      let _ = wait_next_event [Poll] and wx' = size_x () and wy' = size_y ()
      in loop wx' wy'
    in 
    loop 0 0 
  with Graphic_failure _ -> print_endline "Exiting... Thanks for playing!"

