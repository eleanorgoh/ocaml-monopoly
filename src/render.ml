open Graphics
open Property

let init_window = 
  open_graph ""; 
  set_window_title "Monopoly"; 
  resize_window 1000 800

(* Preset colors *)
let brown = rgb 139 69 19 
let light_blue = rgb 0 191 255 
let pink = rgb 255 105 180 
let orange = rgb 255 140 0 
let red = rgb 220 20 60 
let yellow = rgb 255 255 153 
let green = rgb 60 179 113
let dark_blue = rgb 0 0 139
let go_green = rgb 207 255 229

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
let prop_5 = init_property 0 "Mac's" Brown Property 0 1 2 3 4 5 1 9999 0
let prop_6 = init_property 0 "Terrace" Pink Property 1 1 1 1 1 1 1 9999 0
let prop_7 = init_property 0 "Sage" Brown Property 1 1 1 1 1 1 1 10 0 
let prop_8 = init_property 
    0 "Expensive Rent" Green Property 9999 9999 9999 9999 9999 9999 9999 10 0
let prop_9 = init_property 0 "Olin" Pink Property 1 1 1 1 1 1 1 10 0
let prop_10 = init_property 0 "Libe" Orange Property 0 0 0 1 1 1 0 0 0 

let go_prop = init_property 1 "GO" Light_Blue Go 0 1 2 3 4 5 10 20 0

let prop_lst_28 = [
  go_prop; prop_0; prop_1; prop_2; prop_3; prop_4; prop_5; 
  prop_6; prop_7; prop_8; prop_9; prop_10; prop_0; prop_1; 
  prop_2; prop_3; prop_4; prop_5; prop_6; prop_7; prop_8; 
  prop_9; prop_10; prop_0; prop_1; prop_2; prop_3; prop_4
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
  | (x, y) -> set_color black; draw_rect x y sq_dim sq_dim; draw_rect x 
      (y+sq_dim - header_height) sq_dim header_height

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

let draw_tile_go coord prop = 
  generic_light_green_tile coord prop;
  draw_string_in_tile Center coord "GO" (sq_dim / 2) 45;
  draw_string_in_tile Center coord "Collect $200" (sq_dim / 7) 8

let draw_train coord scale = 
  let x = fst coord in 
  let y = snd coord in 
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

let anim_player_pos player = failwith "TODO"
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

let draw_buildings prop = failwith "TODO"

let draw_tile coord prop = match Property.get_type prop with 
  | Property -> draw_tile_property coord prop
  | Go -> draw_tile_go coord prop
  | Railroad -> draw_tile_railroad coord prop
  | Utility -> failwith "TODO"
  | Tax -> failwith "TODO"
  | Chance_card -> failwith "TODO"
  | Community_chest -> failwith "TODO"
  | Free_parking -> failwith "TODO"
  | Go_to_jail -> failwith "TODO"
  | In_jail_just_visiting -> failwith "TODO"

let draw_board board = 
  let rec loop board coords = match board, coords with 
    | [], [] -> () 
    | h_board :: t_board, h_coords :: t_coords -> 
      draw_tile_outline h_coords; draw_tile h_coords h_board; 
      loop t_board t_coords 
    | _, _ -> failwith "Board size mismatch"
  in loop board coords

let () = init_window; 
  try draw_board prop_lst_28;
    let rec loop x y  = 
      let _ = wait_next_event [Poll] and wx' = size_x () and wy' = size_y ()
      in loop wx' wy'
    in 
    loop 0 0 
  with Graphic_failure _ -> print_endline "Exiting... Thanks for playing!"

