open Graphics
open Property

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

let mouse_within_tile mouse_x mouse_y =
  let rec mouse_within_tile_helper mouse_x mouse_y acc = function 
    | [] -> None
    | (x, y) :: t ->
      if (mouse_x >= x && mouse_x <= x + 100 
          && mouse_y >= y && mouse_y <= y + 100) 
      then Some acc
      else mouse_within_tile_helper mouse_x mouse_y (acc + 1) t
  in 
  mouse_within_tile_helper mouse_x mouse_y 0 coords

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

let draw_building coord scale color = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_poly [|(x, y); (x + 2 * scale, y); (x + 2 * scale, y + scale);
              (x + scale, y + 2 * scale); (x, y + scale)|]

let rec draw_buildings num_left coord color = 
  if num_left = 0 then ()
  else 
    let x' = fst coord + padding + padding / 5 in 
    let y' = snd coord in 
    draw_building coord 5 color;
    draw_buildings (num_left - 1) (x', y') color

let building_color prop = match Property.get_color prop with 
  | Dark_Blue | Brown -> white 
  | _ -> black

let draw_tile_property coord prop = 
  let x = fst coord in 
  let y = snd coord in 
  let color = prop |> Property.get_color |> convert_color in 
  set_color go_green; fill_rect (fst coord) (snd coord) sq_dim sq_dim;
  fill_header color coord;
  draw_buildings (Property.get_num_buildings prop) 
    (x + padding, y + sq_dim - header_height + 2 * padding / 3) 
    (building_color prop);
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
  fill_poly [|(x + 13 * scale, y + 6 * scale); (x + 13 * scale, y + 8 * scale);
              (x + 15 * scale, y + 8 * scale); (x + 21 * scale, y + 2 * scale);
              (x + 21 * scale, y); (x + 19 * scale, y);|];
  fill_poly [|(x + 15 * scale, y); (x + 13 * scale, y);
              (x + 13 * scale, y + 2 * scale); (x + 19 * scale, y + 8 * scale);
              (x + 21 * scale, y + 8 * scale); (x + 21 * scale, y + 6 * scale)|]

let draw_tile_tax coord prop = 
  let x = fst coord in 
  let y = snd coord in
  generic_light_green_tile coord prop;
  draw_tax (x + 2 * padding - 1, y + sq_dim / 2 - 9) 3 gray;
  draw_tax (x + 2 * padding, y + sq_dim / 2 - 8) 3 black;
  draw_string_in_tile Center coord ("Pay $200") (sq_dim / 7) 8

let square_marker_centered coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_poly [|(x - scale, y - scale); (x - scale, y + scale); 
              (x + scale, y + scale); (x + scale, y - scale)|];
  set_color black;
  set_line_width 3;
  draw_poly [|(x - scale, y - scale); (x - scale, y + scale); 
              (x + scale, y + scale); (x + scale, y - scale)|];
  set_line_width 1

let circle_marker_centered coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_circle x y scale;
  set_color black;
  set_line_width 3;
  draw_circle x y scale;
  set_line_width 1

let diamond_marker_centered coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_poly [|(x, y - scale); (x - scale, y); (x, y + scale); (x + scale, y)|];
  set_color black;
  set_line_width 3;
  draw_poly [|(x, y - scale); (x - scale, y); (x, y + scale); (x + scale, y)|];
  set_line_width 1

let triangle_marker_centered coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_poly [|(x - scale, y - scale); (x, y + scale); (x + scale, y - scale)|];
  set_color black;
  set_line_width 3;
  draw_poly [|(x - scale, y - scale); (x, y + scale); (x + scale, y - scale)|];
  set_line_width 1

let render_player_pos_helper player coord = 
  match player with 
  | "Circle" -> circle_marker_centered coord cyan 10
  | "Square" -> square_marker_centered coord cyan 10
  | "Triangle" -> triangle_marker_centered coord magenta 10
  | "Diamond" -> diamond_marker_centered coord magenta 10
  | _ -> failwith "Somehow got an incorrect player marker type."

let rec render_player_positions rstate posns offset = 
  match posns with 
  | [] -> () 
  | (player, pos) :: t -> 
    let board = Render_state.get_board rstate in 
    begin 
      match pos with 
      | None -> render_player_positions rstate t offset
      | Some pos ->
        let curr_tile_type = Newboard.get_type board pos in 
        let tile_coords = Stdlib.List.nth coords pos in
        (begin 
          match curr_tile_type with 
          | In_jail_just_visiting -> 
            let x' = fst tile_coords + offset in 
            let y' = snd tile_coords + sq_dim - header_height in 
            render_player_pos_helper player (x', y') (* put player in the header *)
          | _ ->       
            let x'' = fst tile_coords + sq_dim / 5 + offset in 
            let y'' = snd tile_coords + sq_dim / 2 in 
            render_player_pos_helper player (x'', y'') (* put player in center *)
        end); render_player_positions rstate t (offset + 2 * padding) 
    end

let render_player_positions_from_state rstate = 
  render_player_positions rstate (Render_state.lst_posns rstate) 0

let draw_lightning coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_poly [|(x, y); (x + 3 * scale, y + 6 * scale); 
              (x + scale, y + 6 * scale); (x + 3 * scale, y + 9 * scale);
              (x + 7 * scale, y + 9 * scale); (x + 4 * scale, y + 7 * scale);
              (x + 7 * scale, y + 7 * scale);|];
  set_color black;
  draw_poly [|(x, y); (x + 3 * scale, y + 6 * scale); 
              (x + scale, y + 6 * scale); (x + 3 * scale, y + 9 * scale);
              (x + 7 * scale, y + 9 * scale); (x + 4 * scale, y + 7 * scale);
              (x + 7 * scale, y + 7 * scale);|]

let draw_tile_utility coord prop = 
  let x = fst coord in 
  let y = snd coord in 
  generic_light_green_tile coord prop; 
  draw_string_in_tile Center coord ("Utilities") 
    (sq_dim - sq_dim / 5) 15;
  draw_lightning (x + sq_dim / 3 - 2, y + sq_dim / 3 - 2) black 4;
  draw_lightning (x + sq_dim / 3, y + sq_dim / 3) yellow 4;
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

let draw_car coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  (* Draw body of car *)
  set_color color;
  fill_poly [|(x, y); (x, y + 3 * scale); (x + 2 * scale, y + 3 * scale); 
              (x + 5 * scale, y + 6 * scale); (x + 9 * scale, y + 6 * scale);
              (x + 12 * scale, y + 3 * scale); (x + 15 * scale, y + 3 * scale);
              (x + 15 * scale, y)|];
  set_color black;
  draw_poly [|(x, y); (x, y + 3 * scale); (x + 2 * scale, y + 3 * scale); 
              (x + 5 * scale, y + 6 * scale); (x + 9 * scale, y + 6 * scale);
              (x + 12 * scale, y + 3 * scale); (x + 15 * scale, y + 3 * scale);
              (x + 15 * scale, y)|];
  (* Tires *)
  set_color gray;
  fill_circle (x + 4 * scale) y scale; fill_circle (x + 10 * scale) y scale;
  set_color black;
  draw_circle (x + 4 * scale) y scale; draw_circle (x + 10 * scale) y scale;
  (* Windows and lights *)
  set_color red;
  fill_rect x (y + scale) (2 * scale) scale; 
  set_color yellow;
  fill_rect (x + 14 * scale) (y + scale) scale scale;
  set_color light_blue;
  fill_poly [|(x + 3 * scale, y + 3 * scale); (x + 5 * scale, y + 5 * scale);
              (x + 7 * scale, y + 5 * scale); (x + 7 * scale, y + 3 * scale)|];
  fill_poly [|(x + 8 * scale, y + 3 * scale); (x + 8 * scale, y + 5 * scale);
              (x + 9 * scale, y + 5 * scale); (x + 11 * scale, y + 3 * scale)|];
  set_color black;
  draw_rect x (y + scale) (2 * scale) scale; 
  draw_rect (x + 14 * scale) (y + scale) scale scale;
  draw_poly [|(x + 3 * scale, y + 3 * scale); (x + 5 * scale, y + 5 * scale);
              (x + 7 * scale, y + 5 * scale); (x + 7 * scale, y + 3 * scale)|];
  draw_poly [|(x + 8 * scale, y + 3 * scale); (x + 8 * scale, y + 5 * scale);
              (x + 9 * scale, y + 5 * scale); (x + 11 * scale, y + 3 * scale)|]

let draw_tile_free_parking coord prop = 
  let x = fst coord in 
  let y = snd coord in
  generic_light_green_tile coord prop;
  draw_string_in_tile Center coord ("FREE PARKING") 
    (sq_dim - sq_dim / 5) 15;
  draw_car (x + sq_dim / 5, y + sq_dim / 3) orange 4 

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

let draw_arrow coord color scale = 
  let x = fst coord in 
  let y = snd coord in 
  set_color color;
  fill_poly [|(x, y); (x, y + 2 * scale); (x + 4 * scale, y + 2 * scale); 
              (x + 4 * scale, y + 3 * scale); (x + 6 * scale, y + scale); 
              (x + 4 * scale, y - scale); (x + 4 * scale, y)|];
  set_color black;
  draw_poly [|(x, y); (x, y + 2 * scale); (x + 4 * scale, y + 2 * scale); 
              (x + 4 * scale, y + 3 * scale); (x + 6 * scale, y + scale); 
              (x + 4 * scale, y - scale); (x + 4 * scale, y)|]

let draw_tile_go_to_jail coord prop = 
  let x = fst coord in 
  let y = snd coord in
  generic_light_green_tile coord prop;
  draw_string_in_tile Center coord ("GO TO JAIL") (sq_dim - sq_dim / 5) 15;
  draw_arrow (x + sq_dim / 3 + sq_dim / 15, y + 3 * sq_dim / 5) red 4;
  draw_jail_cell (x + padding, y + padding) 2

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

let draw_letter_box_offset x y color scale = 
  set_color black;
  fill_rect (x - 5 * scale - 3) (y - 5 * scale - 3) (25 * scale) (35 * scale);
  set_color color; 
  fill_rect (x - 5 * scale) (y - 5 * scale) (25 * scale) (35 * scale)

let alph_M x y color scale = 
  set_color color;
  fill_rect x y (3 * scale) (25 * scale);
  fill_poly [|(x, y + 20 * scale); 
              (x + 3 * scale, y + 25 * scale); 
              (x + 9 * scale, y + 15 * scale); 
              (x + 6 * scale, y + 10 * scale)|];
  fill_poly [|(x + 6 * scale, y + 10 * scale); 
              (x + 9 * scale, y + 10 * scale);
              (x + 15 * scale, y + 20 * scale);
              (x + 12 * scale, y + 25 * scale)|];
  fill_rect (x + 12 * scale) y (3 * scale) (25 * scale)

let draw_letter x y color scale alph_func = 
  draw_letter_box_offset x y color scale;
  alph_func (x - 2) (y - 2) black scale;
  alph_func x y white scale

let alph_O x y color scale = 
  set_color color;
  fill_rect x y (15 * scale) (3 * scale);
  fill_rect x y (3 * scale) (25 * scale);
  fill_rect x (y + 22 * scale) (15 * scale) (3 * scale);
  fill_rect (x + 12 * scale) y (3 * scale) (25 * scale)

let alph_N x y color scale = 
  set_color color; 
  fill_rect x y (3 * scale) (25* scale);
  fill_poly [|(x, y + 20 * scale); 
              (x + 3 * scale, y + 25 * scale); 
              (x + 15 * scale, y + 5 * scale); 
              (x + 12 * scale, y)|];
  fill_rect (x + 12 * scale) y (3 * scale) (25 * scale)

let alph_P x y color scale = 
  set_color color; 
  fill_rect x (y + 10 * scale) (15 * scale) (3 * scale);
  fill_rect x y (3 * scale) (25 * scale);
  fill_rect x (y + 22 * scale) (15 * scale) (3 * scale);
  fill_rect (x + 12 * scale) (y + 10 * scale) (3 * scale) (15 * scale)

let alph_L x y color scale = 
  set_color color; 
  fill_rect x y (15 * scale) (3 * scale);
  fill_rect x y (3 * scale) (25 * scale)

let alph_Y x y color scale = 
  set_color color; 
  fill_poly [|(x, (y + 25 * scale)); ((x + 4 * scale), (y + 25 * scale)); 
              ((x + 10 * scale), (y + 13 * scale)); 
              ((x + 6 * scale), (y + 13 * scale))|];
  fill_poly [|((x + 5 * scale), (y + 13 * scale));
              ((x + 9 * scale), (y + 13 * scale)); 
              ((x + 15 * scale), (y + 25 * scale));
              ((x + 11 * scale), (y + 25 * scale))|];
  fill_rect (x + 6 * scale) y (3 * scale) (13 * scale) 

let draw_monopoly_centerpiece ctx cty = 
  draw_letter (ctx + 165) (cty + 100) brown 2 alph_Y;
  draw_letter (ctx + 115) (cty + 100) dark_blue 2 alph_L;
  draw_letter (ctx + 65) (cty + 100) light_blue 2 alph_O; 
  draw_letter (ctx + 15) (cty + 100) green 2 alph_P;
  draw_letter (ctx - 35) (cty + 100) dark_go_green 2 alph_O;
  draw_letter (ctx - 85) (cty + 100) dark_yellow 2 alph_N;
  draw_letter (ctx - 135) (cty + 100) orange 2 alph_O;
  draw_letter (ctx - 185) (cty + 100) red 2 alph_M

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

let camel_arr = 
  [|(351, 356); (373, 372); (382, 407); (385, 438); (377, 445); 
    (380, 452); (388, 451); (392, 447); (412, 457); (427, 459); (438, 455); 
    (457, 457); (473, 451); (478, 439); (478, 430); (460, 422); (440, 403); 
    (426, 360); (407, 318); (374, 288); (340, 277); (319, 273); (307, 248); 
    (301, 233); (296, 215); (291, 198); (293, 188); (288, 166); (288, 139); 
    (287, 120); (295, 109); (299, 98); (316, 87); (320, 76); (290, 71); 
    (275, 92); (269, 107); (271, 138); (270, 159); (263, 171); (264, 186); 
    (261, 199); (257, 186); (250, 171); (243, 148); (234, 135); (235, 113); 
    (242, 105); (252, 102); (248, 94); (231, 90); (214, 93); (208, 103); 
    (211, 113); (209, 120); (222, 155); (231, 186); (235, 200); (243, 223); 
    (248, 242); (222, 240); (179, 253); (164, 259); (151, 260); (149, 243);
    (139, 221); (123, 199); (124, 167); (134, 120); (151, 108); (163, 99); 
    (157, 90); (125, 90); (120, 105); (115, 114); (117, 124); (94, 177); 
    (101, 199); (102, 214); (101, 229); (97, 234); (91, 218); (62, 183); 
    (55, 176); (59, 165); (50, 138); (50, 110); (60, 90); (69, 85); (69, 77); 
    (37, 72); (30, 82); (28, 90); (32, 100); (30, 105); (33, 116); (32, 143); 
    (33, 182); (46, 201); (54, 220); (58, 244); (56, 267); (62, 291); 
    (68, 305); (69, 310); (67, 312); (62, 307); (54, 291); (48, 283); 
    (36, 274); (27, 274); (30, 288); (41, 302); (53, 315); (69, 327); 
    (82, 346); (99, 362); (118, 377); (126, 382); (134, 391); (147, 399); 
    (158, 413); (171, 420); (197, 431); (215, 430); (237, 428); (252, 417); 
    (262, 405); (265, 399); (281, 399); (296, 391); (315, 386); (328, 377); 
    (343, 367)|]

let transform_camel scale trans_x trans_y = Stdlib.Array.map 
    (fun (x, y) -> 
       (int_of_float (float_of_int x *. scale +. trans_x), 
        int_of_float (float_of_int y *. scale +. trans_y))) camel_arr

let fill_camel color trans_x trans_y scale = 
  set_color color; 
  let camel_arr_transformed = transform_camel scale trans_x trans_y in
  fill_poly camel_arr_transformed

let draw_camel color trans_x trans_y scale = 
  set_color color; 
  set_line_width 3;
  let camel_arr_transformed = transform_camel scale trans_x trans_y in
  draw_poly camel_arr_transformed;
  set_line_width 1

let draw_camel_centerpiece () = 
  fill_camel red 274. 184. 0.50; 
  fill_camel orange 274.75 184.75 0.50; 
  fill_camel dark_yellow 275.5 185.5 0.50; 
  fill_camel go_green 276.25 186.25 0.50; 
  fill_camel green 277. 187. 0.50; 
  fill_camel light_blue 277.75 187.75 0.50; 
  fill_camel dark_blue 278.5 188.5 0.50; 
  fill_camel brown 279.25 189.25 0.50; 
  fill_camel white 280. 190. 0.50;
  draw_camel black 280. 190. 0.50

let draw_board board = 
  let rec loop board coords = match board, coords with 
    | [], [] -> () 
    | h_board :: t_board, h_coords :: t_coords -> 
      draw_tile h_coords h_board; draw_tile_outline h_coords; 
      loop t_board t_coords 
    | _, _ -> failwith "Board size mismatch"
  in loop board coords

let draw_state rstate = 
  let board = Render_state.get_board rstate in 
  draw_board board;
  render_player_positions_from_state rstate

let process_json json = Newboard.from_json json

let board = "board.json"|> Yojson.Basic.from_file |> process_json 

let handle_first_click rstate func_call = 
  let status' = wait_next_event [Button_down] in 
  let m_x = status'.mouse_x in 
  let m_y = status'.mouse_y in 
  let pos_opt = mouse_within_tile m_x m_y in 
  match pos_opt with 
  | None -> rstate
  | Some pos -> 
    func_call pos rstate

let detect_click rstate =
  let status = wait_next_event [Button_down] in
  if status.mouse_x >= 850 && status.mouse_x <= 950 && status.mouse_y >= 600
     && status.mouse_y <= 640 then 
    handle_first_click rstate Render_state.add_building_at_pos
  else if status.mouse_x >= 850 && status.mouse_x <= 950 
          && status.mouse_y >= 550 && status.mouse_y <= 590 then 
    handle_first_click rstate Render_state.reset_buildings_at_pos
  else if status.mouse_x >= 850 && status.mouse_x <= 950 
          && status.mouse_y >= 500 && status.mouse_y <= 540 then 
    handle_first_click rstate Render_state.move_circle
  else if status.mouse_x >= 850 && status.mouse_x <= 950 
          && status.mouse_y >= 450 && status.mouse_y <= 490 then 
    handle_first_click rstate Render_state.move_diamond
  else if status.mouse_x >= 850 && status.mouse_x <= 950 
          && status.mouse_y >= 400 && status.mouse_y <= 440 then 
    handle_first_click rstate Render_state.move_square
  else if status.mouse_x >= 850 && status.mouse_x <= 950 
          && status.mouse_y >= 350 && status.mouse_y <= 390 then 
    handle_first_click rstate Render_state.move_triangle
  else rstate

let button coord name = 
  let x = fst coord in 
  let y = snd coord in
  set_color go_green;
  fill_rect x y 100 40;
  set_color black;
  draw_rect x y 100 40;
  draw_string_in_tile Center (x, y) name 15 20

let render () = 
  open_graph ""; 
  set_window_title "Monopoly"; 
  resize_window 1000 801;
  auto_synchronize true;
  button (850, 600) "Add Building";
  button (850, 550) "Reset Buildings";
  button (850, 500) "Move Circle";
  button (850, 450) "Move Diamond";
  button (850, 400) "Move Square";
  button (850, 350) "Move Triangle";
  draw_monopoly_centerpiece 400 400; 
  draw_camel_centerpiece ()

let rec render_loop st = 
  draw_state st;
  let st' = detect_click st in 
  draw_state st'; 
  render_loop st'

let () = 
  try 
    render (); 
    let initial_state = Render_state.init_state board in 
    draw_state initial_state;
    render_loop (detect_click initial_state)
  with Graphic_failure _ -> print_endline "Exiting... Thanks for playing!"

