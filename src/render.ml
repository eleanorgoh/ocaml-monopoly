open Graphics
open Property
open Newboard

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

(** Coordinates for the tiles of a 28-tile board. *)
let coords = [
  (0, 0);
  (0, 100);
  (0, 200);
  (0, 300);
  (0, 400);
  (0, 500);
  (0, 600);
  (0, 700);
  (100, 700);
  (200, 700);
  (300, 700);
  (400, 700);
  (500, 700);
  (600, 700);
  (700, 700);
  (700, 600);
  (700, 500);
  (700, 400);
  (700, 300);
  (700, 200);
  (700, 100);
  (700, 0);
  (600, 0);
  (500, 0);
  (400, 0);
  (300, 0);
  (200, 0);
  (100, 0)
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
    set_color black; draw_rect (fst tup) (snd tup) 100 100
  | (x, y) -> set_color black; draw_rect x y 100 100; draw_rect x (y+70) 100 30

let fill_header c coord = 
  let fill_header c = 
    let x1 = fst coord in 
    let y1 = snd coord + 70 in 
    set_color c; fill_rect x1 y1 100 30; 
    set_color black; draw_rect x1 y1 100 30 
  in fill_header c

type pos =  Left | Center | Right

let draw_string_in_tile pos coord str y_height = 
  let (w, h) = text_size str in 
  let ty = snd coord + y_height in 
  ( match pos with 
    | Center -> Graphics.moveto (fst coord + (100 - w)/2) ty 
    | Right -> let tx = fst coord + 100 - w - 10 in 
      Graphics.moveto tx ty 
    | Left -> let tx = fst coord + 10 in Graphics.moveto tx ty);
  Graphics.set_color black;
  Graphics.draw_string str

let draw_tile_property coord prop = 
  let color = prop |> Property.get_color |> convert_color in 
  fill_header color coord;
  let prop_name = Property.get_name prop in 
  let price = prop |> Property.get_price |> string_of_int in 
  draw_string_in_tile Center coord prop_name 45;
  draw_string_in_tile Center coord ("Price: "^price) 25

let test_prop = init_property 1 "PSB" Light_Blue Property 0 1 2 3 4 5 10 20 0

let draw_board () = 
  let rec loop = function 
    | [] -> () 
    | h :: t -> draw_tile_outline h; draw_tile_property h test_prop; loop t 
  in loop coords

let () = init_window; 
  try draw_board (); 
    let rec loop x y  = 
      let _ = wait_next_event [Poll] and wx' = size_x () and wy' = size_y ()
      in loop wx' wy'
    in 
    loop 0 0 
  with Graphic_failure _ -> print_endline "Exiting..."

