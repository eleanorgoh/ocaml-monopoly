open Graphics
open Property

let init_window = 
  open_graph ""; 
  set_window_title "Monopoly"; 
  resize_window 1000 800

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

let draw_tile coord color = match coord with 
  | (x, y) -> set_color color; fill_rect x y 100 100

let draw_ugly_board = 
  let rec loop = function 
    | [] -> () 
    | h :: t -> draw_tile h light_blue; loop t 
  in loop coords
(* set_color green;
   fill_rect 0 0 100 100;
   fill_rect 200 0 100 100;
   fill_rect 0 100 100 100;
   fill_rect 0 500 100 100;
   fill_rect 500 400 100 100;
   set_color pink;
   fill_rect 300 0 100 100;
   fill_rect 0 200 100 100;
   fill_rect 200 500 100 100;
   fill_rect 500 200 100 100;
   set_color light_blue;
   fill_rect 100 0 100 100;
   fill_rect 400 0 100 100;
   fill_rect 0 300 100 100;
   fill_rect 300 500 100 100;
   fill_rect 500 300 100 100;
   set_color red;
   fill_rect 0 100 100 100;
   fill_rect 500 100 100 100;
   fill_rect 0 400 100 100;
   fill_rect 400 500 100 100;
   set_color yellow;
   fill_rect 500 0 100 100;
   fill_rect 200 0 100 100;
   fill_rect 100 500 100 100;
   fill_rect 500 500 100 100 *)

let () = init_window; 
  try draw_ugly_board; 
    let rec loop x y  = 
      let _ = wait_next_event [Poll] and wx' = size_x () and wy' = size_y ()
      in loop wx' wy'
    in 
    loop 0 0 
  with Graphic_failure _ -> print_endline "Exiting..."

