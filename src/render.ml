open Graphics

let init_window = 
  open_graph ""; 
  set_window_title "Monopoly"; 
  resize_window 600 600

let draw_ugly_board = 
  set_color green;
  fill_rect 0 0 100 100;
  fill_rect 200 0 100 100;
  fill_rect 0 100 100 100;
  fill_rect 0 500 100 100;
  fill_rect 500 400 100 100;
  set_color magenta;
  fill_rect 300 0 100 100;
  fill_rect 0 200 100 100;
  fill_rect 200 500 100 100;
  fill_rect 500 200 100 100;
  set_color cyan; 
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
  fill_rect 500 500 100 100

let () = init_window; 
  try draw_ugly_board; 
    let rec loop x y  = 
      let _ = wait_next_event [Poll] and wx' = size_x () and wy' = size_y ()
      in loop wx' wy'
    in 
    loop 0 0 
  with Graphic_failure _ -> print_endline "Exiting..."

