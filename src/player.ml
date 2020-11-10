type t = {
  name : string; 
  money : int;
  (* properties : Property.t list; *)
  marker : string;
}

let init_new_player name marker = {
  name = name;
  money = 1500;
  (* properties = []; *)
  marker = marker;
}

let get_name p = p.name 

let get_money p = p.money

let get_marker_type p = p.marker

let set_name p name = {p with name = name}

let set_money p amt = {p with money = amt}

let set_marker_type p marker = {p with marker = marker}

