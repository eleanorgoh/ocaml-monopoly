type t = {
  name : string; 
  account_id : int; 
  marker : string;
}

let init_new_player name id marker = {
  name = name;
  account_id = id;
  marker = marker;
}

let get_name p = p.name 

let get_account p = p.account_id

let get_marker_type p = p.marker

let set_name p name = {p with name = name}

let set_account_id p id = {p with account_id = id}

let set_marker_type p marker = {p with marker = marker}

