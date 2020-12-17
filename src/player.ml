type t = {
  name : string; 
  mutable money : int;
  properties : Property.t list;
  marker : string;
  jail_card : bool;
}

let init_new_player name marker = {
  name = name;
  money = 1500;
  properties = [];
  marker = marker;
  jail_card = false;
}

exception UpdateError of string 

let get_name p = p.name 

let get_money p = p.money

let has_jail_card p = p.jail_card

let get_marker_type p = p.marker

let get_properties p = p.properties 

let get_property_by_name p name = 
  let property_lst = get_properties p in 
  let rec rec_find_property = function 
    | [] -> None
    | h :: t -> 
      if (Property.get_name h) = name then Some h 
      else rec_find_property t
  in 
  rec_find_property property_lst

let receive_jail_card p = 
  {p with jail_card = true}

let use_jail_card p = 
  if p.jail_card 
  then {p with jail_card = false}
  else failwith "Does not have jail card"

let add_property p prop = 
  if not (List.mem prop p.properties) then {p with properties = 
                                                     (prop :: p.properties)}
  else raise (UpdateError "Already owned property")

let remove_property p prop = 
  if (List.mem prop p.properties) then 
    {p with properties = List.filter (fun x -> x <> prop) p.properties} 
  else raise (UpdateError "Player does not own this property") 

let set_name p name = {p with name = name}

let set_money p amt = p.money <- amt

let set_marker_type p marker = {p with marker = marker}

let rec string_helper property_lst result=
  match property_lst with 
  | [] ->  String.sub result 0 (String.length result - 2) 
  | h::t -> string_helper t (Property.get_name h ^ ", "  ^ result)

let player_to_string p = 
  let basic_info = 
    "Player " ^ p.name ^ " (marker: " ^ p.marker ^ ") has a balance of $" ^ 
    string_of_int p.money ^ ". \n" in 
  if List.length p.properties = 0 then 
    basic_info ^ "You do not own any properties right now. \n"
  else 
    basic_info ^ "You own the following properties: " ^ 
    string_helper p.properties "" ^ ". \n"