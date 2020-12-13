type category = 
  | Community
  | Chance

type action = 
  | Pay of int
  | Receive of int
  | OutJail
  | GoJail
  | Advance of string

type t = {
  category : category;
  name : string;
  action : action;
}

let init_card ctg name act = {
  category = ctg;
  name = name;
  action = act;
}

let get_name card = card.name

let get_category card = card.category

let get_action card = card.action