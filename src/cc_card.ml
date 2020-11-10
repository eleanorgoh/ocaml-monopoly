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