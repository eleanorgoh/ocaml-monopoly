type pos = int

type state = {
  players : (Player.t * pos) list;

  board : Newboard.t;

  chance_stack : Cc_card.t list;

  community_stack : Cc_card.t list;
}

let create_card (category : Cc_card.category) (board : Newboard.t)= 
  let names = Newboard.get_names board in 
  let x = Random.int (List.length names) in
  match Random.int 5 with 
  | 0 -> Cc_card.init_card category "Pay" (Cc_card.Pay (Random.int 100))
  | 1 -> Cc_card.init_card category "Receive" (Cc_card.Receive (Random.int 100))
  | 2 -> Cc_card.init_card category "Get out of jail" Cc_card.OutJail
  | 3 -> Cc_card.init_card category "Go to jail" Cc_card.GoJail
  | 4 -> 
    Cc_card.init_card category "Advance" (Cc_card.Advance (List.nth names x))
  | _ -> failwith "Somehow it chose an impossible random number"

let rec create_card_stack 
    (category : Cc_card.category) (board : Newboard.t) lst count = 
  match count with 
  | 0 -> lst
  | _ -> let card = create_card category board in 
    create_card_stack category board (card::lst) (count - 1)

let init_state (players : Player.t list) (board : Newboard.t) = {
  players = List.map (fun x -> (x,0)) players;
  board = board;
  chance_stack = create_card_stack Cc_card.Chance board [] 16;
  community_stack = create_card_stack Cc_card.Community board [] 16;
}

let current_pos = failwith ""

let winner = failwith ""

let handle_command = failwith ""

let valid_command = failwith ""