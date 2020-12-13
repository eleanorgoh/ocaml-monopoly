type pos = int

type state = {
  players : (Player.t * pos) list;

  board : Newboard.t;

  chance_stack : CC_card.t list;

  community_stack : CC_card.t list;
}

let create_card (category : CC_card.category) (board : Newboard.t)= 
  let names = Newboard.get_names board in 
  let x = Random.int (List.length names) in
  match Random.int 5 with 
  | 0 -> CC_card.init_card category "Pay" (CC_card.Pay Random.int 100)
  | 1 -> CC_card.init_card category "Receive" (CC_card.Receive Random.int 100)
  | 2 -> CC_card.init_card category "Get out of jail" CC_card.OutJail
  | 3 -> CC_card.init_card category "Go to jail" CC_card.GoJail
  | 4 -> 
    CC_card.init_card category "Advance" (CC_card.Advance (List.nth x names))

let create_card_stack (card_type : CC_card.category) lst = failwith "" 

let init_state (players : Player.t list) (board : Newboard.t) = {
  players = List.map (fun x -> (x,0)) players;
  board = board;
  chance_stack = create_card_stack CC_card.Chance;
  community_stack = create_card_stack CC_card.Community;
}

let current_pos = failwith ""

let winner = failwith ""

let handle_command = failwith ""

let valid_command = failwith ""