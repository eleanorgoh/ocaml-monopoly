type pos = int

type state = {
  players : (Player.t * pos) list;
  board : Newboard.t;
  chance_stack : Cc_card.t list;
  community_stack : Cc_card.t list;
  num_rolls : int;
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
  num_rolls = 0;
}

let rec pos_helper name players = 
  match players with
  | [] -> failwith "Could not find player."
  | h::t -> if Player.get_name (fst h) = name then snd h 
    else pos_helper name t

let current_pos name state = pos_helper name state.players

let rec count_balances count players = 
  match players with
  | [] -> count
  | h::t -> if Player.get_money (fst h) > 0 then count_balances (count + 1) t
    else count_balances count t

let rec find_winner players = 
  match players with
  | [] -> None
  | h::t -> if Player.get_money (fst h) > 0 then Some (fst h)
    else find_winner t

let winner state = 
  if count_balances 0 state.players = 1 then find_winner state.players
  else None

let player_stat state = Player.player_to_string (fst (List.hd state.players))

let current_tile state board = 
  let player_pos = snd (List.hd state.players) in 
  "Currently you are on: " ^ Property.get_name (List.nth board player_pos) ^ "." 

let view_options state = failwith ""

let valid_command (command : Command.t) state = 
  match command with 
  | End_Turn ->  true
  | Forfeit -> failwith ""
  | Roll -> failwith ""
  | Buy s -> failwith ""
  | Sell s -> failwith ""
  | Collect (s1, s2, s3) -> failwith ""
  | _ -> failwith "Command should not have been run/DNE."

let move_card_to_bottom lst = (List.tl lst)@[List.hd lst]

let card_helper card state = 
  match Cc_card.get_action card with
  | Pay x -> 
    let player = fst (List.hd state.players) in 
    let balance = Player.get_money player in 
    let pay = Player.set_money player (balance + x) in
    {
      players = state.players;
      board = state.board;
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 0;
    }
  | Receive x -> failwith ""
  | OutJail -> failwith ""
  | GoJail -> failwith ""
  | Advance s -> failwith ""

let handle_command (command : Command.t) state = 
  match command with 
  | End_Turn -> (* Outputs state for next player. *)
    let curr_player = List.hd state.players in 
    let new_lst = (List.tl state.players)@[curr_player] in 
    {
      players = new_lst;
      board = state.board;
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 0;
    }
  | Forfeit -> (* Removes player from list, and their buildings from board.*)
    {
      players = List.tl state.players;
      board = Newboard.reset_properties (state.board) 
          (fst (List.hd state.players));
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 0;
    }
  (* Updates player position. Automatically draws card if it lands on a chance
     or community tile. Automatically moves to jail if it lands on go to jail.*)
  | Roll -> 
    failwith ""
  (* Draws a card from stack, automatically executes action related to card.*)
  | Draw_Chance -> failwith ""
  | Draw_Community -> failwith ""

  (* Buys a property.*)
  | Buy s -> failwith ""
  (* Sells a property for half the price bought.*)
  | Sell s -> failwith ""

  | Collect (s1, s2, s3) -> failwith ""
  | _ -> failwith "Command should not have been run/DNE."