type pos = int

type state = {
  players : (Player.t * pos) list;
  board : Newboard.t;
  chance_stack : Cc_card.t list;
  community_stack : Cc_card.t list;
  num_rolls : int;
}

type t = state

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

let create_players (lst : (string * string) list) = 
  List.map (fun (x,y) -> Player.init_new_player x y) lst

let init_state (board : Newboard.t) (players : (string * string) list) =
  let instantiate_players = create_players players in 
  {
    players = List.map (fun x -> (x,0)) instantiate_players;
    board = board;
    chance_stack = create_card_stack Cc_card.Chance board [] 16;
    community_stack = create_card_stack Cc_card.Community board [] 16;
    num_rolls = 0;
  }

let rec pos_helper name players : int = 
  match players with
  | [] -> failwith "Could not find player."
  | h::t -> if Player.get_name (fst h) = name then snd h 
    else pos_helper name t

let current_pos state name = pos_helper name state.players

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

let current_tile state = 
  let player_pos = snd (List.hd state.players) in 
  "Currently you are on: " ^ Property.get_name (List.nth state.board player_pos)
  ^ "." 

let valid_command state (command : Command.t) = 
  match command with 
  | End_Turn ->  true
  | Forfeit -> true
  | Roll -> if state.num_rolls = 0 then true else false
  | Buy s -> 
    let player = fst (List.hd state.players) in 
    let pos = snd (List.hd state.players) in 
    let property = List.nth state.board pos in 
    if Property.get_num_buildings property < 5 
    && Property.get_name property = s
    && Property.get_price property <= Player.get_money player
    then true else false
  | Sell s -> 
    let player = fst (List.hd state.players) in 
    let pos = snd (List.hd state.players) in 
    let property = List.nth state.board pos in 
    let property_names = 
      List.map (fun x -> Property.get_name x) (Player.get_properties player) in
    if Property.get_name property = s 
    && List.mem s property_names
    then true else false
  | Collect (s1, s2, s3) -> failwith "Unimplemented collect"
  | _ -> failwith "Command should not have been run/DNE."

let rec options_helper state (lst : Command.t list) str = 
  match lst with 
  | [] -> str 
  | h::t -> if valid_command state h
    then options_helper state t (Command.to_string h ^ " " ^ str) 
    else options_helper state t str

let view_options state = 
  let pos = snd (List.hd state.players) in 
  let property = List.nth state.board pos in 
  let property_name = Property.get_name property in 
  let commands_lst = [Command.End_Turn; Command.Forfeit; Command.Roll;
                      Command.Buy property_name; Command.Sell property_name;] in
  options_helper state commands_lst ""

let move_card_to_bottom lst = (List.tl lst)@[List.hd lst]

let rec tile_pos board tile_name : int= 
  match board with
  | [] -> failwith "Jail not found"
  | h::t -> if Property.get_name h = tile_name
    then Property.get_pos h else tile_pos t tile_name

let rec handle_command state (command : Command.t)  = 
  match command with 
  | End_Turn ->
    let curr_player = List.hd state.players in 
    let new_lst = (List.tl state.players)@[curr_player] in 
    {state with players = new_lst; num_rolls = 0}
  | Forfeit -> {
      players = List.tl state.players;
      board = Newboard.reset_properties (state.board) 
          (fst (List.hd state.players));
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 0;
    }
  | Buy s -> 
    let player = fst (List.hd state.players) in 
    let pos = snd (List.hd state.players) in 
    let property = List.nth state.board pos in
    begin
      match Player.get_property_by_name player s with 
      | None -> let updated_player = Action.buy_property property player in 
        {state with players = (updated_player, pos)::List.tl state.players}
      | Some prop -> Action.buy_building property player; state
    end
  | Sell s -> 
    let player = fst (List.hd state.players) in 
    let pos = snd (List.hd state.players) in 
    let property = List.nth state.board pos in
    begin
      match Player.get_property_by_name player s with 
      | None -> failwith "Does not own property"
      | Some prop ->
        let updated_player = Action.sell_property property player in 
        {state with players = (updated_player, pos)::List.tl state.players}
    end
  | Roll -> 
    let roll = Action.roll_dice in roll_helper roll state
  | _ -> failwith "Command should not have been run/DNE."

and roll_helper (roll: Action.t) state= 
  match roll with 
  | Step x -> 
    let player = fst (List.hd state.players) in
    let pos = (snd (List.hd state.players) + x) mod List.length state.board in 
    let new_state = 
      {state with players = (player, pos)::List.tl state.players} in
    let board = state.board in 
    let property = List.nth board pos in
    advance_helper new_state property
  | Jail -> 
    let player = fst (List.hd state.players) in
    let pos = tile_pos state.board "Jail" in 
    {
      players = (player, pos)::List.tl state.players;
      board = state.board;
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 1;
    }
  | Draw_Chance | Draw_Community -> failwith "Impossible"

and card_helper card state = 
  let player = fst (List.hd state.players) in 
  match Cc_card.get_action card with
  | Pay x -> 
    let balance = Player.get_money player in 
    Player.set_money player (balance + x); 
    state
  | Receive x -> 
    let balance = Player.get_money player in 
    Player.set_money player (balance - x);
    state
  | OutJail -> 
    let pos = snd (List.hd state.players) in 
    let update_player = Player.receive_jail_card player in 
    {state with players = (update_player, pos)::List.tl state.players}
  | GoJail -> 
    let pos = tile_pos state.board "Jail" in 
    {state with players = (player, pos)::List.tl state.players}
  | Advance s -> 
    let pos = tile_pos state.board s in 
    let new_state = 
      {state with players = (player, pos)::List.tl state.players} in
    let property = List.nth new_state.board pos in 
    advance_helper new_state property

(* Updates player position. Automatically draws card if it lands on a chance
     or community tile. Automatically moves to jail if it lands on go to jail.
     Automatically charges rent if lands on property owned by someone else.*)
and advance_helper state property = 
  match Property.get_type property with 
  (* Get charged if land on someone elses property *)
  | Property -> failwith ""
  (* Get charged if land on someone elses property *)
  | Railroad -> failwith ""
  (* Get charged if land on someone elses property *)
  | Utility -> failwith ""
  (* Get charged. *)
  | Tax -> failwith ""
  | Chance_card -> 
    let card = List.hd state.chance_stack in 
    let new_state = card_helper card state in 
    {new_state with chance_stack = move_card_to_bottom new_state.chance_stack}
  | Community_chest -> 
    let card = List.hd state.chance_stack in 
    let new_state = card_helper card state in 
    {new_state with chance_stack = move_card_to_bottom new_state.chance_stack}
  | Go_to_jail -> roll_helper Action.Jail state
  | In_jail_just_visiting | Go | Free_parking -> state