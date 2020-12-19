
type state = {
  players : (Player.t * int) list;
  board : Newboard.t;
  chance_stack : Cc_card.t list;
  community_stack : Cc_card.t list;
  num_rolls : int;
}

type t = state

let print_roll_message () = 
  print_string ("\n~~~~~~~~~~~~~~~~~~~~" 
                ^ " Rolling the dice " ^ "~~~~~~~~~~~~~~~~~~~~ \n");
  print_string("───▄██▄─██��───▄
─▄██████████��███��
─▌████████████▌
▐▐█░█▌░▀████▀░░
░▐▄▐▄░░░▐▄▐▄░░░░
" ^ "\n")

let print_pass_go () = 
  print_string ("Hooray! You passed 'Go'! You will now receive $200. \n\n");
  print_string (" /$$$$$$   /$$$$$$   /$$$$$$ 
 /$$__  $$ /$$$_  $$ /$$$_  $$
|__/  \ $$| $$$$\ $$| $$$$\ $$
  /$$$$$$/| $$ $$ $$| $$ $$ $$
 /$$____/ | $$\ $$$$| $$\ $$$$
| $$      | $$ \ $$$| $$ \ $$$
| $$$$$$$$|  $$$$$$/|  $$$$$$/
|________/ \\______/  \\______/ 

                              " ^ "\n")

let print_jail_message () = 
  print_string ("Unfortunately, you are going to jail. \n\n");
  print_string 
    ("█████ █████ █████ █████ █████ █████ █████ █████ █████ █████ █████ █████ 
                                                                        
██ ██ ██ ██ ██ ██          ██  █████  ██ ██          ██ ██ ██ ██ ██ ██  
██ ██ ██ ██ ██ ██          ██ ██   ██ ██ ██          ██ ██ ██ ██ ██ ██  
██ ██ ██ ██ ██ ██          ██ ███████ ██ ██          ██ ██ ██ ██ ██ ██  
██ ██ ██ ██ ██ ██     ██   ██ ██   ██ ██ ██          ██ ██ ██ ██ ██ ██  
██ ██ ██ ██ ██ ██      █████  ██   ██ ██ ███████     ██ ██ ██ ██ ██ ██  
                                                                        
█████ █████ █████ █████ █████ █████ █████ █████ █████ █████ █████ █████" ^ "\n")


let get_player_pos state : (Player.t * int) list = 
  state.players

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

let has_special_char str = 
  if String.contains str '_' then 0 
  else if String.contains str '-' then 1
  else 2

let get_special_char num = 
  if num = 0 then '_' else '-'

let transform_hyphen_str str = 
  let char_num = has_special_char str in 
  if char_num = 2 then str 
  else let c = get_special_char char_num in 
    let index = String.index str c in 
    let length_right = String.length str - index - 2 in 
    let uppercase_char = String.get str (index + 1) |> 
                         Char.escaped |> String.uppercase_ascii in 
    let left = String.sub str 0 (index + 1) in 
    let right = String.sub str (index + 2) length_right in 
    left ^ uppercase_char ^ right


let current_pos state name = pos_helper name state.players

let rec count_balances count players = 
  match players with
  | [] -> count
  | h::t -> let p = fst h in 
    if Player.get_money p > 0 || Player.get_properties p = []
    then count_balances (count + 1) t else count_balances count t

let rec find_winner players = 
  match players with
  | [] -> None
  | h::t -> if Player.get_money (fst h) > 0 then Some (fst h)
    else find_winner t

let winner state = 
  if count_balances 0 state.players = 1 then find_winner state.players
  else None

let get_board state = state.board 

let player_stat state = 
  Player.player_to_string (fst (List.hd state.players))

let current_tile state = 
  let player_pos = snd (List.hd state.players) in 
  "Currently you are on the following tile: " ^ 
  Property.get_name (List.nth state.board player_pos)
  ^ ". \n" 

let valid_buy state command str = 
  let player = fst (List.hd state.players) in 
  let pos = snd (List.hd state.players) in 
  let property = List.nth state.board pos in 
  let property_names = 
    List.map (fun x -> Property.get_name x) (Player.get_properties player) in
  let prop = String.capitalize_ascii str |> transform_hyphen_str in 
  if Property.get_num_buildings property < 5 
  && Property.get_price property <= Player.get_money player
  && not (List.mem prop property_names)
  then true else false

let valid_sell state command str = 
  let player = fst (List.hd state.players) in 
  let pos = snd (List.hd state.players) in 
  let curr_prop = List.nth state.board pos in 
  let property_names = 
    List.map (fun x -> Property.get_name x) (Player.get_properties player) in
  let prop = String.capitalize_ascii str |> transform_hyphen_str in 
  if (Property.get_name curr_prop = prop 
      && List.mem (Property.get_name curr_prop) property_names)
  || List.mem prop property_names
  then true else false

let valid_command state (command : Command.t) = 
  match command with 
  | End_Turn ->  true
  | Forfeit -> true
  | Roll -> if state.num_rolls = 0 then true else false
  | Buy s -> valid_buy state command s

  | Sell s -> valid_sell state command s
  | Quit -> true
  | Collect (s1, s2, s3) -> failwith "Unimplemented collect"
  | _ -> failwith "Command should not have been run/DNE."


let rec options_helper state (lst : Command.t list) str = 
  match lst with 
  | [] -> "This is a list of moves you can make: " ^ 
          String.sub str 0 (String.length str - 2) ^ ". \n"
  | h::t -> if valid_command state h
    then options_helper state t (Command.to_string h ^ ", " ^ str) 
    else options_helper state t str

let get_all_other_prop_options other_properties prop  = 
  let rec loop acc prop = function 
    | [] -> acc 
    | h :: t -> 
      if h <> prop then loop (acc @ [Command.Buy h; Command.Sell h]) prop t
      else loop (acc @ [Command.Buy ("Building")]) prop t 
  in loop [] prop other_properties

let view_options state = 
  let player = fst (List.hd state.players) in 
  let pos = snd (List.hd state.players) in 
  let property = List.nth state.board pos in 
  let property_name = Property.get_name property in 
  let all_properties = List.map (fun x -> Property.get_name x) 
      (Player.get_properties player) in
  let commands_lst = [Command.End_Turn; Command.Forfeit; 
                      Command.Buy property_name; Command.Sell property_name;
                      Command.Roll; ] 
                     @ (get_all_other_prop_options all_properties property_name) 
  in options_helper state commands_lst ""

let move_card_to_bottom lst = (List.tl lst)@[List.hd lst]

let rec tile_pos board tile_name : int= 
  match board with
  | [] -> failwith "Tile not found"
  | h::t -> if Property.get_name h = tile_name
    then Property.get_pos h else tile_pos t tile_name

let rec handle_command state (command : Command.t)  = 
  match command with 
  | End_Turn -> handle_end_turn state command 
  | Forfeit -> {
      players = List.tl state.players;
      board = Newboard.reset_properties (state.board) 
          (fst (List.hd state.players));
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 0;
    }
  | Buy s -> handle_buy state command s
  | Sell s -> handle_sell state command s
  | Roll -> 
    let roll = Action.roll_dice () in roll_helper roll state
  | Quit -> state
  | _ -> failwith "Command should not have been run/DNE."

and handle_end_turn state command = 
  let curr_player = List.hd state.players in 
  let new_lst = (List.tl state.players)@[curr_player] in 
  {state with players = new_lst; num_rolls = 0}


and handle_buy state command str = 
  let player = fst (List.hd state.players) in 
  let pos = snd (List.hd state.players) in 
  let property = List.nth state.board pos in
  begin
    match Player.get_property_by_name player (Property.get_name property) with 
    | None -> let updated_player = Action.buy_property property player in 
      print_string ("Buying the following property: " ^ 
                    (Property.get_name property) ^ "!\n");
      {state with players = (updated_player, pos)::List.tl state.players}
    | Some prop -> Action.buy_building property player; 
      print_string ("Buying a building on the following property: " 
                    ^ (Property.get_name property) ^ "!\n");
      state
  end

and handle_sell state command str = 
  let player = fst (List.hd state.players) in 
  let pos = snd (List.hd state.players) in 
  let prop = String.capitalize_ascii str |> transform_hyphen_str in 

  begin
    match Player.get_property_by_name player (prop) with 
    | None -> failwith "Does not own property"
    | Some p ->
      let updated_player = Action.sell_property p player in 
      print_string ("Selling the following property: " ^ 
                    (prop) ^ "!\n");
      {state with players = (updated_player, pos)::List.tl state.players}
  end


and roll_helper (roll: Action.t) state = 
  match roll with 
  | Step x -> roll_step_helper state x
  | Jail -> roll_jail_helper state
  | Draw_Chance | Draw_Community -> failwith "Impossible"

and roll_step_helper state steps = 
  let player = fst (List.hd state.players) in
  let old_pos = snd (List.hd state.players) in 
  let pos = (old_pos + steps) mod (List.length state.board) in 
  let new_state = 
    {state with players = (player, pos)::List.tl state.players} in
  print_roll_message ();
  print_string ("After rolling, you are moving " ^ 
                (string_of_int steps) ^ " steps forward. \n\n");
  if old_pos > pos && pos <> 0 then 
    let balance = Player.get_money player in 
    Player.set_money player (balance + 200); 
    print_pass_go ();
  else print_string ("");
  let board = state.board in 
  let property = List.nth board pos in
  advance_helper new_state property

and roll_jail_helper state = 
  let player = fst (List.hd state.players) in
  let pos = tile_pos state.board "Go_to_jail" in 
  print_jail_message ();
  if Player.has_jail_card player then 
    let () = print_string 
        ("BUT, you have a 'Get Out Of Jail' card! Using that card now. \n") in 
    let new_player = Player.use_jail_card player in 
    {
      players = (new_player, pos)::List.tl state.players;
      board = state.board;
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 0;
    }
  else 
    let () = roll_jail_pay_helper player in
    {
      players = (player, pos)::List.tl state.players;
      board = state.board;
      chance_stack = state.chance_stack;
      community_stack = state.community_stack;
      num_rolls = 1;
    } 

and roll_jail_pay_helper player = 
  let balance = Player.get_money player in 
  Player.set_money player (balance - 50); 
  print_string ("\nYou have to pay the bank $50 :(. \n");
  print_string ("Your balance is now: $" 
                ^ string_of_int (Player.get_money player) ^ "\n");

and card_helper card state = 
  let player = fst (List.hd state.players) in 
  match Cc_card.get_action card with
  | Pay x -> pay_helper card state player x
  | Receive x -> receive_helper card state player x
  | OutJail -> out_jail_helper card state player
  | GoJail -> go_jail_helper card state player
  | Advance s -> advance_pre_helper card state player s

and pay_helper card state player amt = 
  let balance = Player.get_money player in 
  Player.set_money player (balance - amt); 
  print_string ("This card says you have to pay the bank $" 
                ^ string_of_int amt ^ " :(. \n");
  print_string ("Your balance is now: $" 
                ^ string_of_int (Player.get_money player) ^ "\n");
  state

and receive_helper card state player amt = 
  let balance = Player.get_money player in 
  Player.set_money player (balance + amt);
  print_string ("It's your lucky day! " ^ "This card says you receive $" 
                ^ string_of_int amt ^ " from the bank. \n");
  print_string ("Your balance is now: $" 
                ^ string_of_int (Player.get_money player) ^ "\n");
  state

and out_jail_helper card state player = 
  print_string ("This card gives you a 'Get Out Of Jail' card! \n");
  let pos = snd (List.hd state.players) in 
  let update_player = Player.receive_jail_card player in 
  {state with players = (update_player, pos)::List.tl state.players}

and go_jail_helper card state player = 
  print_string ("Rip. This card says you're going to jail. \n");
  let pos = tile_pos state.board "Go_to_jail" in 
  if Player.has_jail_card player then 
    let () = print_string 
        ("BUT, you have a 'Get Out Of Jail' card! Using that card now. \n") in 
    let new_player = Player.use_jail_card player in 
    {state with players = (new_player, pos)::List.tl state.players}
  else 
    {state with players = (player, pos)::List.tl state.players}

and advance_pre_helper card state player str = 
  print_string ("This card says you get to advance to " ^ str ^ "! \n");
  let pos = tile_pos state.board str in 
  let new_state = 
    {state with players = (player, pos)::List.tl state.players} in
  let property = List.nth new_state.board pos in 
  advance_helper new_state property

and advance_helper state property = 
  match Property.get_type property with 
  | Property | Railroad | Utility -> 
    handle_prop_util_rroad state property
  | Tax -> handle_tax state property
  | Chance_card -> 
    handle_chance_card state property
  | Community_chest -> 
    handle_community_chest state property
  | Go_to_jail -> 
    print_string 
      ("RIP. You landed on a 'Go To Jail' tile... \n");
    roll_helper Action.Jail state
  | Go -> print_string 
            ("You landed on 'Go'! Back to square one now. \n");
    state
  | In_jail_just_visiting -> 
    print_string 
      ("You landed in jail... but thank goodness you're just visiting!");
    state 
  | Free_parking -> 
    print_string ("You landed on a free parking space! \n");
    print_string ("You can just sit back and chill. \n");
    state 

and handle_prop_util_rroad state property = 
  print_string ("You landed on a " 
                ^ Property.string_of_property_type 
                  (Property.get_type property) ^ " tile. \n");
  get_charged state property

and handle_tax state property = 
  print_string ("You landed on a Tax tile. \n");
  let player = fst (List.hd state.players) in 
  let tax_amount = Property.get_rent_cost property in 
  let balance = Player.get_money player in 
  Player.set_money player (balance - tax_amount); 
  print_string ("You are being taxed $" 
                ^ string_of_int tax_amount ^ " by the bank. \n");
  state

and handle_chance_card state property = 
  print_string ("You landed on a Chance Card! \n");
  let card = List.hd state.chance_stack in 
  let new_state = card_helper card state in 
  {new_state with chance_stack = move_card_to_bottom new_state.chance_stack}

and handle_community_chest state property = 
  print_string ("You landed on a Community Chest Card! \n");
  let card = List.hd state.chance_stack in 
  let new_state = card_helper card state in 
  {new_state with chance_stack = move_card_to_bottom new_state.chance_stack}


and player_owns player property = 
  let property_name = Property.get_name property in 
  match Player.get_property_by_name player property_name with 
  | Some x -> true
  | None -> false

and others_owns_helper player_lst property=
  match player_lst with 
  | [] -> None
  | h::t -> match player_owns (fst h) property with
    | false -> others_owns_helper t property
    | true -> Some (fst h)

and get_charged state property = 
  let player = fst (List.hd state.players) in 
  if player_owns player property then state
  else 
    let players = List.tl state.players in
    match others_owns_helper players property with
    | None -> state
    | Some h -> let owner = h in 
      Action.collect_rent owner player property;
      print_string ("You are being charged for rent by " 
                    ^ (Player.get_name owner) ^ " for: " 
                    ^ (Property.get_name property) ^ ". \n");
      state