open Cc_card

type t = 
  | Jail
  | Step of int 
  | Draw_Chance 
  | Draw_Community

exception TransactionError of string
exception PlayerBankrupt of string

let roll_dice = 
  let die_1 = Random.int 5 + 1 in 
  let die_2 = Random.int 5 + 1 in 
  let die_3 = Random.int 5 + 1 in 
  let die_4 = Random.int 5 + 1 in 
  let die_5 = Random.int 5 + 1 in 
  let die_6 = Random.int 5 + 1 in 
  if die_1 <> die_2 then Step (die_1 + die_2) 
  else if die_3 <> die_4 then Step (die_1 + die_2 + die_3 + die_4) 
  else if die_5 <> die_6 then Step (die_1 + die_2 + die_3 + die_4 
                                    + die_5 + die_6) 
  else Jail

let draw_community = Draw_Community

let draw_chance = Draw_Chance

let buy_property property player = 
  let property_cost = Property.get_price property in 
  let player_money = Player.get_money player in 
  let existing_properties = Player.get_properties player in 
  if List.mem property existing_properties then raise 
      (TransactionError "Player already owns this property.")
  else if player_money < property_cost then raise 
      (TransactionError "Player does not have enough money to buy this 
      property.")
  else 
    Player.set_money player (player_money - property_cost); 
  Player.add_property player property 

let buy_building property player =  
  let existing_properties = Player.get_properties player in 
  let num_properties = Property.get_num_buildings property in 
  let player_money = Player.get_money player in 
  let building_cost = Property.get_building_cost property in 
  if not (List.mem property existing_properties) then raise 
      (TransactionError "Player does not own this property.")
  else if num_properties = 5 then raise 
      (TransactionError "Player cannot build another building on 
      this property.")
  else if player_money < building_cost then raise 
      (TransactionError "Player does not have enough money to buy a building.")
  else 
    Player.set_money player (player_money - building_cost);
  Property.add_building property  

let collect_rent collector payer property = 
  if not (List.mem property (Player.get_properties collector)) then raise 
      (TransactionError "Collector does not own this property.")
  else
    let rent = Property.get_rent_cost property in 
    let payer_money = Player.get_money payer in 
    let collector_money = Player.get_money collector in 
    if payer_money < rent then raise (PlayerBankrupt "Payer is bankrupt!")
    else 
      Player.set_money collector (collector_money + rent); 
    Player.set_money payer (payer_money - rent)

let sell_property property player = 
  let resale_price = Property.get_price property / 2 in 
  let player_money = Player.get_money player in 
  let prop_opt = Player.get_property_by_name player (Property.get_name property) 
  in 
  match prop_opt with 
  | None -> 
    raise (TransactionError "Player can't sell property they don't own.")
  | Some prop -> 
    Player.set_money player (player_money + resale_price); 
    Player.remove_property player prop 
