open Cc_card

type t = 
  | Jail
  | Step of int 
  | Draw_Chance 
  | Draw_Community

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