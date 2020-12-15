open OUnit2
open Action 
open Player
open Test_utils

(** [check_validity] is false if the outcome of [roll] is of type Step and 
    has a value not in [2..35], true otherwise. *)
let check_validity = function
  | Jail -> true 
  | Step n -> n >= 2 && n <= 35
  | _ -> assert false

(** [roll_dice_test name  printer expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [check_validity roll_dice]. *)
let roll_dice_test 
    (name : string)
    expected_output : test = 
  name >:: (fun _ ->
      assert_equal expected_output (check_validity roll_dice)
        ~printer:string_of_bool)

let roll_tests = [
  roll_dice_test "roll dice 1" true;
  roll_dice_test "roll dice 2" true;
  roll_dice_test "roll dice 3" true;
  roll_dice_test "roll dice 4" true;
]

let prop_1 = Property.init_property 0 "PSB" Green Property 0 1 2 3 4 5 1 1 0
let prop_2 = Property.init_property 0 "Home" Yellow Property 0 1 2 3 4 5 1 1 0
let prop_3 = Property.init_property 0 "Slope" Yellow Property 0 1 2 3 4 5 1 1 0
let prop_4 = Property.init_property 
    0 "Duffield" Yellow Property 0 1 2 3 4 5 0 1 0
let prop_5 = Property.init_property 
    0 "Mac's" Yellow Property 0 1 2 3 4 5 1 9999 0
let prop_6 = Property.init_property 
    0 "Terrace" Green Property 1 1 1 1 1 1 1 9999 0

let prop_7 = Property.init_property 0 "Sage" Brown Property 1 1 1 1 1 1 1 10 0 
let prop_8 = Property.init_property 
    0 "Expensive Rent" Green Property 9999 9999 9999 9999 9999 9999 9999 10 0
let prop_9 = Property.init_property 
    0 "Olin" Light_Blue Property 1 1 1 1 1 1 1 10 0
let prop_10 = Property.init_property 
    0 "Libe" Light_Blue Property 0 0 0 1 1 1 0 0 0 

let p1 = init_new_player "Sam" "gudetama"

let p2 = init_new_player "Eleanor" "puppy"
let p3 = init_new_player "John" "bear"
let p4 = Action.buy_property prop_4 (init_new_player "George" "camel")
let p5 = Action.buy_property prop_8 (init_new_player "Orwell" "animal")

let p1_1_prop = add_property (init_new_player "Sam" "gudetama") 
    prop_1 
let p1_2_prop = add_property p1_1_prop 
    prop_3
let p2_1_prop = add_property (init_new_player "Eleanor" "puppy") prop_10
let p3_1_prop = add_property (init_new_player "John" "bear") prop_6
let p4_1_prop = add_property (init_new_player "Eric" "horse") prop_6
let p6_1_prop = add_property (init_new_player "Sarah" "fish") prop_6
let p6_2_prop = add_property p6_1_prop prop_7
let p7_1_prop = add_property (init_new_player "Emily" "dog") prop_6
let p7_2_prop = add_property p7_1_prop prop_9
let p8 = init_new_player "Jam" "cat"


(* Buys first building for p1 on prop1 *)
let () = Action.buy_building prop_1 p1_1_prop 

(* Buys first building for p1 on prop3 *)
let () = Action.buy_building prop_3 p1_2_prop 
(* Buys second building for p1 on prop3 *)
let () = Action.buy_building prop_3 p1_2_prop 
(* Buys third building for p1 on prop3 *)
let () = Action.buy_building prop_3 p1_2_prop
(* Buys fourth building for p1 on prop3 *)
let () = Action.buy_building prop_3 p1_2_prop 
(* Buys fifth building for p1 on prop3 *)
let () = Action.buy_building prop_3 p1_2_prop 


let () = Action.buy_building prop_4 p4 
let () = Action.buy_building prop_4 p4 
let () = Action.buy_building prop_4 p4 
(** P4 collects rent from P5, legally, prop_4 has 3 houses *)
let () = Action.collect_rent p4 p5 prop_4

(** P2_1_prop collects rent from P3, and has 0 houses *)
let () = Action.collect_rent p2_1_prop p3 prop_10

let transaction_tests = [
  (* buy_property tests *)
  two_arg_getter "Player with no properties buys one property - check property" 
    Action.buy_property prop_1 p1 Player.get_properties 
    (pp_list property_to_string) [prop_1]; 
  two_arg_func_exception "Player tries to buy property that's too expensive" 
    Action.buy_property prop_5 p1
    (TransactionError "Player does not have enough money to buy this 
      property.");
  two_arg_getter "Player with no properties buys one property - check money" 
    Action.buy_property prop_1 p8 Player.get_money string_of_int 1499; 
  two_arg_func_exception "Player tries to buy property they already own" 
    Action.buy_property prop_1 p1_1_prop
    (TransactionError "Player already owns this property.");

  (* buy_building tests *)
  two_arg_func_exception "Player with no properties buys building" 
    Action.buy_building prop_1 p2 
    (TransactionError "Player does not own this property.");
  one_arg_func_test "Player with one property that has no
   buildings buys building" 
    Property.get_num_buildings prop_1 string_of_int 1;
  two_arg_func_exception "Player tries to buy sixth building on property" 
    Action.buy_building prop_3 p1_2_prop
    (TransactionError "Player cannot build another building on 
      this property.");

  (* collect_rent tests *)
  one_arg_func_test "Check transaction after P5 lands on P4's property with 
     3 houses - P5" Player.get_money p5 string_of_int 1487;
  one_arg_func_test "Check transaction after P5 lands on P4's property with 
     3 houses - P4" Player.get_money p4 string_of_int 1502;
  one_arg_func_test "Check transaction after P3 lands on P2's property with 
     0 houses - P2" Player.get_money p2_1_prop string_of_int 1500;
  one_arg_func_test "Check transaction after P3 lands on P2's property with 
     0 houses - P3" Player.get_money p3 string_of_int 1500;
  three_arg_func_exception "P5 collects rent from P1, legally, prop_8 brings 
     P1 bankrupt" Action.collect_rent p5 p1 prop_8 
    (PlayerBankrupt "Payer is bankrupt!");

  (* sell_property tests *)
  two_arg_func_exception "Player tries to sell property they don't own" 
    Action.sell_property prop_1 p3
    (TransactionError "Player can't sell property they don't own.");
  two_arg_getter "Player with one property sells property - check property" 
    Action.sell_property prop_6 p3_1_prop Player.get_properties 
    (pp_list property_to_string) []; 
  two_arg_getter "Player with one property sells property - check money" 
    Action.sell_property prop_6 p4_1_prop Player.get_money string_of_int 
    (1500 + 4999); 
  two_arg_getter "Player with two properties sells property - check property" 
    Action.sell_property prop_7 p6_2_prop Player.get_properties 
    (pp_list property_to_string) [prop_6]; 
  two_arg_getter "Player with two properties sells property - check money" 
    Action.sell_property prop_9 p7_2_prop Player.get_money string_of_int 
    (1505); 
]

let suite =
  "test suite for Action"  >::: List.flatten [
    roll_tests;
    transaction_tests;
  ]

let _ = run_test_tt_main suite
