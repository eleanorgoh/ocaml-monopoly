open OUnit2
open Cc_card
open Test_utils

(** [string_of_card card] is the string form of [card]. *)
let string_of_card = function 
  | Community -> "Community"
  | Chance -> "Chance"

let string_of_action  = function
  | Pay int -> "Pay"
  | Receive int -> "Receive"
  | Advance int-> "Advance"
  | OutJail -> "OutJail"
  | GoJail -> "GoJail"


let card1 = init_card Community "Pay $200 for taxes" (Pay 200)
let card2 = init_card Chance "Receive $200 from bank for birthday" (Receive 200)
let card3 = init_card Community "Get out of jail free card" (OutJail)
let card4 = init_card Chance "It's your unlucky day. Go to jail" (GoJail)
let card5 = init_card Community "It's your lucky day. Advance to Go" (Advance "Go")


let card_tests = [
  one_arg_func_test "Pay $200 for taxes, - test name"  
    get_name card1 String.escaped "Pay $200 for taxes";
  one_arg_func_test "Pay $200 for taxes, - test category"  
    get_category card1 string_of_card Community;
  one_arg_func_test "Pay $200 for taxes, - test action" 
    get_action card1 string_of_action (Pay 200) ;

  one_arg_func_test "Receive $200 from bank for birthday, - test name"  
    get_name card2 String.escaped "Receive $200 from bank for birthday";
  one_arg_func_test "Receive $200 from bank for birthday, - test category"  
    get_category card2 string_of_card Chance;
  one_arg_func_test "Receive $200 from bank for birthday, - test action" 
    get_action card2 string_of_action (Receive 200);

  one_arg_func_test "Get out of jail free card, - test name"  
    get_name card3 String.escaped "Get out of jail free card";
  one_arg_func_test "Get out of jail free card, - test category"  
    get_category card3 string_of_card Community;
  one_arg_func_test "Get out of jail free card, - test action" 
    get_action card3 string_of_action OutJail;

  one_arg_func_test "It's your unlucky day. Go to jail, - test name"  
    get_name card4 String.escaped "It's your unlucky day. Go to jail";
  one_arg_func_test "It's your unlucky day. Go to jail, - test category"  
    get_category card4 string_of_card Chance;
  one_arg_func_test "It's your unlucky day. Go to jail, - test action" 
    get_action card4 string_of_action GoJail;

  one_arg_func_test "It's your lucky day. Advance to Go, - test name"  
    get_name card5 String.escaped "It's your lucky day. Advance to Go";
  one_arg_func_test "It's your lucky day. Advance to Go, - test category"  
    get_category card5 string_of_card Community;
  one_arg_func_test "It's your lucky day. Advance to Go, - test action" 
    get_action card5 string_of_action (Advance "Go");

]

let suite =
  "test suite for card"  >::: List.flatten [
    card_tests;
  ]

let _ = run_test_tt_main suite