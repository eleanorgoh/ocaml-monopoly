open OUnit2
open Action 
open Player
open Cc_card
open Command
open Property
open Test_utils

(* --------------------------- TESTING APPROACH ---------------------------
   1. OUnit tests
   - The following modules were tested by OUnit: Action, Player, Cc_card, 
     Command, Property, and State. Tests were developed using both black box 
     and white box testing. We aimed to test every function in these modules, 
     testing both that the output was correct given the input and that 
     the tests would reach all branches of code within those functions. 
     We also tested that exceptions were raised. It was important to 
     thoroughly test these modules because they make up the foundation 
     of the game, and some modules like Action use functions defined in 
     other modules like Player. When playing the game, there are many 
     different moves that players can make that can result in different 
     states of the game, and illegal moves raise exceptions. Using this 
     testing approach, we were able to ensure the correctness of what 
     was happening in the game. 

   3. Manual tests
   - The following modules were tested manually: Render and Main. 
     Main is the game engine, and Render is the GUI, so by playing the game 
     and using the GUI, we were able to manually verify the correctness of 
     these modules. 
*)


(* --------------------------- CC_CARD TESTS --------------------------- *)
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
let card5 = 
  init_card Community "It's your lucky day. Advance to Go" (Advance "Go")


let card_tests = [
  one_arg_func_test "Pay $200 for taxes, - test name"  
    Cc_card.get_name card1 String.escaped "Pay $200 for taxes";
  one_arg_func_test "Pay $200 for taxes, - test category"  
    get_category card1 string_of_card Community;
  one_arg_func_test "Pay $200 for taxes, - test action" 
    get_action card1 string_of_action (Pay 200) ;

  one_arg_func_test "Receive $200 from bank for birthday, - test name"  
    Cc_card.get_name card2 String.escaped "Receive $200 from bank for birthday";
  one_arg_func_test "Receive $200 from bank for birthday, - test category"  
    get_category card2 string_of_card Chance;
  one_arg_func_test "Receive $200 from bank for birthday, - test action" 
    get_action card2 string_of_action (Receive 200);

  one_arg_func_test "Get out of jail free card, - test name"  
    Cc_card.get_name card3 String.escaped "Get out of jail free card";
  one_arg_func_test "Get out of jail free card, - test category"  
    get_category card3 string_of_card Community;
  one_arg_func_test "Get out of jail free card, - test action" 
    get_action card3 string_of_action OutJail;

  one_arg_func_test "It's your unlucky day. Go to jail, - test name"  
    Cc_card.get_name card4 String.escaped "It's your unlucky day. Go to jail";
  one_arg_func_test "It's your unlucky day. Go to jail, - test category"  
    get_category card4 string_of_card Chance;
  one_arg_func_test "It's your unlucky day. Go to jail, - test action" 
    get_action card4 string_of_action GoJail;

  one_arg_func_test "It's your lucky day. Advance to Go, - test name"  
    Cc_card.get_name card5 String.escaped "It's your lucky day. Advance to Go";
  one_arg_func_test "It's your lucky day. Advance to Go, - test category"  
    get_category card5 string_of_card Community;
  one_arg_func_test "It's your lucky day. Advance to Go, - test action" 
    get_action card5 string_of_action (Advance "Go");

]

(* --------------------------- COMMAND TESTS --------------------------- *)
(** [string_of_command command] is the string form of [command]. *)
let string_of_command = function 
  | Start -> "Start"
  | Quit -> "Quit"
  | Restart -> "Restart"
  | End_Turn -> "End Turn" 
  | Forfeit -> "Forfeit"
  | Player_Name p -> "Player Name: " ^ p 
  | Roll -> "Roll"
  | Draw_Chance -> "Draw Chance"
  | Draw_Community -> "Draw Community"
  | Buy p -> "Buy " ^ p
  | Sell p -> "Sell " ^ p
  | Collect (p1, p2, prop) -> "Collect: " ^ p1 ^ " collecting from " ^ 
                              p2 ^ " for " ^ prop 
  | Help -> "Help"


let helper_tests = [

  (* split_str *)
  one_arg_func_test "split str: empty" split_str "" (pp_list String.escaped)
    [];
  one_arg_func_test "split str: all spaces" split_str "" 
    (pp_list String.escaped) [];
  one_arg_func_test "split str: only one word, no spaces" split_str "hello" 
    (pp_list String.escaped) ["hello"];
  one_arg_func_test "split str: only one word, leading/trailing spaces" 
    split_str "          hello      " (pp_list String.escaped) ["hello"];
  one_arg_func_test "split str: two words, only one space in between" 
    split_str "hello bye" (pp_list String.escaped) ["hello"; "bye"];
  one_arg_func_test "split str: two words, more than one space 
                in between" split_str "hello          bye" 
    (pp_list String.escaped) ["hello"; "bye"];
  one_arg_func_test "split str: two words, leading/trailing spaces" 
    split_str "  hello          bye   " 
    (pp_list String.escaped) ["hello"; "bye"];

  (* capitalize_property *)
  two_arg_func_test "capitalize prop: both empty" capitalize_property "" "" 
    String.escaped "";
  two_arg_func_test "capitalize prop: str1 empty, str2 not empty" 
    capitalize_property "" "atlantic" String.escaped "Atlantic";
  two_arg_func_test "capitalize prop: str1 not empty, str2 empty" 
    capitalize_property "london" "" String.escaped "London";
  two_arg_func_test "capitalize prop: both not empty" 
    capitalize_property "london" "bridge" String.escaped "London Bridge";
  two_arg_func_test "capitalize prop: remove leading whitespace" 
    capitalize_property "london" "   bridge" String.escaped "London Bridge";
  two_arg_func_test "capitalize prop: remove trailing whitespace" 
    capitalize_property "london   " "bridge      " String.escaped 
    "London Bridge";
  two_arg_func_test "capitalize prop: remove leading and 
          trailing whitespace" capitalize_property " london   " 
    "    bridge      " String.escaped "London Bridge";

]

let check_input_tests = [
  (* check_one_arg_input *)
  one_arg_func_exception "empty string" check_one_arg_input "" 
    (Unparsable "Please enter a valid command.");
  one_arg_func_test "start" check_one_arg_input "start" string_of_command Start;
  one_arg_func_test "quit" check_one_arg_input "quit" string_of_command Quit;
  one_arg_func_test "restart" check_one_arg_input "restart" 
    string_of_command Restart;
  one_arg_func_test "forfeit" check_one_arg_input "forfeit" 
    string_of_command Forfeit;
  one_arg_func_test "roll" check_one_arg_input "roll" string_of_command Roll;
  one_arg_func_test "help" check_one_arg_input "help" string_of_command Help; 

  (* check_two_arg_input *)
  two_arg_func_test "name: empty" check_two_arg_input "name" "" 
    string_of_command (Player_Name "");
  two_arg_func_test "name: not empty" check_two_arg_input "name" "Jen" 
    string_of_command (Player_Name "Jen");
  two_arg_func_test "end: end turn" check_two_arg_input "end" "turn" 
    string_of_command End_Turn;
  two_arg_func_exception "end: exception - empty" check_two_arg_input
    "end" "" (Unparsable "Please enter a valid command. 
      Did you mean 'end turn'?");
  two_arg_func_exception "end: exception - non empty" check_two_arg_input
    "end" "fdfkcdkmckdfj" (Unparsable "Please enter a valid command. 
      Did you mean 'end turn'?");
  two_arg_func_test "draw community" check_two_arg_input 
    "draw" "community" string_of_command Draw_Community;
  two_arg_func_test "draw chance" check_two_arg_input 
    "draw" "chance" string_of_command Draw_Chance;
  two_arg_func_exception "draw: exception - empty" check_two_arg_input
    "draw" "" (Unparsable "Please enter a valid command. 
        Did you mean 'draw chance' or 'draw community'?");
  two_arg_func_exception "draw: exception - non empty" check_two_arg_input
    "draw" "fdfkcdkmckdfj" (Unparsable "Please enter a valid command. 
        Did you mean 'draw chance' or 'draw community'?");

  (* check_buysell_input *)
  three_arg_func_exception "exception" check_buysell_input "fjdkjf" "" "hi"
    (Unparsable "Please enter a valid command.");
  three_arg_func_test "buy: both empty" check_buysell_input "buy" "" "" 
    string_of_command (Buy "");
  three_arg_func_test "buy: first empty" check_buysell_input "buy" "" 
    "atlantic" string_of_command (Buy "Atlantic");
  three_arg_func_test "buy: second empty" check_buysell_input "buy" "london" 
    "" string_of_command (Buy "London");
  three_arg_func_test "buy: both non empty" check_buysell_input 
    "buy" "   london " "  bridge   " string_of_command (Buy "London Bridge");
  three_arg_func_test "sell: both non empty" check_buysell_input 
    "sell" "   london " "  bridge   " string_of_command (Sell "London Bridge");

  (* check_collect_input *)
  five_arg_func_test "collect: Jen collecting from Sarah, non empty prop"
    check_collect_input "collect" "Jen" "Sarah" "atlantic  " "  ave  " 
    string_of_command (Collect ("Jen", "Sarah", "Atlantic Ave")); 
  five_arg_func_exception "collect: Jen collecting from Sarah, prop = 'Jen'"
    check_collect_input "collect" "Jen" "Sarah" "Jen" "" 
    (Unparsable "Please enter a valid command. If you want to 
  collect money from someone, enter the command as follows: 
  'collect <your username> <their username> <property name>'.");
  five_arg_func_exception "collect: Jen collecting from Sarah, empty prop"
    check_collect_input "collect" "Jen" "Sarah" "" "" 
    (Unparsable "Please enter a valid command. If you want to 
  collect money from someone, enter the command as follows: 
  'collect <your username> <their username> <property name>'.");
  five_arg_func_exception "collect: Jen collecting from Sarah, 
        prop = 'Sarah'" check_collect_input "collect" "Jen" "Sarah" "Jen" "" 
    (Unparsable "Please enter a valid command. If you want to 
  collect money from someone, enter the command as follows: 
  'collect <your username> <their username> <property name>'.");
  five_arg_func_exception "collect: Jen collecting from Sarah, 
        both property names are the same" check_collect_input 
    "collect" "Jen" "Sarah" "atlantic" "atlantic" 
    (Unparsable "Please enter a valid command. If you want to 
  collect money from someone, enter the command as follows: 
  'collect <your username> <their username> <property name>'.");
  five_arg_func_exception "collect: first word not 'collect'" 
    check_collect_input " c jfdkjfkdj" "Jen" "Sarah" "atlantic" "ave" 
    (Unparsable "Please enter a valid command.");

  (* create_command *)
  one_arg_func_exception "empty command" create_command [] 
    (Unparsable "Please enter a non-empty command.");
  one_arg_func_exception "random command" create_command 
    ["hi"; "bye"; "fjdk"; "fdeec"; "     "; "ocodfds"] 
    (Unparsable "Please enter a valid command.");
  one_arg_func_test "one arg" create_command ["start"] string_of_command
    Start;
  one_arg_func_test "two args" create_command ["name"; "jen"] string_of_command
    (Player_Name "Jen");
  one_arg_func_test "three args" create_command 
    ["buy"; "atlantic   "; "  ave"] string_of_command (Buy "Atlantic Ave");
  one_arg_func_test "five args" create_command 
    ["collect"; "Jen   "; "  sarah"; "  atlantic "; " ave  "]
    string_of_command (Collect ("Jen", "Sarah", "Atlantic Ave"));

  (* parse *)
  one_arg_func_exception "parse: empty string" parse "" 
    (Unparsable "Please enter a non-empty command.");
  one_arg_func_exception "parse: all spaces" parse "           " 
    (Unparsable "Please enter a non-empty command.");
  one_arg_func_test "one word with spaces" parse 
    "    start      " string_of_command Start;
  one_arg_func_test "one word no spaces" parse 
    "start" string_of_command Start;
  one_arg_func_test "two words, one space in between, all lowercase" 
    parse "end turn" string_of_command End_Turn;
  one_arg_func_test "two words, one space in between, mixed case" parse 
    "EnD tUrn" string_of_command End_Turn;
  one_arg_func_test "three words, one space in between" parse 
    "buy atlantic ave" string_of_command (Buy "Atlantic Ave");
  one_arg_func_test "three words, multiple spaces in between" parse 
    "         bUy       atlAntIC               avE        " 
    string_of_command (Buy "Atlantic Ave");
  one_arg_func_test "five words, multiple spaces in between" parse 
    "         collect       jen               sarah atlantic        ave" 
    string_of_command (Collect ("Jen", "Sarah", "Atlantic Ave"));


]

(* --------------------------- PLAYER TESTS --------------------------- *)

let init_player_tests = [
  two_arg_getter "Player: name = 'Joe', marker = 'default' - test name" 
    init_new_player "Joe" "default" Player.get_name String.escaped "Joe";
  two_arg_getter "Player: name = 'Bobo', marker = 'ugly' - test marker" 
    init_new_player "Bobo" "ugly" get_marker_type String.escaped "ugly";
  two_arg_getter "Player: name = 'Bobo', marker = 'ugly' - test money" 
    init_new_player "Bobo" "ugly" get_money string_of_int 1500;
  two_arg_getter "Player: name = 'Bobo', marker = 'ugly' - test properties" 
    init_new_player "Bobo" "ugly" Player.get_properties 
    (pp_list property_to_string) [];
]

let prop_1 = Property.init_property 1 "PSB" Green Property 1 1 1 1 2 3 4 5 0
let prop_2 = Property.init_property 1 "Home" Yellow Property 1 1 1 1 1 1 1 1 0
let prop_3 = Property.init_property 1 "Slope" Yellow Property 1 1 1 1 1 1 1 1 0
let prop_4 = Property.init_property 
    1 "Duffield" Yellow Property 1 1 1 1 1 1 1 1 0
let prop_5 = Property.init_property 1 "Mac's" Yellow Property 1 1 1 1 1 1 1 1 0
let player_1 = init_new_player "Sam" "gudetama"
let player_1_with_one_property = add_property (init_new_player "Sam" "gudetama") 
    prop_1 
let player_1_with_two_properties = add_property player_1_with_one_property 
    prop_3
let player_2 = init_new_player "Eleanor" "puppy"

(** [get_property_by_name_test player property expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output]
    with [get_property_by_name (player property)]. *)
let get_property_by_name_test 
    (name : string) 
    player
    property_name
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_property_by_name player property_name) 
        ~printer:property_option_to_string)

let property_tests = [
  (* Test get_property_by_name *)
  get_property_by_name_test "Eleanor has no properties" player_2 "PSB" None;
  get_property_by_name_test "Sam with 1 property" player_1_with_one_property 
    "PSB" (Some prop_1);
  get_property_by_name_test "Sam with 2 properties" player_1_with_two_properties 
    "Slope" (Some prop_3);
  get_property_by_name_test "Sam does not have Libe" player_1_with_one_property 
    "Libe" None;

  (* Test get_properties *)
  one_arg_func_test "Eleanor has no properties" Player.get_properties player_2 
    (pp_list property_to_string) [];
  one_arg_func_test "Sam has 1 property" Player.get_properties 
    player_1_with_one_property (pp_list property_to_string) [prop_1];
  one_arg_func_test "Sam has 2 properties" Player.get_properties 
    player_1_with_two_properties (pp_list property_to_string) [prop_3; prop_1];

  (* Test remove_property *)
  two_arg_func_exception "Removing but Eleanor has no properties" 
    Player.remove_property player_2 prop_1 
    (UpdateError "Player does not own this property");
  two_arg_func_test "Removing one of Sam's 2 properties" Player.remove_property 
    player_1_with_two_properties prop_3 player_to_string 
    player_1_with_one_property;
  two_arg_func_test "Removing all of Sam's properties" Player.remove_property 
    player_1_with_one_property prop_1 player_to_string 
    player_1;
]

(* --------------------------- ACTION TESTS --------------------------- *)
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
      assert_equal expected_output (check_validity (roll_dice ()))
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

(* --------------------------- PROPERTY TESTS --------------------------- *)
(** [string_of_color color] is the string form of [color]. *)
let string_of_color = function 
  | Brown -> "Brown"
  | Light_Blue -> "Light_Blue"
  | Pink -> "Pink"
  | Orange -> "Orange" 
  | Red -> "Red"
  | Yellow -> "Yellow"
  | Green -> "Green"
  | Dark_Blue -> "Dark_Blue"
  | No_Color -> "No_Color"

let prop1 = init_property 1 "PSB" Light_Blue Railroad 0 1 2 3 4 5 10 20 0
let prop2 = init_property 2 "Duffield" Brown Property 0 1 2 3 4 5 10 20 0
let prop3_1build = init_property 3 "Sage" Green Tax 0 1 2 3 4 5 10 20 0
let () = add_building prop3_1build

let prop4_2build = init_property 4 "WSH" Pink Property 0 1 2 3 4 5 10 20 0
let () = add_building prop4_2build
let () = add_building prop4_2build


let init_property_tests = [
  one_arg_func_test "PSB, Light Blue, 0123451020, - test name"  
    get_name prop1 String.escaped "PSB";
  one_arg_func_test "PSB, Light Blue, 0123451020, - test color"  
    get_color prop1 string_of_color Light_Blue;
  one_arg_func_test "PSB, Light Blue, 0123451020, - test rent cost 
    no buildings" get_rent_cost prop1 string_of_int 0;
  one_arg_func_test "PSB, Light Blue, 0123451020, - test building cost" 
    get_building_cost prop1 string_of_int 10;
  one_arg_func_test "PSB, Light Blue, 0123451020, - test price" 
    get_price prop1 string_of_int 20;
  one_arg_func_test "PSB, Light Blue, 0123451020, - test num buildings" 
    get_num_buildings prop1 string_of_int 0;
]

let add_property_tests = [
  one_arg_func_test "Sage, Green, 0123451020, - test num buildings" 
    get_num_buildings prop3_1build string_of_int 1;
  one_arg_func_test "WSH, Pink, 0123451020, - test num buildings" 
    get_num_buildings prop4_2build string_of_int 2;
  one_arg_func_test "Sage, Green, 0123451020, - test rent cost 
    1 building" get_rent_cost prop3_1build string_of_int 1;
  one_arg_func_test "WSH, Pink, 0123451020, - test rent cost 
    2 buildings" get_rent_cost prop4_2build string_of_int 2;
]

let suite =
  "test suite"  >::: List.flatten [

    (* CC_card test suite *)
    card_tests;

    (* Command test suite *)
    helper_tests;
    check_input_tests;

    (* Player test suite *)
    init_player_tests;
    property_tests;

    (* Action test suite *)
    roll_tests;
    transaction_tests;

    (* Property test suite *)
    init_property_tests;
    add_property_tests;
  ]

let _ = run_test_tt_main suite