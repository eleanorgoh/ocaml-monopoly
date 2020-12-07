open OUnit2
open Player
open Test_utils

let init_player_tests = [
  two_arg_getter "Player: name = 'Joe', marker = 'default' - test name" 
    init_new_player "Joe" "default" get_name String.escaped "Joe";
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

let suite =
  "test suite for player test"  >::: List.flatten [
    init_player_tests;
    property_tests;
  ]

let _ = run_test_tt_main suite