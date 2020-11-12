open OUnit2
open Player

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [three_arg_func_test f arg1 arg2 expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [getter (f arg1 arg2)]. *)
let three_arg_func_test 
    (name : string) 
    f
    arg1
    arg2
    getter
    printer
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (getter (f arg1 arg2)) 
        ~printer:printer)

let property_option_to_string = function 
  | None -> "None"
  | Some p -> Property.get_name p

let property_to_string prop = Property.get_name prop

let init_player_tests = [
  three_arg_func_test "Player: name = 'Joe', marker = 'default' - test name" 
    init_new_player "Joe" "default" get_name String.escaped "Joe";
  three_arg_func_test "Player: name = 'Bobo', marker = 'ugly' - test marker" 
    init_new_player "Bobo" "ugly" get_marker_type String.escaped "ugly";
  three_arg_func_test "Player: name = 'Bobo', marker = 'ugly' - test money" 
    init_new_player "Bobo" "ugly" get_money string_of_int 1500;
  three_arg_func_test "Player: name = 'Bobo', marker = 'ugly' - test properties" 
    init_new_player "Bobo" "ugly" Player.get_properties 
    (pp_list property_to_string) [];
]
let prop_1 = Property.init_property "PSB" Green 1 1 1 1 2 3 4 5 0
let prop_2 = Property.init_property "Home" Yellow 1 1 1 1 1 1 1 1 0
let prop_3 = Property.init_property "Slope" Yellow 1 1 1 1 1 1 1 1 0
let prop_4 = Property.init_property "Duffield" Yellow 1 1 1 1 1 1 1 1 0
let prop_5 = Property.init_property "Mac's" Yellow 1 1 1 1 1 1 1 1 0
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

(** [one_arg_func_test f arg1 printer expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [f arg1]. *)
let one_arg_func_test 
    (name : string) 
    f
    arg1
    printer
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f arg1)
        ~cmp:cmp_set_like_lists
        ~printer:printer)

(** [two_arg_func_test f arg1 arg2 printer expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [f arg1 arg2]. *)
let two_arg_func_test 
    (name : string) 
    f
    arg1
    arg2
    printer
    expected_output : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (f arg1 arg2)
        ~printer:printer)

(** [update_error name f arg1 arg2 str] constructs an OUnit
    test named [name] that asserts that [f arg1 arg2] raises 
    a UpdateError [str] exception. *)
let update_error
    (name : string) 
    f
    arg1
    arg2 
    str : test = 
  name >:: (fun _ -> 
      assert_raises (UpdateError str) (fun _ -> f arg1 arg2))

let player_to_string p = 
  (get_name p) ^ " " ^ (pp_list property_to_string (Player.get_properties p))

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
    player_1_with_two_properties (pp_list property_to_string) [prop_1; prop_3];

  (* Test remove_property *)
  update_error "Removing but Eleanor has no properties" Player.remove_property player_2 prop_1 "Player does not own this property";
  two_arg_func_test "Removing one of Sam's 2 properties" Player.remove_property 
    player_1_with_two_properties prop_3 player_to_string 
    player_1_with_one_property;
  two_arg_func_test "Removing all of Sam's properties" Player.remove_property 
    player_1_with_one_property prop_1 player_to_string 
    player_1;
]

let suite =
  "test suite for A2"  >::: List.flatten [
    init_player_tests;
    property_tests;
  ]

let _ = run_test_tt_main suite