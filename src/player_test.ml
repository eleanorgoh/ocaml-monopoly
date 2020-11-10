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

let init_player_tests = [
  three_arg_func_test "Player: name = 'Joe', marker = 'default' - test name" 
    init_new_player "Joe" "default" get_name String.escaped "Joe";
  three_arg_func_test "Player: name = 'Bobo', marker = 'ugly' - test marker" 
    init_new_player "Bobo" "ugly" get_marker_type String.escaped "ugly";
  three_arg_func_test "Player: name = 'Bobo', marker = 'ugly' - test money" 
    init_new_player "Bobo" "ugly" get_money string_of_int 1500;
  (* three_arg_func_test "Player: name = 'Bobo', marker = 'ugly' - test properties" 
     init_new_player "Bobo" "ugly" get_properties String.escaped "ugly"; *)
]

let suite =
  "test suite for A2"  >::: List.flatten [
    init_player_tests;
  ]

let _ = run_test_tt_main suite