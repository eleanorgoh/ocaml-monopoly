open OUnit2
open Action 

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

(** [check_validity] is false if the outcome of [roll] is of type Step and 
    is in [2..35], true otherwise. *)
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

let suite =
  "test suite for Action"  >::: List.flatten [
    roll_tests;
  ]

let _ = run_test_tt_main suite
