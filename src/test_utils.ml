open OUnit2
open Property
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

(** [two_arg_getter f arg1 arg2 expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [getter (f arg1 arg2)]. *)
let two_arg_getter 
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

(** [two_arg_func_exception name f arg1 arg2 ex] constructs an OUnit
    test named [name] that asserts that [f arg1 arg2] raises 
    [ex]. *)
let two_arg_func_exception
    (name : string) 
    f
    arg1
    arg2
    ex : test = 
  name >:: (fun _ -> 
      assert_raises ex (fun _ -> f arg1 arg2))

(** [three_arg_func_exception f arg1 arg2 arg3 ex] constructs 
    an OUnit test named [name] that asserts that [f arg1 arg2 arg3] raises 
    [ex]. *)
let three_arg_func_exception 
    (name : string) 
    f
    arg1
    arg2
    arg3
    ex : test = 
  name >:: (fun _ -> 
      assert_raises ex (fun _ -> f arg1 arg2 arg3))

let property_to_string prop = Property.get_name prop

let property_option_to_string = function 
  | None -> "None"
  | Some p -> Property.get_name p

let player_to_string p = 
  (get_name p) ^ " " ^ (pp_list property_to_string (Player.get_properties p))
