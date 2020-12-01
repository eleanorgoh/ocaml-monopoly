open OUnit2
open Command
open Test_utils

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

let suite =
  "test suite for command tests"  >::: List.flatten [
    helper_tests;
    check_input_tests;

  ]

let _ = run_test_tt_main suite