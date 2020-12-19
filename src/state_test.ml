open OUnit2
open State
open Newboard
open Player
open Test_utils

let board = Yojson.Basic.from_file "board.json"
let game_board = from_json board

let board_to_string = Yojson.Basic.to_string board

let players_1 = [("David","Ship")]
let players_2 = [("Alan", "Cat"); ("Sam", "Hat")]
let players_3 = [("Alan", "Cat"); ("Sam", "Hat"); ("Eleanor", "Basket")]
let state_1 = init_state game_board players_1
let state_2 = init_state game_board players_2
let state_3 = init_state game_board players_3


let current_pos_test = [
  two_arg_func_test "Player: name = 'David', position = 0" 
    current_pos state_1 "David" string_of_int 0;
  two_arg_func_test "Player: name = 'Alan', position = 0" 
    current_pos state_2 "Alan" string_of_int 0;
  two_arg_func_test "Player: name = 'Sam', position = 0" 
    current_pos state_2 "Sam" string_of_int 0;
  two_arg_func_test "Player: name = 'Eleanor', position = 0" 
    current_pos state_2 "Eleanor" string_of_int 0;
  two_arg_func_test "Player: name = 'Sam', position = 0" 
    current_pos state_2 "Sam" string_of_int 0;
]

let init_state_board_test
    (name : string)
    (state : State.t)
    expected_output : test =
  name >:: (fun _ -> 
      assert_equal expected_output (get_board state))

let init_state_board_tests = [
  init_state_board_test "State: 1, Board: Board.json" state_1 game_board;
  init_state_board_test "State: 2, Board: Board.json" state_2 game_board;
  init_state_board_test "State: 3, Board: Board.json" state_3 game_board
]

let valid_command_test
    (name : string)
    (state : State.t)
    (command : Command.t)
    expected_output : test =
  name >:: (fun _ -> 
      assert_equal expected_output (valid_command state command))

let valid_command_tests = [
  valid_command_test "test for command = End_Turn" state_1 End_Turn true;
  valid_command_test "test for command = Forfeit" state_1 Forfeit true;
  valid_command_test "test for command = Roll" state_1 Roll true;
  valid_command_test "test for command = Buy" state_1 (Buy "PSB") true;
  valid_command_test "test for command = Sell" state_1 (Sell "PSB") false;
  valid_command_test "test for command = Quit" state_1 Quit true;
]


let suite =
  "test suite for card"  >::: List.flatten [
    current_pos_test;
    init_state_board_tests;
    valid_command_tests
  ]

let _ = run_test_tt_main suite