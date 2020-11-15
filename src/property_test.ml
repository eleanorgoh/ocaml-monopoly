open OUnit2
open Property
open Test_utils

let prop1 = init_property "PSB" Light_Blue 0 1 2 3 4 5 10 20
let prop2 = init_property "Duffield" Brown 0 1 2 3 4 5 10 20
let prop3_1build = init_property "Sage" Green 0 1 2 3 4 5 10 20
let () = add_building prop3_1build

let prop4_2build = init_property "WSH" Pink 0 1 2 3 4 5 10 20
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
  "test suite for property"  >::: List.flatten [
    init_property_tests;
    add_property_tests;

  ]

let _ = run_test_tt_main suite
