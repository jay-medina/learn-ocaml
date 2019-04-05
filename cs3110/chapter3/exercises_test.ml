open OUnit2
open Exercises

let make_product_test name_of_test expected input = 
  let result = product input in 
  name_of_test >:: 
    (fun _ -> 
      assert_equal 
        expected 
        result 
        ~printer:string_of_int
    )

let make_concat_test name_of_test expected input = 
  let result = concat input in 
  name_of_test >:: (fun _ -> 
    assert_equal 
    expected 
    result 
    ~printer: (fun x -> x)
    )

let tests = "test suite for exercises" >::: [
  make_product_test "empty array" 1 [];
  make_product_test "three items" 6 [1;2;3];
  make_concat_test "empty str" "" [];
  make_concat_test "first last" "Jose Medina" ["Jose"; " "; "Medina"]
]


let _ = run_test_tt_main tests
