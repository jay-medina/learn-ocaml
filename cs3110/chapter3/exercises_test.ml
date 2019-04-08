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

let make_list_desc_test name_of_test input expected = 
  let result = sortDesc input in 
  let rec listCompare first second = 
    match (first, second) with 
    |  ([], []) -> true
    |  (x::xs, y::ys) -> x = y && listCompare xs ys
    |  _ -> false
  in
  name_of_test >:: (fun _ -> 
    assert_equal
    expected
    result
    ~cmp: listCompare
  )

let tests = "test suite for exercises" >::: [
  make_product_test "empty array" 1 [];
  make_product_test "three items" 6 [1;2;3];
  make_concat_test "empty str" "" [];
  make_concat_test "first last" "Jose Medina" ["Jose"; " "; "Medina"];
  make_list_desc_test "empty array" [] [];
  make_list_desc_test "sorted asc" [1;2;3] [3;2;1];
  make_list_desc_test "random order" [4;6;3;2;4;5] [6;5;4;4;3;2];
]


let _ = run_test_tt_main tests
