open OUnit2

let tests = "test suite for exercises" >::: [
  
]
(* open Sum

let make_sum_test name expected_output input =
  let result = sum input in
  name >:: (fun _ -> assert_equal expected_output result ~printer:string_of_int)

let tests = "test suite for sum" >::: [
  make_sum_test "empty" 0 [];
  make_sum_test "one" 1 [1];
  make_sum_test "onetwo" 3 [1; 2];
]

(* old version *)
(* let tests = "test suite for sum" >::: [
  "empty"  >:: (fun _ -> assert_equal 0 (sum []) ~printer:string_of_int);
  "one"    >:: (fun _ -> assert_equal 1 (sum [1]) ~printer:string_of_int);
  "onetwo" >:: (fun _ -> assert_equal 3 (sum [1; 2]) ~printer:string_of_int);
] *)

let _ = run_test_tt_main tests *)
