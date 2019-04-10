open OUnit2
open Ocaml_jasmine
open Exercises

let make_concat_test name_of_test expected input = 
  let result = concat input in 
  name_of_test >:: (fun _ -> 
    assert_equal 
    expected 
    result 
    ~printer: (fun x -> x)
    )

let rec listCompare first second = 
    match (first, second) with 
    |  ([], []) -> true
    |  (x::xs, y::ys) -> x = y && listCompare xs ys
    |  _ -> false

let make_list_desc_test name_of_test input expected = 
  let result = sortDesc input in
  name_of_test >:: (fun _ -> 
    assert_equal
    expected
    result
    ~cmp: listCompare
  )


let make_last_element_test test_name input expected = 
  let result = getLastElement input in 
  test_name >:: (fun _ -> 
    assert_equal
    expected
    result
  )

let make_anyzeros_test test_name input expected = 
  let result = anyZeros input in 
  test_name >:: (fun _ -> 
    assert_equal
    expected
    result
  )

let make_li_test test_name fn input expected = 
  let result = fn 2 input in 
  test_name >:: (fun _ -> 
    assert_equal
    expected
    result
    ~cmp: listCompare
  )

let tests = "test suite for exercises" >::: [
  test "should return 1 for empty array" (fun _ -> 
    let result = product [] in
    expectToBeEqual result 1
  );
  test "should return 6 for product of three items" (fun _ -> 
    let result = product [1;2;3] in
    expectToBeEqual result 6
  );
  test "empty str" (fun _ -> 
    expectToBeEqual (concat []) ""
  );
  test "first last" (fun _ -> 
    expectToBeEqual "Jose Medina" (concat ["Jose"; " "; "Medina"])
  );
  make_list_desc_test "empty array" [] [];
  make_list_desc_test "sorted asc" [1;2;3] [3;2;1];
  make_list_desc_test "random order" [4;6;3;2;4;5] [6;5;4;4;3;2];
  make_last_element_test "empty array" [] None;
  make_last_element_test "filled array" [1;2;3;4] (Some 4);
  make_anyzeros_test "empty array" [] false;
  make_anyzeros_test "filled array, no zeros" [2;3;4;5;6] false;
  make_anyzeros_test "filled array, zeros" [2;3;0;5;6] true;
  make_li_test "empty array" take [] [];
  make_li_test "2 item array" take [1;2] [1;2];
  make_li_test "5 item array" take [1;2;3;4;5] [1;2];
  make_li_test "empty array" drop [] [];
  make_li_test "2 item array" drop [1;2] [];
  make_li_test "5 item array" drop [1;2;3;4;5] [3;4;5];
  test "should return true" (fun _ ->
    expectToBeTruthy (is_unimodal [])
  );
  test "should return true for single item" (fun _ ->
    expectToBeTruthy (is_unimodal [2])
  );
  test "should return true for increasing list" (fun _ ->
    expectToBeTruthy (is_unimodal [2;3;4;5;6])
  );
  test "should return true for decreasing list" (fun _ ->
    expectToBeTruthy (is_unimodal [6;5;4;3;2])
  );
  test "should return false for mix items list" (fun _ -> 
    expectToBeFalsy (is_unimodal [3;2;4;5;1])
  );

]


let _ = run_test_tt_main tests
