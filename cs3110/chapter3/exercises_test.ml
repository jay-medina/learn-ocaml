open OUnit2
open Ocaml_jasmine
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
  it "should return 1 for empty array" (fun _ -> 
    let result = product [] in
    expect result ToBe 1
  );
  it "should return 6 for product of three items" (fun _ -> 
    let result = product [1;2;3] in
    expect result ToBe 6
  );
  make_concat_test "empty str" "" [];
  make_concat_test "first last" "Jose Medina" ["Jose"; " "; "Medina"];
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
  it "should return true" (fun _ -> 
    let result = is_unimodal [] in
    expect result ToBe true
  )

]


let _ = run_test_tt_main tests
