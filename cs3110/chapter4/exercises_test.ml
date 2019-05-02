open OUnit2
open Ocaml_jasmine
open Exercises

let printerForListOfInts li = 
  li 
  |> (List.map string_of_int) 
  |> (List.fold_left (^) "")

let tests = "test suite" >::: [
  test "should pass" (fun _ -> 
    expectToBeEqual 1 1
  );
  test "twice = twice2" (fun _ -> 
    let fn = (fun x -> 2 * x) in 
    let result1 = (twice fn 2) in 
    let result2 = (twice2 fn 2) in 
    expectToBeEqual result1 result2
  );
  test "cliplist1 = cliplist2" (fun _ -> 
    let li = [1;2;3;10;11;12;0;22] in 
    let result1 = cliplist1 li in 
    let result2 = cliplist2 li in 
    expectToBeEqual result1 result2
  );
  "range function" >:: (fun _ -> 
    let li = 1 -- 5 in 
    assert_equal
    li
    [1;2;3;4;5]
    ~printer: printerForListOfInts
  );
  "sum cube odd" >:: (fun _ -> 
    let s = sum_cube_odd 5 in 
    assert_equal s 153
    ~printer:string_of_int
  );
  "sum cube odd pipe" >:: (fun _ -> 
    let s = sum_cube_odd_pipe 5 in 
    assert_equal s 153
    ~printer:string_of_int
  )
]


let _ = run_test_tt_main tests
