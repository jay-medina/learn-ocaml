open OUnit2

type matchers = ToBe ;;

let expect first comparer second = 
    match comparer with
    | ToBe -> first = second


let it (test_name: string) (test_fn: _ -> bool) = 
  test_name >:: (fun _ -> 
    assert_equal
    (test_fn())
    true
  )
