open OUnit2

let expectToBeEqual first second = first = second
let expectToBeTruthy first = first = true
let expectToBeFalsy first = first = false

let it (test_name: string) (test_fn: _ -> bool) = 
  test_name >:: (fun _ -> 
    assert_equal
    (test_fn())
    true
  )

let test = it


let test1 = it "should test toBe" (fun _ -> 
  expectToBeEqual 11 12
)

let test2 = it "should test toBeTruthy" (fun _ ->
  expectToBeTruthy (2+2=4) 
)

let test3 = it "should test toBeFalsy" (fun _ ->
  expectToBeFalsy (2+2=3) 
)
