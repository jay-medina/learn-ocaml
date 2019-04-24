open OUnit2
open Ocaml_jasmine
open Exercises

let tests = "test suite" >::: [
  test "should pass" (fun _ -> 
    expectToBeEqual 1 1
  )
]
