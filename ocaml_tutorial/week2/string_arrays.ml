(*
  1. Write a function is_sorted : string array -> bool which checks if the values of the input array are sorted in strictly increasing order, implying that its elements are unique (use String.compare).
  
  
  2. Using the binary search algorithm, an element can be found very quickly in a sorted array. Write a function find : string array -> string -> int such that find arr word is the index of the word in the sorted array arr if it occurs in arr or -1 if word does not occur in arr.
  
  The number or array accesses will be counted, to check that you obtain the expected algorithmic complexity. Beware that you really perform the minimal number of accesses. For instance, if your function has to test the contents of a cell twice, be sure to put the result of the access in a variable, and then perform the tests on that variable.

*)

let is_sorted a =
  if (Array.length a) <= 1 then true
  else 
    let rec helper a1 index =
      if ((Array.length a1) - 1) = index then true
      else
        let first = a1.(index) in
        let snd = a1.(index+1) in
        let compareResult = String.compare first snd in

        if compareResult = -1 then (helper a1 (index + 1))
        else false
    in
      helper a 0 
    

let find dict word =
  let rec findHelper low high =
    if low > high then -1
    else 
      let mid = (low + high) / 2 in
      if dict.(mid) = word then mid
      else if (String.compare dict.(mid) word) = -1 then findHelper (mid + 1) high
      else findHelper low (mid - 1)
  in
    findHelper 0 ((Array.length dict) - 1)

let li = [| "bye"; "hi"; "meow" |] ;;
let result = find li "meows";;

print_endline (string_of_int result );;
