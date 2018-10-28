(*


Consider a non empty array of integers a.

  1. Write a function min : int array -> int that returns the minimal element of a.
  2. Write a function min_index : int array -> int that returns the index of the minimal element of a.

Do you think these functions work well on large arrays ?

  3. Define a variable it_scales and set it to "yes" or "no".

*)


let min a =
  let rec findMin a index minValue =
    if index == Array.length a then minValue
    else if a.(index) < minValue then findMin a (index + 1) a.(index)
    else findMin a (index + 1) minValue
  in
    if Array.length a = 1 then a.(0)
    else 
      findMin a 1 a.(0)
    

let min_index a =
  let rec findMin a index minIndex =
    if index == Array.length a then minIndex
    else if a.(index) < a.(minIndex) then findMin a (index + 1) index
    else findMin a (index + 1) minIndex
  in
    if Array.length a = 1 then 0
    else 
      findMin a 1 0

let it_scales =
  "no";;

print_endline (string_of_int (min_index [|2;2;-1|]))
