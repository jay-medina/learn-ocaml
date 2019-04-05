(* 

Exercise: list expressions [✭]

    Construct a list that has the integers 1 through 5 in it. 
    Use the square bracket notation for lists.

    Construct the same list, but do not use the square bracket notation. Instead use :: and [].

    Construct the same list again. This time, 
    the following expression must appear in your answer: [2;3;4]. 
    Use the @ operator, and do not use ::.

*)
let li1 = [1;2;3;4;5]
let li1 = 1::2::3::4::[5]
let li1 = [1] @ [2;3;4] @ [5];;

(* 
Exercise: product [✭✭]

Write a function that returns the product of all the elements in a list. 
The product of all the elements of an empty list is 1. 

Hint: recall the sum function we defined in lecture. 

Put your code in a file named lab03.ml. Use the toplevel to test your code.
*)

let rec product li =
  match li with 
  | [] -> 1
  | x::xs -> x * product xs
