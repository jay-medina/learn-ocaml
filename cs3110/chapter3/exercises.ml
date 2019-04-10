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

let rec concat li = 
  match li with 
  | [] -> ""
  | x::xs -> x ^ concat xs

let patterns (li: string list) =
  match li with
  | "bigred"::xs -> true
  | [x;y] -> true
  | [_;_;_;_] -> true
  | x::y::xs -> x = y
  | _ -> false

(* Library *)
let getFifth (li: 'a list) =
  if List.length li < 5 then 0
  else (List.nth li 4)

let sortDesc li = 
  List.rev (List.sort Pervasives.compare li)

(* library puzzle *) 
let getLastElement (li: 'a list) =
  let len = List.length li in 
  if len = 0 then None
  else List.nth_opt li (len - 1)

let anyZeros = 
  List.exists (fun x -> x = 0) 

(* take drop *)
let take (i: int) (li: 'a list) = 
    let rec looper nth rest acc = 
      match rest with
      | [] -> acc
      | x::xs -> 
        if nth = 0 then acc
        else looper (nth - 1) xs (acc @ [x])
    in
    if List.length li < i || i < 1 then li 
    else 
      looper i li []
    
let drop (i: int) (li: 'a list) = 
  let rec looper nth rest = 
      match rest with
      | [] -> []
      | x::xs -> 
        if nth = 0 then rest
        else looper (nth - 1) xs
  in
    if List.length li < i || i < 1 then [] 
    else 
      looper i li

let is_unimodal (li: int list) =
  let rec checkOrder comparer rest = 
    match rest with 
    | ([] | [_]) -> true
    | x::y::xs -> (comparer x y) && (checkOrder comparer (y::xs))
  in
  match li with
  | ([] | [_]) -> true
  | x::y::xs -> if x < y then checkOrder (fun x y -> x < y) li
                else checkOrder (fun x y -> x > y) li
