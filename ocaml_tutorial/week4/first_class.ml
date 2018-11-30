(*
1. Write a function compose : ('a -> 'a) list -> ('a -> 'a) that takes as argument a list l of functions, and that returns the function that is the composition of the functions in l. For instance, compose [f;g;h] x = f (g (h x)). Or with concrete functions, compose [(fun x -> x+1);(fun x -> 3*x);(fun x -> x-1)] 2 = 4.
    
    
2. Write a function fixedpoint : (float -> float) -> float -> float -> float that takes a function f of type float -> float and two floating-point arguments start and delta. The function fixedpoint applies repetitively f to the result of its previous application, starting from start, until it reaches a value y where the difference between y and (f y) is smaller than delta. In that case it returns the value of y. For instance, fixedpoint cos 0. 0.001 yields approximately 0.739 (ref).
*)

type int_ff = int -> int

let compose_of_two f g = fun x -> f (g x)

let rec compose l =
  match l with 
    [] -> (fun x -> x)
  | [f] -> f
  | f::fs -> compose_of_two f (compose fs)

let rec fixedpoint f start delta =
  let y = f start in 
  let diff = abs_float (start -. y) in

  if diff < delta then start else (fixedpoint f y delta)

let ans = compose [(+) 1; ( * ) 2] ;;

let sol = print_int (ans 3) ;;
