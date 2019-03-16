let rootMeanSquare x y = 
  let xSquared = x *. x in 
  let ySquared = y *. y in 
  let sum = (xSquared +. ySquared) /. 2.0 in 
  sqrt sum

let rec fib n = 
  if n = 1 then 1
  else if n = 2 then 1
  else fib (n-1) + fib (n-2)

let fib_fast n =
  let rec h n pp p =
    if n = 1 then p 
    else h (n-1) p (pp+p)
  in 
    h n 0 1


(* determines if the day and month are a valid date *)
let isValidDate d m =
  if d < 1 || d > 31 then false
  else if m = 2 && d > 28 then false
  else if (m = 4 || m = 6 || m = 9 || m = 11) && d > 30 then false
  else true

let () = assert (isValidDate 0 4 = false)
let () = assert (isValidDate 1 4 = true)
let () = assert (isValidDate 31 1 = true)
let () = assert (isValidDate 31 2 = false)
let () = assert (isValidDate 31 3 = true)
let () = assert (isValidDate 28 2 = true)
let () = assert (isValidDate 30 9 = true)
let () = assert (isValidDate 31 9 = false)


(* create a symbol to calculate average between two floating point numbers *)
let (+/.) x y = (x +. y) /. 2.0

(* 
Write a function print_int_list : int list -> unit that prints its input list, one number per line. 

For example, print_int_list [1;2;3] should result in this output: 
1
2
3
*)
let rec print_int_list = function 
| [] -> () 
| h::t -> print_endline (string_of_int h); 
          print_int_list t

(*
Write a function print_int_list' : int list -> unit whose specification is the same as print_int_list. 
Do not use the keyword rec in your solution, but instead to use the List module function List.iter. 

Here is some code to get you started:
*)
let print_int_list' lst = 
  List.iter (fun x -> print_endline (string_of_int x)) lst
