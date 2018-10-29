(*
A queue is a standard FIFO data structure.

In this exercise, we implement a queue with a pair of two lists (front, back) such that front @ List.rev back represents the sequence of elements in the queue.

  1. Write a function is_empty : queue -> bool such that is_empty q is true if and only if q has no element.
    
  2. Write a function enqueue : int -> queue -> queue such that enqueue x q is the queue as q except that x is at the end of the queue.
  
  3. Write a function split : int list -> int list * int list such that split l = (front, back) where l = back @ List.rev front and the length of back and front is List.length l / 2 or List.length l / 2 + 1
    
  4. Write a function dequeue : queue -> int * queue such that dequeue q = (x, q') where x is the front element of the queue q and q' corresponds to remaining elements. This function assumes that q is non empty.
*)

type queue = int list * int list

let is_empty (front, back) =
  match (front, back) with
  | ([], []) -> true  
  | _ -> false ;;
  

let enqueue x (front, back) =
  match (front, back) with
  | ([], []) -> ([x], [])
  | ([y], []) -> ([y], [x])
  | (f, b) -> (f, x::b) ;; 

let split l =
  let half = (((List.length l) / 2) + 1) in

  let rec putHalf (f, b) li size =
    if size = 0 then (List.rev li, b) 
    else
      match ((f, b), li) with
      | ((_, b), []) -> ([], b)
      | ((_, b), x::xs) -> putHalf (f, b@[x]) xs (size - 1)
  in
    putHalf ([], []) l half ;;

let dequeue (front, back) =
  let emptyQueue = ([], []) in

  match (front, back) with
  | ([],[]) -> (0, emptyQueue)
  | ([x], []) -> (x, emptyQueue)
  | ([x], [y]) -> (x, ([y], []))
  | ([x], y::ys) -> (x, ([y], ys))
  | (x::xs, y) -> (x, (xs, y))
  | ([], [y]) -> (y, emptyQueue)
  | ([], y) -> let items = List.rev y in 
                 match items with 
                 | [] -> (0, emptyQueue)
                 | z::zs -> (z, ([], List.rev zs)) ;;

let (f, b) = split [3; 2; 1; 0];; (* ([0; 1], [3; 2] *)

