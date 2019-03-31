(* Variants *)
(* create a day variant *)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

(* to access a variant, use pattern matching *)

let int_of_day d =
  match d with 
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7

let a = int_of_day Mon
