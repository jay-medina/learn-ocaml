(*
An implementation of list with an efficient concatenation

Concatenating two standard OCaml lists takes a time proportional to the length of the first list. In this exercise, we implement a data structure for lists with a constant time concatenation.
The preludes gives a type 'a clist, which is either a single element of type 'a, the concatenation of two 'a clist or an empty 'a clist.

This representation of a list is not linear: it is a tree-like datastructure since the CApp constructor contains two values of type 'a clist.

The sequence of elements contained in a value of type 'a clist is obtained by a depth-first traversal of the tree. For instance, the example given in the prelude, of type int clist is a valid representation for the sequence [1;2;3;4]. 


  1. Write a function to_list : 'a clist -> 'a list which computes the 'a list that contains the same elements as the input 'a clist, in the same order.
  
  2. Write a function of_list : 'a list -> 'a clist which computes the 'a clist that contains the same elements as the input 'a list, in the same order.
  
  3. Write a function append : 'a clist -> 'a clist -> 'a clist such that:
      a. append CEmpty l = append l CEmpty = l
      b. append l1 l2 = CApp (l1, l2) otherwise
  
  4. Write a function hd : 'a clist -> 'a option that returns Some x where x is the first element of the input 'a clist, if it is not empty, and returns None otherwise.
  
  5. Write a function tl : 'a clist -> 'a clist option that returns Some l where l is the input sequence without its first element, if this input sequence is not empty, or returns None otherwise.
*)
type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))

let rec to_list l =
  match l with
  | CEmpty -> []
  | CSingle a -> [a]
  | CApp (x, y) -> (to_list x) @ (to_list y)

let rec of_list l =
  match l with
  | [] -> CEmpty
  | [a] -> CSingle a
  | x::xs -> CApp (of_list [x], of_list xs)

let append l1 l2 =
  match (l1, l2) with
  | (CEmpty, l2) -> l2
  | (l1, CEmpty) -> l1
  | (l1, l2) -> CApp(l1, l2) ;;

let rec hd l =
  match l with
  | CEmpty -> None
  | CSingle a -> Some a
  | CApp (x, y) -> let ans = hd x in 
                   if (ans = None) then hd y else ans

let tl l =
  match l with 
  | CEmpty -> None
  | CSingle a -> Some CEmpty
  | l1 -> let asList = to_list l1 in 
          match asList with 
          [] -> None
          | _ -> Some (of_list (List.tl asList))


let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l ;;

let print_int_option = function
  | None -> print_string ""
  | Some x -> print_int x 

(* let ex = print_list (to_list example);; *)

(* let ex2 = print_list (to_list (of_list [1;2;3;4])) ;; *)

let c_list1 = (of_list [5;2;2;2])
(* let c_list2 = (of_list [3;3;3;3])
let comb_clists = append c_list1 c_list2
let ex3 = print_list (to_list comb_clists) *)

(* let ex4 = print_int_option (hd c_list1) *)

let fail_ex1 = tl (CApp (CApp (CEmpty, CEmpty), CEmpty))
let fail_ex2 = tl (CApp (CEmpty, CEmpty))
