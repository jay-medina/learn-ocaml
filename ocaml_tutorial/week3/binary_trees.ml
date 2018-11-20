(*
Binary Trees



A binary tree t, of the 'a bt type given in the prelude, is either an empty tree, or the root of a tree with a value and two children subtrees.

    1. Write a function height : 'a bt -> int that computes the height of a tree.
    2. A tree is balanced if, for all internal node n, its two subtrees have the same height. Write a function balanced : 'a bt -> bool that tells if a tree is balanced.
*)

type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

let max x y = if x > y then x else y

let rec height t = 
  match t with 
  | Empty -> 0
  | Node (l, _, r) -> 
      let l_height = height l in 
      let r_height = height r in 
      (max l_height r_height) + 1
                    

let rec balanced t  = 
  match t with
  | Empty -> true
  | Node (l, _, r) -> 
      (height l) = (height r)


(* tests *)
let b_tree = Node(Empty, 4 , Node(Empty, 5, Empty));;

let sample = (Node (Node (Node (Node (Node 
  (Empty, 3, Empty), 4, Node (Empty, 1, Empty)), 0, Node (Node 
                         (Empty, 2, Empty), 
                         2, 
                         Empty
                       )
                   ),
                   -4, 
                   Node (Empty, -3, Empty)
                  ),
    0,
    Node (Empty, 2,
     Node (Empty, -4, Node (Empty, 1, Node (Empty, -5, Empty))))))

let ans = print_int (height b_tree)

let isBalanced = print_bool (balanced b_tree)
