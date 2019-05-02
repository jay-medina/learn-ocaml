let double x = 2 * x
let square x = x * x
let twice f x = f ( f x)
let quad = twice double
let fourth = twice square

(* repeat *)
let rec repeat f n x = 
  if n <= 0 then x 
  else if n = 1 then f x 
  else f (repeat f (n - 1) x)

let twice2 f = repeat f 2

(* clip *)
let clip n = 
  if n < 0 then 0 
  else if n > 10 then 10
  else n

let rec cliplist1 li = 
  match li with 
  | [] -> []
  | h::t -> (clip h) :: cliplist1 t

let cliplist2 =
  List.map clip 

(* sum cube odd *)
let (--) a b = 
  let rec create items n = 
    if n > b then items
    else create (n::items) (n + 1)
  in 
    List.rev (create [] a)

let sum_cube_odd n =
  let isOdd x = x mod 2 = 1 in 
  let li = List.filter isOdd (0 -- n) in 
  let cubed = List.map (fun x -> x * x * x) li in 
  List.fold_left (fun x y -> x + y) 0 cubed

let sum_cube_odd_pipe n =
  (0 -- n)
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left (fun x y -> x + y) 0

(* exists *)
let rec exists_rec fn li =
  match li with 
  | [] -> false
  | x::xs -> fn x || exists_rec fn xs

let exists_fold fn =
  List.fold_left (fun accum next -> accum || fn next) false

let exists_lib = 
  List.exists

(* library uncurried *)
let uncurried fn (a, b) = fn a b
let curry fn a b = fn (a, b)

let uncurried_nth = uncurried List.nth

let uncurried_append = uncurried List.append

let uncurried_compare = uncurried Char.compare

let uncurried_max = uncurried Pervasives.max

let curried_nth = curry uncurried_nth
let curried_append = curry uncurried_append
let curried_compare = curry uncurried_compare
let curried_max = curry uncurried_max

let original f g lst = (List.map (g |> f) lst)

type 'a tree = 
 | Leaf
 | Node of 'a * 'a tree * 'a tree

let rec tree_map fn tre = 
  match tre with
  | Leaf -> Leaf
  | Node (a, lft, right) -> Node (fn a, tree_map fn lft, tree_map fn right)

let add1 = tree_map (fun x -> x + 1)
