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

let powerset (li: int list) = 
  let createNewList (x: int) = 
    List.map (fun y -> List.concat [y; [x]])
  in
  let addNextSetToCurrent (total: int list list) (nextSets: int list list) = 
    List.concat [total; nextSets]
  in
  let rec loop (total: int list list) (currentLI: int list) = 
    match currentLI with
    | [] -> total
    | x::xs -> let nextSets = createNewList x total in
               let nextTotal = addNextSetToCurrent total nextSets in 
               (loop nextTotal xs)
               
  in
    loop [[]] li
  
  (* student *)
type student = { first_name: string; last_name: string; gpa: float }

let s: student = {
  first_name = "Jose";
  last_name = "Medina";
  gpa = 3.4;
}

let getStudentsName (s: student) = s.first_name ^ " " ^ s.last_name

let createStudent first last gpa = {
  first_name = first; 
  last_name = last;
  gpa = gpa;
}

(* pokerecord *)
type poketype = Normal | Fire | Water

type pokemon = {
  name: string;
  hp: int;
  ptype: poketype;
}

let charizard: pokemon = {
  name= "charizard";
  hp= 78;
  ptype= Fire;
}

let metapod: pokemon = {
  name= "metapod";
  hp=50;
  ptype=Normal;
}

(* safe hd and tl *)
let safe_hd li = 
  match li with 
  | [] -> None
  | x::_ -> Some x

let safe_tl li = 
  match li with 
  | [] -> None
  | _::xs -> Some xs

(* pokefun  *)
let rec max_hp pokemon_list = 
  let rec findBiggest biggest rest = 
    match rest with 
    | [] -> biggest
    | p::ps -> if biggest.hp > p.hp then (findBiggest biggest ps)
               else (findBiggest p ps)
  in
    match pokemon_list with 
    | [] -> None
    | p::ps -> Some (findBiggest p ps)

(* date before *)
(* year * month * day *)
type date = int * int * int

let is_before (date1: date) (date2: date) = 
 let (y1, m1, d1) = date1 in
 let (y2, m2, d2) = date2 in
 if (y1 < y2) then true
 else if (y1 = y2 && m1 < m2) then true
 else if (y1 = y2 && m1 = m2) then d1 = d2
 else false

(* earliest date *)
let earliest (date_list: date list) = 
  let rec loop selected rest = 
    match rest with 
    | [] -> selected
    | d::ds -> if (is_before selected d) then (loop selected ds)
               else (loop d ds)
  in
  match date_list with
  | [] -> None
  | d::ds -> Some (loop d ds)

(* cards *)
type suit = Hearts | Diamonds | Spades | Clubs
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type card = { suit: suit; rank: rank }

let c1: card = { suit = Clubs; rank = Ace }
let c1: card = { suit = Hearts; rank = Queen }
let c1: card = { suit = Diamonds; rank = Two }
let c1: card = { suit = Spades; rank = Seven }

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x:int) = 
  if x < 0 then Neg
  else if x = 0 then Zero
  else Pos

let quadrant : int*int -> quad option = fun(x,y) ->
  match (sign x, sign y) with
  | (Pos, Pos) -> Some I
  | (Neg, Pos) -> Some II
  | (Neg, Neg) -> Some III
  | (Pos, Neg) -> Some IV
  | _ -> None

(* depth *) 
type 'a tree = 
| Leaf 
| Node of 'a * 'a tree * 'a tree

let rec depth tr = 
  match tr with 
  | Leaf -> 0
  | Node (v, left, right) ->
        let topDepth = (max (depth left) (depth right)) in 
        1 + topDepth
          
(* list max *)
let rec list_max (li: int list) = 
  match li with 
  | [] -> raise (Failure "list_max")
  | [x] -> x
  | x::xs -> let tempMax = list_max xs in 
             if x > tempMax then x else tempMax

let rec list_max_string (li: int list) = 
  match li with 
  | [] -> raise (Failure "list_max")
  | [x] -> string_of_int x
  | x::xs -> string_of_int (list_max li)
