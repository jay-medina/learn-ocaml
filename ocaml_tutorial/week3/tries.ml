(*
 The data structure called trie is very convenient to represent a dictionary whose keys are strings. It is space-efficient way while providing a very fast lookup function.

In this exercise, we will implement such a data structure, assuming that we want to associate integers to the strings of the dictionary.
Let us define a trie using two mutually defined types (given in the prelude):

    - trie which represents a trie, that is a tree whose root may contain an integer and whose children are indexed by characters ;
    
    - char_to_children which implements the associative data structure whose keys are characters and whose values are trie (childrens). 

As a trade-off between speed and memory consumption, we choose an associative list to represent the association between characters and children. 

The prelude also gives examples of empty trie and of another one that contains the following pairs (key, value): 
[("A", 15); ("to", 7); ("tea", 3);("ted", 4); ("ten", 12); ("i", 11); ("in", 5); ("inn", 9)]

1. Write a function children_from_char : char_to_children -> char -> trie option such that

    1. children_from_char m c = Some t if (c, t) is the first pair in m with c as a first component ;
    2. children_from_char m c = None if no such pair exists in m. 

2. Write a function update_children : char_to_children -> char -> trie -> char_to_children such that

    1. children_from_char (update_children m c t) c = Some t ;
    2. children_from_char (update_children m c t) c' = children_from_char m c' for c <> c';
    3. If children_from_char m c = Some t then List.length (update_children m c t') = List.length m. 

3.  Write a function lookup : trie -> string -> int option such that lookup trie w = Some i if i is the value of the key w in trie and lookup trie w = None if w is not a key of trie.
To look for a key in a trie, iterate over the characters of the key from left to right. Given the current character c and the current node of the trie n, look for the children n for character c. If such a children exists, continue with that trie and the remainder of the key. If no such children exists, the key is not in the trie. When the characters of the key are entirely consumed, look at the root of the current trie. If there is an integer, this is the value you are looking for. If there is no integer, the key not in the trie. 
*)
type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	     [('i', 
         Trie (Some 11, 
              [('n', 
                Trie (Some 5, 
                      [('n', 
                       Trie (Some 9, [])
                       )]
                     )
               )]
              )
        );
	      ('t',
	       Trie (None,
		           [('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let rec children_from_char m c =
  match m with
  | [] -> None
  | (ch, t)::rest -> if ch = c then Some t else (children_from_char rest c)
  

let rec update_children m c t =
  match m with
  | [] -> [(c, t)]
  | (ch, t1)::rest -> if ch = c then (c, t)::rest 
                     else (ch,t1)::(update_children rest c t)

let rec lookup trie w =
  let rec isInCharList ch_list =
    match ch_list with
    | [] -> false
    | (ch, _)::xs -> ch = w || (isInCharList xs)
  in
  let rec looper trie =
    match trie with 
    | Trie (opt, []) -> None
    | Trie (opt, (_, tr)::xs) -> if (lookup tr w) = None then (looper (Trie(opt, xs))) else opt 
  in
  match trie with
  | Trie (opt, li) -> if (isInCharList li) then opt else (looper trie)


let ans = (lookup example 'n')

let insert trie w v =
  "Replace this string with your implementation." ;;
