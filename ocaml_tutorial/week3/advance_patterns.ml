(*
Advanced Patterns

Let's rewrite some pattern matching with advanced constructs.

  1. Factorize the pattern matching of function simplify using or-patterns. It should boil down to three cases.
  
  2. The only_small_lists function takes a list as input and returns this list only if it contains two or less elements, otherwise the empty list is returned. Rewrite this function using or-patterns and an as-pattern. It should boil down to two cases.
  
  3. Turn the third case of no_consecutive_repetition into two distinct cases, dropping the if construct in favor of a when clause.
*)

type e = EInt of int | EMul of e * e | EAdd of e * e

let simplify = function
  | EMul (EInt 1, e) -> e
  | EMul (e, EInt 1) -> e
  | EMul (EInt 0, e) -> EInt 0
  | EMul (e, EInt 0) -> EInt 0
  | EAdd (EInt 0, e) -> e
  | EAdd (e, EInt 0) -> e
  | e -> e

let only_small_lists = function
  | [] -> []
  | [x] -> [x]
  | [x;y] -> [x;y]
  | _ -> []

let rec no_consecutive_repetition = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: ys ->
      if x = y then
        no_consecutive_repetition (y :: ys)
      else
        x :: (no_consecutive_repetition (y :: ys))
