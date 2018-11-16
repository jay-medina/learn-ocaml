(*
 Abstract syntax trees are a convenient way to represent a syntactic expression in a structured way.
 Let us consider arithmetic expressions formed by the following rules:

    1. an integer is an arithmetic expression ;
    2. if lhs and rhs are arithmetic expressions then lhs + rhs is an arithmetic expression;
    3. if lhs and rhs are arithmetic expressions then lhs * rhs is an arithmetic expression. 

    Such an expression can be represented by a value of type exp as defined in the given prelude (as well as the definition of 1 + 2 * 3 as an example). 

    1. Write the expression 2 * 2 + 3 * 3 in a variable my_example. 

    2. Write a function eval : exp -> int that computes the value of an arithmetic expression. The evaluation rules are:

    If the expression is an integer x, the evaluation is x.
    If the expression is lhs + rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x + y.
    If the expression is lhs * rhs and lhs evaluates to x and rhs evaluates to y, then the evaluation is x * y. 

    3. Write the reverse transformation of factorize, expand : exp -> exp, which turns an expression of the shape a * (b + c) into a * b + a * c. 
*)

type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

let my_example =
  let first = EMul (EInt 2, EInt 2) in
  let second = EMul (EInt 3, EInt 3) in 
  EAdd (first, second)

let rec eval e =
  match e with
  | EInt x -> x
  | EAdd (x, y) -> (eval x) + (eval y)
  | EMul (x, y) -> (eval x) * (eval y)

let factorize e =
  let factor a b c =
    EMul (a, (EAdd (b, c)))
  in
  match e with
  | EAdd ((EMul (a1, b1)), (EMul (a2, c1))) -> if a1 = a2 then factor a1 b1 c1 
                                               else e
  | _ -> e

let expand e =
  match e with 
  | EMul (a, (EAdd (b, c))) -> EAdd ((EMul (a, b)), (EMul (a, c)))
  | _ -> e

let simplify e =
  match e with 
  | EMul (EInt 0, e1) -> EInt 0
  | EMul (e1, EInt 0) -> EInt 0
  | EMul (EInt 1, e1) -> e1
  | EMul (e1, EInt 1) -> e1
  | EAdd (EInt 0, e1) -> e1
  | EAdd (e1, EInt 0) -> e1
  | _ -> e

(* ----- test ---- *)
