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
