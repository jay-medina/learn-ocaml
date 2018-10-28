(* 
  Puzzle : If you multiply my grand-son age by four, you know how old I am. 
  Now, if you exchange the two digits of our ages then you have to multiply by three 
  my age to get the age of my grand-son! 


  1. Write a function exchange of type int -> int that takes an integer x between 10 and 99 
     and returns an integer which is x whose digits have been exchanged. 
     For instance, exchange 73 = 37.

  2. Define is_valid_answer of type int * int -> bool such that 
     is_valid_answer (grand_father_age, grand_son_age) returns true if and only if 
     grand_father_age and grand_son_age verify the constraints of the puzzle.

  3. Write a function find : (int * int) -> (int * int) that takes a pair 
     (max_grand_father_age, min_grand_son_age) and returns a solution (grand_father_age, grand_son_age) 
     to the problem, where min_grand_son_age <= grand_son_age < grand_father_age <= max_grand_father_age 
     or (-1, -1) if there was no valid answer in the given range.

*)

let exchange k  = 
  let (x, y) = (k / 10, k mod 10) in y * 10 + x

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 = grand_father_age && ((exchange grand_father_age) * 3) = (exchange grand_son_age)

(* tests *)
is_valid_answer (91, 38) = false


let find (max_grand_father_age, min_grand_son_age) =
  let rec helper(gf, gs) =
    if (gs >= gf || gs > max_grand_father_age || gf > max_grand_father_age) then (-1, -1)
    else if (is_valid_answer(gf, gs)) then (gf, gs)
    else 
      let ans = helper((gs + 1) * 4, gs + 1) 
      in 
      if ans = (-1, -1) then helper(gf + 1, (gf + 1) / 4) 
      else ans
  in
    helper(min_grand_son_age * 4, min_grand_son_age) ;;

find (99, 10) = (72, 18);;
find (72, 21) = ( -1, -1) ;;
 find (71, 10) = (-1, -1) ;;
