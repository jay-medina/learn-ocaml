let a = if (2 = 2) then 0 else 1 

let isPrime x = 
  let rec hasNoOtherDivisors y next =
    next > (y / 2) || (y mod next <> 0 && hasNoOtherDivisors y (next + 2))
  in
    if x = 1 then false 
    else if x = 2 then true
    else if x mod 2 = 0 then false
    else hasNoOtherDivisors x 3

let isVowel ch = 
 match ch with 
 | 'a' | 'e' | 'i' | 'o' | 'u' -> true
 | _ -> false
