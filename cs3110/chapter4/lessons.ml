let (|>>) = fun x f -> f x

let dp = 5 |>> string_of_int

let both f g x = (f x, g x)

let double x = 2 * x
let square x = x * x
let ds = both double square
let p = ds 3
