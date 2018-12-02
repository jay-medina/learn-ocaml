(*
Write a program that takes a floating point number representing temperature in Celsius from the standard input and converts it to Fahrenheit.
*)
let cel = read_float ()

let fahren = cel *. 9.0 /. 5.0 +. 32.0

let () = print_float fahren; print_newline() 

