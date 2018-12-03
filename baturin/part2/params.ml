(* operators are funcs *)
let four = (+) 2 2

(* creator your own operator *)
let (^^) x y = x ^ x ^ y ^ y

let s = "foo" ^^ "bar" (* foofoobarbar *)

let greet ~greeting ~name = Printf.printf "%s %s!\n" greeting name

let () = greet ~name:"world" ~greeting:"hello"

let () = greet "hello" "world"

let greet ?(greeting="hello") name = Printf.printf "%s %s!\n" greeting name

let () = greet "world" ~greeting:"hi"

let join_strings ?(s3=" ") s1 s2  = Printf.printf "%s%s%s\n" s1 s3 s2 
