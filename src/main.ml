let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2);;

Printf.printf "Answer: %d\n" (fib 5)


(* Inspired by Pierre Weiss, https://stackoverflow.com/a/9863069 *)
let explode s =
  let rec expl i l =
    match i with
    | -1 -> l
    |  x -> expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;



