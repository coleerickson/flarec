type nfa =
  | Transition of char * nfa
  | Alternative of nfa * nfa
  | Match;;

(* Inspired by Pierre Weiss, https://stackoverflow.com/a/9863069 *)
let explode s =
  let rec expl i l =
    match i with
    | -1 -> l
    |  x -> expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let x = Alternative ((Transition ('c', Match)), (Transition ('a', Match)));;

