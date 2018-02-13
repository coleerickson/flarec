(* Inspired by Pierre Weiss, https://stackoverflow.com/a/9863069 *)
let explode s =
  let rec expl i l =
    match i with
    | -1 -> l
    |  x -> expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

type
  transition = {c: char; mutable next: nfa} and
  alternative = {mutable first: nfa; mutable second: nfa} and
  nfa =
    | Transition of transition
    | Alternative of alternative
    | Match;;

let x = Alternative {
  first = Transition { c = 'a'; next = Match };
  second = Match
}

(* Inspired by https://ocaml.org/learn/tutorials/99problems.html#Working-with-lists *)
type 'a nestedList = One of 'a | Many of 'a nestedList list;;
let a: char nestedList = Many [ One 'h'; One 'i'; Many [ One 'i'; One 'm']; One 'o';One 'k'];;

let concat = function
  | One (x),   One (y)   -> Many ([One x; One y])
  | One (x),   Many (ys) -> Many (One x :: ys)
  | Many (xs), One (y)   -> Many (xs @ [One y])
  | Many (xs), Many (ys) -> Many (xs @ ys);;

exception Unmatched_parens;;

let rec parse_parens' (chars: char list) (output: char nestedList) (paren_count: int) =
  match chars with
  | '(' :: rest -> Many [parse_parens' rest output (paren_count + 1)]
  | ')' :: rest -> Many [output; parse_parens' rest (Many []) (paren_count - 1)]
  | c :: rest -> concat (One c, parse_parens' rest output paren_count)
  | [] -> if paren_count != 0 then raise Unmatched_parens else output;;

let parse_parens s =
  parse_parens' (explode s) (Many []) 0;;

type regex =
  | Alternative of regex * regex
  | Concatenation of regex * regex
  | Character of char
  | Empty;;

let parse_regex s =
  let rec parse_regex' chars =
    match chars with
    | c :: rest -> Concatenation (Character c, parse_regex' rest)
    | [] -> Empty in
  parse_regex' (explode s);;

parse_regex "hey";;

Printf.printf "first\n";;
parse_parens "";;
parse_parens "a";;
(*
Printf.printf "Trying '('\n";;
parse_parens "(";;
*)
(*
Printf.printf "Trying ')'\n";;
parse_parens ")";;
*)
Printf.printf "Trying 'a(b)c'\n";;
parse_parens "a(b)c";;
