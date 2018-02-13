(* Inspired by Pierre Weiss, https://stackoverflow.com/a/9863069 *)
let explode s =
  let rec expl i l =
    match i with
    | -1 -> l
    |  x -> expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

type
  transition = {c: char; next: nfa} and
  alternative = {first: nfa; second: nfa} and
  nfa =
    | Transition of transition
    | Alternative of alternative
    | Match;;

let x = Alternative {
  first = Transition { c = 'a'; next = Match };
  second = Match
}

