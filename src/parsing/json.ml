type regex = [
  | `Concat of regex * regex
  | `Group of regex
  | `Alternation of regex * regex
  | `Repetition of regex
  | `Char of char
  | `Wildcard
  | `Empty
]

type value = [
  | `Regex of regex
  | `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string
]

(* https://stackoverflow.com/a/10132568 *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
type nfa = {
  edges: (int * char option) list IntMap.t;
  source: int;
  sink: int;
  finals: int list;
};;

let list_map_union = IntMap.merge (fun key a b ->
  match a, b with
  | Some a, Some b -> Some (a @ b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None
);;

let list_map_append k v m =
  let value_to_insert =
  match IntMap.find_opt k m with
  | Some existing -> existing @ v
  | None -> v in
  let m = IntMap.remove k m in
  IntMap.add k value_to_insert m;;


(* let list_map_append x m = IntMap.update x (fun value ->
  match value with
  | Some existing -> existing @ x
  | None -> Some x
) m;; *)

let rec regex_to_nfa r =
  let i = ref 0 in
  let next_i = fun () -> i := !i + 1; !i - 1 in
  match r with
  | `Char c ->
    let source = next_i () in
    let sink = next_i () in
    let edges = IntMap.empty in
    let edges = list_map_append source [(sink, Some c)] edges in
    { edges; source; sink; finals = [] }
  | `Alternation (r1, r2) ->
    let source = next_i () in
    let sink = next_i () in
    let nfa1 = regex_to_nfa r1 in
    let nfa2 = regex_to_nfa r2 in
    let edges = list_map_union nfa1.edges nfa2.edges in
    let edges = list_map_append source [(nfa1.source, None); (nfa2.source, None)] edges in
    let edges = list_map_append nfa1.sink [(sink, None)] edges in
    let edges = list_map_append nfa2.sink [(sink, None)] edges in
    let edges = list_map_union nfa1.edges nfa2.edges in
    { edges; source; sink; finals = [] }
  | `Group r -> regex_to_nfa r (* TODO submatch extraction *)
  | `Repetition r ->
    let source = next_i () in
    let sink = next_i () in
    let nfa1 = regex_to_nfa r in
    let edges = nfa1.edges in
    let edges = list_map_append source [(nfa1.source, None)] edges in
    let edges = list_map_append nfa1.sink [(sink, None)] edges in
    let edges = list_map_append sink [(source, None)] edges in
    { edges; source; sink; finals = [] }
  | _ -> raise (Invalid_argument "not implemented");;
  (* | `Concat (r1, r2) ->
    let n1 = regex_to_nfa r1 in
    let n2 = regex_to_nfa r2 in
    let t = IntMap.empty;;
    let t = IntMap.add
  | `Wildcard   -> output_string outc "."
  | `Empty      -> output_string outc " rempty! ";; *)


(* part 1 *)
open Core
open Out_channel

let repeat s n =
  let rec rep output i =
  if i = 0 then output
  else if i > 0 then (rep (s ^ output) (i - 1))
  else raise (Invalid_argument "Repetition must occur at least 0 times") in
  rep "" n

let rec print_regex outc = function
  | `Concat (r1, r2) -> print_regex outc r1; print_regex outc r2
  | `Group r -> output_string outc "("; print_regex outc r; output_string outc ")"
  | `Alternation (r1, r2) -> print_regex outc r1; output_string outc "|"; print_regex outc r2
  | `Repetition r -> output_string outc "("; print_regex outc r; output_string outc ")*"
  | `Wildcard -> output_string outc "."
  | `Char c -> printf "%c" c
  | `Empty -> output_string outc " rempty! "

(* Fix so that it uses linked lists to efficiently do string concatenation see https://stackoverflow.com/a/28969106*)
let regex_to_s r =
  let rec r_to_s indent = function
  | `Concat (r1, r2) ->
    (repeat "  " indent) ^ "(concat\n" ^
                            (r_to_s (indent + 1) r1) ^
                            (r_to_s (indent + 1) r2) ^
    (repeat "  " indent) ^ ")\n"
  | `Group r ->
    (repeat "  " indent) ^ "(group\n" ^
                            (r_to_s (indent + 1) r) ^
    (repeat "  " indent) ^ ")\n"
  | `Alternation (r1, r2) ->
    (repeat "  " indent) ^ "(alternation\n" ^
                           (r_to_s (indent + 1) r1) ^
                           (r_to_s (indent + 1) r2) ^
    (repeat "  " indent) ^ ")\n"
  | `Repetition r ->
    (repeat "  " indent) ^ "(repetition\n" ^
                           (r_to_s (indent + 1) r) ^
    (repeat "  " indent) ^ ")\n"
  | `Wildcard   ->
    (repeat "  " indent) ^ "(wildcard)\n"
  | `Char c     ->
    (repeat "  " indent) ^ "'" ^ String.make 1 c ^ "'\n"
  | `Empty      ->
    (repeat "  " indent) ^ "(empty)\n" in
  r_to_s 0 r

  (* | _ -> (repeat "hey" 0) *)

let rec output_value outc = function
  | `Regex r    -> printf "%s" (regex_to_s r)
  | `Assoc obj  -> print_assoc outc obj
  | `List l     -> print_list outc l
  | `String s   -> printf "\"%s\"" s
  | `Int i      -> printf "%d" i
  | `Float x    -> printf "%f" x
  | `Bool true  -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `Null       -> output_string outc "null"

(* output_string outc "regex/"; print_regex outc r; output_string outc "/" *)

(* let eval s r =
  let rec eval_seq l r =
  let (hd::tl) = l in
  match r with
  | `Concat (r1, r2) -> eval_seq s r1 && eval_seq s r2
  | `Group r -> eval_seq r (* TODO Be sure to add capturing groups to output here *)
  | `Alternation (r1, r2) -> eval_seq s r1 || eval_seq r2
  | `Repetition r -> eval_seq s r || eval s (`Repetition r)
  | `Wildcard   -> if
  | `Char c     -> printf "%c" c
  | `Empty      -> output_string outc " rempty! " in
  eval_seq (explode s) r *)
(* this doesn't work because we need to be able to keep track of which states we're in at a given moment *)

(* We need to advance every state in every recursion *)

(* this function maps from states to lists of states, consuming one character of the input *)
(* we should deduplicate them afterward *)
(* let step c = function
  | `Concat (r1, r2) -> eval_seq s r1 && eval_seq s r2
  | `Group r -> eval_seq r (* TODO Be sure to add capturing groups to output here *)
  | `Alternation (r1, r2) -> eval_seq s r1 || eval_seq r2
  | `Repetition r -> eval_seq s r || eval s (`Repetition r)
  | `Wildcard   -> if
  | `Char c     -> printf "%c" c
  | `Empty      -> output_string outc " rempty! " in *)

(* let eval s r =
  let rec eval_seq l r =
  let (hd::tl) = l in
  match r with
  | `Concat (r1, r2) -> eval_seq s r1 && eval_seq s r2
  | `Group r -> eval_seq r (* TODO Be sure to add capturing groups to output here *)
  | `Alternation (r1, r2) -> eval_seq s r1 || eval_seq r2
  | `Repetition r -> eval_seq s r || eval s (`Repetition r)
  | `Wildcard   -> if
  | `Char c     -> printf "%c" c
  | `Empty      -> output_string outc " rempty! " in
  eval_seq (explode s) r *)


and print_assoc outc obj =
  output_string outc "{ ";
  let sep = ref "" in
  List.iter ~f:(fun (key, value) ->
      printf "%s\"%s\": %a" !sep key output_value value;
      sep := ",\n  ") obj;
  output_string outc " }"

and print_list outc arr =
  output_string outc "[";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc ", ";
      output_value outc v) arr;
  output_string outc "]"
