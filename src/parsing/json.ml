

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

(* TODO
type transition =
  epsilon
  char
  wildcard

*)

type nfa = {
  edges: (int * char option) list IntMap.t;
  start: int;
  final: int;
}

let list_map_union = IntMap.merge (fun key a b ->
  match a, b with
  | Some a, Some b -> Some (a @ b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None
)

let list_map_append k v m =
  let value_to_insert =
  match IntMap.find_opt k m with
  | Some existing -> existing @ v
  | None -> v in
  let m = IntMap.remove k m in
  IntMap.add k value_to_insert m

let rec regex_to_nfa r =
  let i = ref 0 in
  let next_i = fun () -> i := !i + 1; !i - 1 in
  let rec r_to_n r =
  match r with
  | `Char c ->
    let start = next_i () in
    let final = next_i () in
    let edges = IntMap.empty in
    let edges = list_map_append start [(final, Some c)] edges in
    { edges; start; final }
  | `Alternation (r1, r2) ->
    let start = next_i () in
    let final = next_i () in
    let nfa1 = r_to_n r1 in
    let nfa2 = r_to_n r2 in
    let edges = list_map_union nfa1.edges nfa2.edges in
    let edges = list_map_append start [(nfa1.start, None); (nfa2.start, None)] edges in
    let edges = list_map_append nfa1.final [(final, None)] edges in
    let edges = list_map_append nfa2.final [(final, None)] edges in
    { edges; start; final }
  | `Group r -> r_to_n r (* TODO submatch extraction *)
  | `Repetition r ->
    let {edges; start; final} = r_to_n r in
    let edges = list_map_append final [(start, None)] edges in
    { edges; start; final=start }
  | `Concat (r1, r2) ->
    let start = next_i () in
    let final = next_i () in
    let nfa1 = r_to_n r1 in
    let nfa2 = r_to_n r2 in
    let edges = list_map_union nfa1.edges nfa2.edges in
    let edges = list_map_append start [(nfa1.start, None)] edges in
    let edges = list_map_append nfa1.final [(nfa2.start, None)] edges in
    let edges = list_map_append nfa2.final [(final, None)] edges in
    { edges; start; final }
  | `Wildcard   ->
    let start = next_i () in
    let final = next_i () in
    let edges = IntMap.empty in
    let edges = list_map_append start [(final, Some '.')] edges in (* TODO actually represent wildcard transitions *)
    { edges; start; final }
  | `Empty      ->
    let start = next_i () in
    let final = next_i () in
    let edges = IntMap.empty in
    let edges = list_map_append start [(final, None)] edges in
    { edges; start; final }
  | _ -> raise (Invalid_argument "not implemented") in (*TODO remove *)
  r_to_n r

(* TODO Figure out where imports go properly *)
open Core
open Out_channel

(* TODO mark start and final on graph *)
let rec nfa_to_dot {edges; start; final } =
  "digraph G {\n" ^
    Printf.sprintf "\"%d\" [peripheries=2]\n" final ^
    IntMap.fold (fun k vs result ->
    result ^ (
      List.fold_left vs ~init:"" ~f:(fun output (v, label) ->
        output ^ Printf.sprintf "\"%d\" -> \"%d\" [label=%s]\n" k v (
          match label with
          | Some label_char -> String.make 1 label_char
          | None -> "None"
        )
      )
    )
  ) edges "" ^ "}"

let all_nodes {edges; start; final} =
  List.dedup (IntMap.fold (fun k vs result ->
    k::(List.map vs (fun (v, c) -> v)) @ result
  ) edges [start; final])

let rec nfa_fingers_to_dot {edges; start; final} fingers =
  "digraph G {\n" ^
    Printf.sprintf "\"%d\" [peripheries=2]\n" final ^
    List.fold_left fingers ~init:"" ~f:(fun output finger ->
      output ^ Printf.sprintf "\"%d\" [color=red]\n" finger
    ) ^
    IntMap.fold (fun k vs result ->
    result ^ (
      List.fold_left vs ~init:"" ~f:(fun output (v, label) ->
        output ^ Printf.sprintf "\"%d\" -> \"%d\" [label=%s]\n" k v (
          match label with
          | Some label_char -> String.make 1 label_char
          | None -> "None"
        )
      )
    )
  ) edges "" ^ "}"

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
  | `Regex r    -> printf "%s" (nfa_to_dot (regex_to_nfa r))
    (* | `Regex r    -> printf "%s" (regex_to_s r) *)
  | `Assoc obj  -> print_assoc outc obj
  | `List l     -> print_list outc l
  | `String s   -> printf "\"%s\"" s
  | `Int i      -> printf "%d" i
  | `Float x    -> printf "%f" x
  | `Bool true  -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `Null       -> output_string outc "null"

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

let explode s =
  let rec expl i l =
    match i with
    | -1 -> l
    |  x -> expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

(* https://gist.github.com/23Skidoo/1664038 *)
let sort_and_remove_duplicates l =
  let sl = List.sort compare l in
  let rec go l acc = match l with
    | [] -> List.rev acc
    | [x] -> List.rev (x::acc)
    | (x1::x2::xs) ->
      if x1 = x2
      then go (x2::xs) acc
      else go (x2::xs) (x1::acc)
in go sl []

(* Adapted from https://ocaml.org/learn/tutorials/file_manipulation.html#Writing *)
let save_nfa_to_file nfa fingers path =
  let oc = open_out path in
  fprintf oc "%s\n" (nfa_fingers_to_dot nfa fingers);
  close_out oc

let rec step c finger edges =
  match (IntMap.find_opt finger edges) with
  | None -> []
  | Some transitions ->
    (List.fold_left transitions ~init:[] ~f:(fun output transition ->
      (follow_transition transition c edges) @ output
    ))
and follow_transition (next, c_opt) c edges : int list =
  match c_opt with
  | Some transition_c -> if transition_c = c then [next] else []
  | None -> step c next edges

let rec is_final_reachable_by_epsilon fingers {edges; start; final} =
  List.exists fingers (fun finger -> finger = final) ||
  List.exists fingers (fun finger ->
    match IntMap.find_opt finger edges with
    | None -> false
    | Some transitions -> is_final_reachable_by_epsilon
      (List.dedup (List.fold_left transitions ~init:[] ~f:(fun output (next, c) ->
        if c = None then next::output else output
      )))
      {edges; start; final}
  )

let num_nfas_written = ref 0

let output_nfa nfa fingers x =
  let path = sprintf "%d-%d-nfa.dot" !num_nfas_written x in
  save_nfa_to_file nfa fingers path

let eval s {edges; start; final} =
  num_nfas_written := !num_nfas_written + 1;
  let rec eval_recurse s_chars fingers x =
    output_nfa {edges; start; final} fingers x;
    match s_chars with
    | [] -> is_final_reachable_by_epsilon fingers {edges; start; final}
    | c::rest -> eval_recurse rest (List.fold_left fingers ~init:[] ~f:(fun output finger ->
      (step c finger edges) @ output
    )) (x + 1) in
  eval_recurse (explode s) [start] 0
