(* type flarex = [

] *)

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


(* TODO *)
(*
  let from_list = List.fold_left ~init:Map.empty ~f:(fun acc (k, v) ->
    Map.add k v acc
  ) *)

(*
module Make = functor (Ord: Map.OrderedType) -> struct
    module Map = Map.Make(Ord)
        include Map
      let add_extend k v m =
          let value_to_insert =
          match IntMap.find_opt k m with
          | Some existing -> existing @ v
          | None -> v in
          let m = IntMap.remove k m in
          IntMap.add k value_to_insert m
end
*)

(* https://stackoverflow.com/a/10132568 *)
module IntMap = Map.Make(struct type t = int let compare = compare end)

type supernode = int list
module IntListMap = Map.Make(struct type t = supernode let compare = compare end)

module StringMap = Map.Make(struct type t = string let compare = compare end)

type transition = [
  | `Epsilon
  | `Value of char
  | `Any
]

type nfa = {
  edges: (int * transition) list IntMap.t;
  start: int;
  final: int;
}

type dfa = {
  edges: (supernode * transition) list IntListMap.t;
  start: supernode;
  finals: supernode list;
}


let list_map_union = IntMap.merge (fun key a b ->
  match a, b with
  | Some a, Some b -> Some (a @ b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None
)


let intmap_add_extend k v m =
  let value_to_insert =
  match IntMap.find_opt k m with
  | Some existing -> existing @ v
  | None -> v in
  let m = IntMap.remove k m in
  IntMap.add k value_to_insert m

let intlistmap_add_extend k v m =
  let value_to_insert =
  match IntListMap.find_opt k m with
  | Some existing -> existing @ v
  | None -> v in
  let m = IntListMap.remove k m in
  IntListMap.add k value_to_insert m

let merge_nodes a b {edges; start; final} =
  let edges = match IntMap.find_opt b edges with
  | Some b_nexts -> intmap_add_extend a b_nexts edges
  | None -> edges in
  let edges = match IntMap.find_opt a edges with
  | Some a_nexts -> IntMap.add a (List.filter (fun (a_next, tr) -> a_next != b) a_nexts ) (IntMap.remove a edges)
  | None -> edges in
  let edges = IntMap.map (List.map (fun (v, tr) -> ((if v = b then a else v), tr))) edges in
  let edges = IntMap.remove b edges in
  let final = if b = final then a else final in
  let start = if b = start then a else start in
  {edges; start; final}

let rec regex_to_nfa r =
  let i = ref 0 in
  let next_i = fun () -> i := !i + 1; !i - 1 in
  let rec r_to_n r =
  match r with
  | `Char c ->
    let start = next_i () in
    let final = next_i () in
    let edges = IntMap.empty in
    let edges = intmap_add_extend start [(final, `Value c)] edges in
    { edges; start; final }
  | `Alternation (r1, r2) ->
    let start = next_i () in
    let final = next_i () in
    let nfa1 = r_to_n r1 in
    let nfa2 = r_to_n r2 in
    let edges = list_map_union nfa1.edges nfa2.edges in
    let edges = intmap_add_extend start [(nfa1.start, `Epsilon); (nfa2.start, `Epsilon)] edges in
    let edges = intmap_add_extend nfa1.final [(final, `Epsilon)] edges in
    let edges = intmap_add_extend nfa2.final [(final, `Epsilon)] edges in
    let {edges; start; final} = merge_nodes nfa1.final final {edges; start; final} in
    let {edges; start; final} = merge_nodes nfa2.final final {edges; start; final} in
    {edges; start; final}
  | `Group r -> r_to_n r (* TODO submatch extraction *)
  | `Repetition r ->
    let {edges; start; final} = r_to_n r in
    let edges = intmap_add_extend final [(start, `Epsilon)] edges in
    { edges; start; final=start }
  | `Concat (r1, r2) ->
    let nfa1 = r_to_n r1 in
    let nfa2 = r_to_n r2 in
    let edges = list_map_union nfa1.edges nfa2.edges in
    merge_nodes nfa1.final nfa2.start { edges; start=nfa1.start; final=nfa2.final }
  | `Wildcard   ->
    let start = next_i () in
    let final = next_i () in
    let edges = IntMap.empty in
    let edges = intmap_add_extend start [(final, `Any)] edges in (* TODO actually represent wildcard transitions *)
    { edges; start; final }
  | `Empty      ->
    let start = next_i () in
    let final = next_i () in
    let edges = IntMap.empty in
    let edges = intmap_add_extend start [(final, `Epsilon)] edges in
    { edges; start; final }
  | _ -> raise (Invalid_argument "not implemented") in (*TODO remove *)
  r_to_n r



(* https://stackoverflow.com/a/40143586 *)
let rec powerset = function
  | [] -> [[]]
  | x :: xs ->
     let ps = powerset xs in
     ps @ List.map (fun ss -> x :: ss) ps

(* TODO Figure out where imports go properly *)
open Core
(* open Out_channel *)

(* TODO mark start and final on graph *)
let rec nfa_to_dot {edges; start; final } =
  "digraph G {\noverlap = compress;ratio=0.7\n" ^
    Printf.sprintf "\"%d\" [peripheries=2]\n" final ^
    IntMap.fold (fun k vs result ->
    result ^ (
      List.fold_left vs ~init:"" ~f:(fun output (v, transition) ->
        output ^ Printf.sprintf "\"%d\" -> \"%d\" [label=\"%s\"]\n" k v (
          match transition with
          | `Value transition_char -> String.make 1 transition_char
          | `Epsilon -> "epsilon"
          | `Any -> ". (wildcard)"
        )
      )
    )
  ) edges "" ^ "}"

let intlist_to_s l = Sexp.to_string (List.sexp_of_t Int.sexp_of_t l)

let rec dfa_to_dot {edges; start; finals} =
  "digraph G {\noverlap = compress;ratio=0.7\n" ^
   (List.fold_left finals ~init:"" ~f:(fun output supernode ->
      output ^ (Printf.sprintf "\"%s\" [peripheries=2]\n" (intlist_to_s supernode))
    )) ^
    (IntListMap.fold (fun k vs result ->
      result ^ (
        List.fold_left vs ~init:"" ~f:(fun output (v, transition) ->
          output ^ Printf.sprintf "\"%s\" -> \"%s\" [label=\"%s\"]\n" (intlist_to_s k) (intlist_to_s v) (
            match transition with
            | `Value transition_char -> String.make 1 transition_char
            | `Epsilon -> "epsilon"
            | `Any -> ". (wildcard)"
          )
        )
      )
    ) edges "") ^
    "}"


let rec nfa_fingers_to_dot {edges; start; final} fingers =
  "digraph G {\noverlap = compress;ratio=0.7\n" ^
    Printf.sprintf "\"%d\" [peripheries=2]\n" final ^
    List.fold_left fingers ~init:"" ~f:(fun output finger ->
      output ^ Printf.sprintf "\"%d\" [color=red]\n" finger
    ) ^
    IntMap.fold (fun k vs result ->
    result ^ (
      List.fold_left vs ~init:"" ~f:(fun output (v, transition) ->
        output ^ Printf.sprintf "\"%d\" -> \"%d\" [label=\"%s\"]\n" k v (
          match transition with
          | `Value transition_char -> String.make 1 transition_char
          | `Epsilon -> "epsilon"
          | `Any -> ". (wildcard)"
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
let sort_and_remove_duplicates ~cmp l =
  let sl = List.sort ~cmp l in
  let rec go l acc = match l with
    | [] -> List.rev acc
    | [x] -> List.rev (x::acc)
    | (x1::x2::xs) ->
      if x1 = x2
      then go (x2::xs) acc
      else go (x2::xs) (x1::acc)
in go sl []

(* Adapted from https://ocaml.org/learn/tutorials/file_manipulation.html#Writing *)
let save_s_to_file s path =
  let oc = open_out path in
  fprintf oc "%s\n" s;
  close_out oc

let rec step c finger edges =
  match (IntMap.find_opt finger edges) with
  | None -> []
  | Some transitions ->
    (List.fold_left transitions ~init:[] ~f:(fun output transition ->
      (follow_transition transition c edges) @ output
    ))
and follow_transition (next, transition) c edges : int list =
  match transition with
  | `Value transition_char -> if transition_char = c then [next] else []
  | `Any -> [next]
  | `Epsilon -> step c next edges

let rec is_final_reachable_by_epsilon fingers {edges; start; final} =
  List.exists fingers (fun finger -> finger = final) ||
  List.exists fingers (fun finger ->
    match IntMap.find_opt finger edges with
    | None -> false
    | Some transitions -> is_final_reachable_by_epsilon
      (List.dedup (List.fold_left transitions ~init:[] ~f:(fun output (next, c) ->
        if c = `Epsilon then next::output else output
      )))
      {edges; start; final}
  ) (*TODO make robust to cycles of epsilon transitions *)

let eval s {edges; start; final} =
  let rec eval_recurse s_chars fingers =
    match s_chars with
    | [] -> is_final_reachable_by_epsilon fingers {edges; start; final}
    | c::rest -> eval_recurse rest (List.fold_left fingers ~init:[] ~f:(fun output finger ->
      (step c finger edges) @ output
    )) in
  eval_recurse (explode s) [start];;

let num_nfas_written = ref 0
let output_nfa nfa fingers x =
  let path = sprintf "%d-%d-nfa.dot" !num_nfas_written x in
  save_s_to_file (nfa_fingers_to_dot nfa fingers) path

let num_dfas_written = ref 0
let output_dfa d x =
  let path = sprintf "%d-%d-dfa.dot" !num_dfas_written x in
  save_s_to_file (dfa_to_dot d) path

let eval_debug s {edges; start; final} =
  num_nfas_written := !num_nfas_written + 1;
  let rec eval_recurse s_chars fingers x =
    output_nfa {edges; start; final} fingers x;
    match s_chars with
    | [] -> is_final_reachable_by_epsilon fingers {edges; start; final}
    | c::rest -> eval_recurse rest (List.fold_left fingers ~init:[] ~f:(fun output finger ->
      (step c finger edges) @ output
    )) (x + 1) in
  eval_recurse (explode s) [start] 0;;


(* Follow epsilon transitions from node x, returns a list of all the transitions to reachable nodes*)
let rec epsilon_closure x edges =
 match IntMap.find_opt x edges with
   | None -> [x]
   | Some transitions -> x::(List.concat_map transitions (fun (next, c) ->
       if c = `Epsilon then epsilon_closure next edges else []
     ))

let compare_transition a b =
  match (a, b) with
  | (`Value a_char, `Value b_char) -> compare a_char b_char
  (* char < epsilon < any *)
  | (`Value _, `Epsilon) -> -1
  | (`Epsilon, `Any) -> -1
  | (`Value _, `Any) -> -1
  (* any > epsilon > char *)
  | (`Any, `Epsilon) -> 1
  | (`Epsilon, `Value _) -> 1
  | (`Any, `Value _) -> 1
  (* equality cases *)
  | (`Any, `Any) -> 0
  | (`Epsilon, `Epsilon) -> 0

(* Compare first by transition. If the transitions are the same, then compare by node value *)
let compare_transition_pair (a_node, a_transition) (b_node, b_transition) =
  match compare_transition a_transition b_transition with
  | 0 -> compare a_node b_node
  | x -> x

(* TODO use sets instead of lists *)
(* NOTE just remember to stop when you reencounter a supernode *)
(* let nfa_to_dfa {edges; start; final} =
  let dfa_nodes = powerset (all_nodes {edges; start; final}) in (* TODO sort *)
  (* For every set in the powerset, create a node and compute transitions *)
  List.fold_left dfa_nodes ~init:IntListMap.empty ~f:(fun output node_set ->
    output |> IntListMap.add dfa_node_set (
      (* For every node within the set, find its transition nodes *)
      dfa_node_set |> List.fold_left ~init:[] ~f:(fun output node ->
          step node
        match IntMap.find_opt node edges with
        | Some transitions -> transitions |> List.map ~f:(fun (_, _) -> )
        (* If one of the nodes transitions to nothing, then just continue *)
        | None -> output
      )
    )
  ) *)

(* Takes a list and returns a list of lists, where the contained lists group consecutive elements of l that have the same value when f is applied to them *)
let group_list ~f l =
  match l with
  | [] -> []
  | x::rest -> List.fold_left rest ~init:[(f x, [x])] ~f:(fun ((fprev, group)::old) next ->
      let fnext = f next in
      if fnext = fprev then (fprev, next::group)::old else (fnext, [next])::(fprev, group)::old)


let compare_second (_, a) (_, b) = compare a b;;
let second_equal (_, a) (_, b) = a = b;;
let group_by ~f l = l
    |> List.sort ~cmp:(fun a b -> compare (f a) (f b))
    |> group_list ~f:f

let pluck_group desired_group keyed_groups =
    keyed_groups
    |> List.find_map ~f:(fun (group_id, group_vals) ->
        if group_id = desired_group then Some group_vals else None)
    |> Option.value ~default:[]

let all_supernodes edges =
  List.dedup (IntListMap.fold (fun k vs result ->
    k::(List.map vs (fun (v, c) -> v)) @ result
    ) edges [])

(* TODO ideally the conversion would remove character transitions that are redundant in the face of wildcard transitions *)
let nfa_to_dfa {edges; start; final} =
  let rec n2d supernodes dfa =
    match supernodes with
    | [] -> dfa
    | supernode::rest ->
      if IntListMap.mem supernode dfa then
        (* Don't repeat if the supernode has already been added to the DFA *)
        n2d rest dfa
      else
        (* Ensure that we put this supernode in the DFA as a key so that we don't recheck it*)
        let dfa = intlistmap_add_extend supernode [] dfa in
        (* Create an alias since the supernode is a list of the nodes from its originating NFA *)
        let subnodes = supernode in
        (* Look up each subnode in the NFA to find the nodes it transitions to. *)
        let transitions = subnodes
                          |> List.concat_map ~f:(fun subnode -> IntMap.find_opt subnode edges
                                                                |> Option.value ~default:[]) in
        (* Group the transitions by their transition characters *)
        let transition_groups = transitions
                                |> group_by ~f:snd
                                |> List.map ~f:(fun (group, vals) -> (group, List.map vals ~f:fst)) in
        (* Separate out the wildcard ones since we have to handle them differently *)
        let wildcard_destinations = List.concat_map ~f:(fun node -> epsilon_closure node edges) (pluck_group `Any transition_groups) in
        (* Remove the wildcard group from the other transition groups *)
        let transition_groups = List.filter ~f:(fun (transition, _) ->
            transition <> `Epsilon) transition_groups in
        (* We create one supernode for each transition character we can take, always carrying the wildcard transitions along *)
        let next_supernodes = List.map transition_groups ~f:(fun (transition, nodes) ->
            (transition, sort_and_remove_duplicates ~cmp:compare (
                (* In order to create the supernode, we have to get the epsilon closure of each subnode *)
                (List.concat_map ~f:(fun node -> epsilon_closure node edges) nodes) @ wildcard_destinations
            ))
        ) in
        (* We then add these supernodes to the DFA *)
        let dfa = List.fold_left next_supernodes ~init:dfa ~f:(fun dfa (transition, next_supernode) ->
            intlistmap_add_extend supernode [(next_supernode, transition)] dfa) in
        (* We now remove the transition information about the supernodes we transitioned to *)
        let next_supernodes = List.map next_supernodes ~f:snd in
        (* We also tack them onto the supernode queue for examination *)
        let supernodes = next_supernodes @ rest in
        n2d supernodes dfa in
  let start_supernode = epsilon_closure start edges in
  let superedges = n2d [start_supernode] IntListMap.empty in
  (* The final supernodes of the DFA are any supernodes that contain the final node of the NFA *)
  let final_supernodes = List.filter ~f:(fun subnodes ->
      List.exists ~f:(fun subnode -> subnode = final) subnodes
    ) (all_supernodes superedges) in
  {edges=superedges; start=start_supernode; finals=final_supernodes}

  (* IntMap.fold_left (fun k vs output_dfa ->
    List.fold_left vs ~init:output_dfa ~f:(fun output (v, transition) ->
      IntPairMap.add (k, v) Printf.sprintf "\"%d\" -> \"%d\" [label=\"%s\"]\n" k v (
        match transition with
        | `Value transition_char -> String.make 1 transition_char
        | `Epsilon -> "epsilon"
        | `Any -> ". (wildcard)"
      )
    )
  ) edges *)
