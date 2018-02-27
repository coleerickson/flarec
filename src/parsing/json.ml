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

(* part 1 *)
open Core
open Out_channel

let repeat s =
  let rec rep output = function
  | 0 -> output
  | i -> (rep (s ^ output) (i - 1))
  | _ -> raise (Invalid_argument "Repetition must occur at least 0 times") in
  rep ""

let rec print_regex outc = function
  | `Concat (r1, r2) -> print_regex outc r1; print_regex outc r2
  | `Group r -> output_string outc "("; print_regex outc r; output_string outc ")"
  | `Alternation (r1, r2) -> print_regex outc r1; output_string outc "|"; print_regex outc r2
  | `Repetition r -> output_string outc "("; print_regex outc r; output_string outc ")*"
  | `Wildcard   -> output_string outc "."
  | `Char c     -> printf "%c" c
  | `Empty      -> output_string outc " rempty! "

(* TODO implement list to string on regex *)
(* Fix so that it uses linked lists efficiently see https://stackoverflow.com/a/28969106*)
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
