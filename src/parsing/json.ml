type regex = [
  | `Concat of regex * regex
  | `Group of regex
  | `Repetition of regex
  | `Char of char
  | `RString of string
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

let rec print_regex outc = function
  | `Concat (r1, r2) -> print_regex outc r1; print_regex outc r2
  | `Group r -> output_string outc " g(g "; print_regex outc r; output_string outc " g)g "
  | `Repetition r -> output_string outc " ("; print_regex outc r; output_string outc ")* "
  | `RString s    -> printf "regex/%s/" s
  | `Char c     -> printf "char[%c]" c
  | `Empty      -> output_string outc " rempty! "

let rec output_value outc = function
  | `Regex r    -> output_string outc "regex/"; print_regex outc r; output_string outc "/"
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
