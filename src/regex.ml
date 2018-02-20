type regex = [
  | `Char of char
  | `Concat of regex * regex
  | `Wildcard
  | `Alternation of regex list
  | `Repetition of regex
  | `Optional of regex
  | `Empty
]

(* part 1 *)
open Core
open Out_channel

(* let output_value outc r = "hey" *)
 let rec output_value outc = function
  | `Char c          -> output_string outc c
  | _ -> output_string outc "help"
  (* | `Concat (r1, r2) -> printf "%s c&c %s" (output_value outc r1) (output_value outc r2)
  | `Wildcard        -> output_string outc "."
  | `Alternation r   -> printf " |(| %s |)| " (output_value outc r)
  | `Repetition r    -> printf "%s*" (output_value outc r)
  | `Optional r      -> printf "%s?" (output_value outc r)
  | `Empty           -> output_string outc " !^! " *)
  (* | `Assoc obj  -> print_assoc outc obj
  | `List l     -> print_list outc l
  | `String s   -> printf "\"%s\"" s
  | `Int i      -> printf "%d" i
  | `Float x    -> printf "%f" x
  | `Bool true  -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `Null       -> output_string outc "null" *)
(*
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
  output_string outc "]" *)
