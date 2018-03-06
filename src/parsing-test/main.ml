open Core
open Test

let print_match_regex regex s =
  Printf.printf "regex: %s\ns: %s\n" regex s;
  Printf.printf "%s\n" (if match_regex regex s then "match" else "no match");
  ()


let () =
  Command.basic ~summary:"Match a string against a regex"
    Command.Spec.(empty +> anon ("regex" %: string) +> anon ("s" %: string))
    (fun regex s () -> print_match_regex regex s)
  |> Command.run
