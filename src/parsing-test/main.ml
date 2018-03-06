open Core
open Test

let print_match_regex regex s =
  if match_regex regex s then Printf.printf "match\n" else Printf.printf "no match\n"


let () =
  Command.basic ~summary:"Match a string against a regex"
    Command.Spec.(empty +> anon ("regex" %: string) +> anon ("s" %: string))
    (fun regex s () -> print_match_regex regex s; ())
  |> Command.run
