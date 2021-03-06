open Core
open Flare.Regex
open Flare.ParseTools

let () =
  Command.basic ~summary:"Match a string against a regex"
    Command.Spec.(empty +> anon ("regex" %: string) +> anon ("s" %: string))
    (fun regex s () -> print_match_regex regex s)
  |> Command.run
