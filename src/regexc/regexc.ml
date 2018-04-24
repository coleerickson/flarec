open Core
open Compile

let () =
  Command.basic ~summary:"Compile a regex into a regex-matching program"
    Command.Spec.(empty +> anon ("regex" %: string) +> anon ("path" %: string))
    (fun regex path () -> compile regex path)
  |> Command.run
