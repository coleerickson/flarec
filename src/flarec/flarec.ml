open Core
open Flare.Compile

let () =
  Command.basic ~summary:"Compile a Flare program"
    Command.Spec.(empty +> anon ("flarex" %: string) +> anon ("path" %: string))
    (fun flarex path () -> compile_flarex flarex path)
  |> Command.run
