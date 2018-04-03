open Core
open Llvm

let context = global_context ()
let the_module = create_module context "regex matcher"
let builder = builder context
let int_type = i32_type context

(* let codegen s {edges; start; final} =
  let blocks = IntMap.map (fun vs -> List.map (fun v, transition ->
    match transition with
    | `Value transition_char -> String.make 1 transition_char
    | `Epsilon -> "epsilon"
    | `Any -> ". (wildcard)"
  ))

  IntMap.fold (fun k vs ->
    build_label
    List.fold_left vs ~init:"" ~f:(fun output (v, transition) ->
      output ^ Printf.sprintf "\"%d\" -> \"%d\" [label=\"%s\"]\n" k v (
        match transition with
        | `Value transition_char -> String.make 1 transition_char
        | `Epsilon -> "epsilon"
        | `Any -> ". (wildcard)"
      )
    )
  ) edges "" *)

let compile r =
  let args_types = Array.create 0 int_type in
  let f_type = function_type int_type args_types in
  let f = declare_function "matchplease" f_type the_module in
  let block = append_block context "entry" f in
  position_at_end block builder;
  let ret_val = const_int int_type 1337 in
  build_ret ret_val builder;
  let main_f = declare_function "main" f_type the_module in
  let main_block = append_block context "mainblock" main_f in
  position_at_end main_block builder;
  let call = build_call f (Array.create 0 (const_int int_type 42)) "calltmp" builder in
  build_ret call builder;

  (* Llvm_analysis.assert_valid_function f; *)
  let result = string_of_llmodule the_module in
  Printf.printf "%s\n" result;
  let oc = open_out "a.ll" in
  fprintf oc "%s\n" result;
  close_out oc
