open Core
open Llvm
open ParseTools
open Regex

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
  (* Do the parsing and DFA conversion immediately *)
  let d = parse_regex_to_dfa_exn r in
  (* Declare printf for debugging *)
  let printf = declare_function "printf" (var_arg_function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ])) the_module in
  let args_types = Array.create 0 int_type in
  let f_type = function_type int_type args_types in
  let f = declare_function "matchplease" f_type the_module in
  let block = append_block context "entry" f in
  position_at_end block builder;
  let ret_val = const_int int_type 1337 in
  let _ = build_ret ret_val builder in
  let main_f_type = function_type int_type (Array.of_list [ int_type; pointer_type (pointer_type (i8_type context)) ]) in
  let main_f = declare_function "main" main_f_type the_module in
  let main_f_params = params main_f in
  let argc = main_f_params.(0) in
  let argv = main_f_params.(1) in
  set_value_name "argc" argc;
  set_value_name "argv" argv;
  
  let main_block = append_block context "mainblock" main_f in
  position_at_end main_block builder;

  let x1 = build_alloca (i32_type context) "1" builder in
  let x2 = build_alloca (pointer_type (pointer_type (i8_type context))) "2" builder in
  let first_argv_argument = build_alloca (pointer_type (i8_type context)) "first_argv_argument" builder in
  let first_char = build_alloca (i8_type context) "first_char" builder in
  let _ = build_store argc x1 builder in
  let _ = build_store argv x2 builder in 
  let x3 = build_load x2 "3" builder in
  let x4 = build_in_bounds_gep x3 (Array.of_list [ (const_int (i64_type context) 1) ]) "4" builder in
  let x5 = build_load x4 "5" builder in
  let _ = build_store x5 first_argv_argument builder in
  let x6 = build_load first_argv_argument "6" builder in 
  
  let x7 = build_in_bounds_gep x6 (Array.of_list [ (const_int (i64_type context) 0) ]) "7" builder in
  let x8 = build_load x7 "8" builder in
  let _ = build_store x8 first_char builder in
  let x9 = build_load first_char "9" builder in
  let x10 = build_sext x9 (i32_type context) "10" builder in
  let format_string = (build_global_string "The char is %c\n" "formatstring" builder) in 
  let format_string2 = const_in_bounds_gep format_string (Array.of_list [ const_int (i32_type context) 0; const_int (i32_type context) 0 ]) in 
  let x11 = build_call printf (Array.of_list [ format_string2; x10 ]) "11" builder in
  

  let fail_block = append_block context "fail-block" main_f in 
  position_at_end fail_block builder;
  build_ret (const_int (i32_type context) 0) builder;
  position_at_end main_block builder;

  let {edges; start; finals} = d in
  let compile_node_with_block a transitions a_node_block = 
    (* We will jump to this a_block whenever we transition to node a *)
    let a_node_block_name = intlist_to_s a in
    let if_done_block = append_block context (a_node_block_name ^ "-if-done") main_f in
    let transitions_block = append_block context (a_node_block_name ^ "-transitions") main_f in
    position_at_end a_node_block builder;
    let is_input_char_zero = build_icmp Icmp.Eq x9 (const_int (i8_type context) 0) "is-input-char-zero" builder in
    build_cond_br is_input_char_zero if_done_block transitions_block builder;
    List.map transitions (fun (b, transition) ->
        
        match transition with
        | `Value c -> ()
        | `Any -> ()
        | `Epsilon -> ()
        ;
    );
    (* gotta make this fail block appear as the else condition of the last if-else *) 
    build_br fail_block builder; () in

  let compile_node a transitions =
    let a_node_block_name = intlist_to_s a in
    let a_node_block = append_block context a_node_block_name main_f in
    compile_node_with_block a transitions a_node_block in
  
  let start_block_name = intlist_to_s start in
  let start_block = append_block context start_block_name main_f in
  compile_node_with_block start (IntListMap.find start edges) start_block;
  position_at_end main_block builder;
  build_br start_block builder;
  IntListMap.mapi compile_node (IntListMap.filter (fun k _ -> k <> start) edges);

    (* let call = build_call f (Array.of_list []) "calltmp" builder in build_ret call builder; *) 
  (* Llvm_analysis.assert_valid_function f; *)
  let result = string_of_llmodule the_module in
  Printf.printf "%s\n" result;
  let oc = open_out "a.ll" in
  fprintf oc "%s\n" result;
  close_out oc
