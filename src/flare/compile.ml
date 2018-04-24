open Core
open Llvm
open ParseTools
open Regex

let context = global_context ()
let the_module = create_module context "regex matcher"
let builder = builder context
let int_type = i32_type context

let compile_regex r output_path =
	(* Do the parsing and DFA conversion immediately *)
	let d = parse_regex_to_dfa_exn r in

  (* DEBUG *)
  (* output_nfa (parse_regex_to_nfa_exn r) [] 0;
  output_dfa d 0; *)

	(* Declare printf for debugging *)
	let printf = declare_function "printf" (var_arg_function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ])) the_module in

	(* let main_f_type = function_type int_type (Array.of_list [ int_type; pointer_type (pointer_type (i8_type context)) ]) in
	let main_f = declare_function "main" main_f_type the_module in
	let main_f_params = params main_f in
	let argc = main_f_params.(0) in
	let argv = main_f_params.(1) in
	set_value_name "argc" argc;
	set_value_name "argv" argv; *)

  let matchregex_f_type = function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ]) in
  let matchregex_f = declare_function "matchregex" matchregex_f_type the_module in
  let matchregex_f_params = params matchregex_f in
  let matchregex_input_ptr_param = matchregex_f_params.(0) in
  set_value_name "matchregex_input_ptr_param" matchregex_input_ptr_param;

  (* Start creating the body of matchregex *)
  let matchregex_block = append_block context "matchregexblock" matchregex_f in
  position_at_end matchregex_block builder;
  let format_string = build_global_string "%20s --'%c'->\n" "formatstring" builder in

  (* Start by storing the parameter *)
  let matchregex_input_ptr = build_alloca (pointer_type (i8_type context)) "matchregex_input_ptr" builder in
  let _ = build_store matchregex_input_ptr_param matchregex_input_ptr builder in

  (* This is a utility function for advancing our pointer into the string *)
  let build_increment_input_ptr builder =
    let input_ptr_value = build_load matchregex_input_ptr "input_ptr_value" builder in
    let incremented_ptr_value = build_in_bounds_gep input_ptr_value (Array.of_list [ (const_int (i64_type context) 1) ]) "incremented_ptr_value" builder in
    build_store incremented_ptr_value matchregex_input_ptr builder; in

	let {edges; start; finals} = d in
	let node_block_map = List.fold_left (all_supernodes edges) ~init:IntListMap.empty ~f:(fun acc supernode ->
		IntListMap.add supernode (append_block context (intlist_to_s supernode) matchregex_f) acc
	) in

	(* TODO entry point, index advancement *)
	let _ = build_br (IntListMap.find start node_block_map) builder in

  (* Add a block for failures to the end, then return to the starting block for matchregex *)
	let fail_block = append_block context "fail-block" matchregex_f in
	position_at_end fail_block builder;
	build_ret (const_int (i32_type context) 0) builder;
	position_at_end matchregex_block builder;

	IntListMap.iter (fun node block ->
		(* Start by checking if we're done processing the string, in which case we'll just return whether or not this is an accepting state *)
		position_at_end block builder;

    (* Retrieve the next character on the input *)
    let input_ptr_value = build_load matchregex_input_ptr "input_ptr_value" builder in
    let next_char = build_load input_ptr_value "next_char" builder in

    (* DEBUG print the transition *)
    (* let next_char_32 = build_sext next_char (i32_type context) "next_char_32" builder in
    let format_string3 = const_in_bounds_gep format_string (Array.of_list [ const_int (i32_type context) 0; const_int (i32_type context) 0 ]) in
    let _ = build_call printf (Array.of_list [ format_string3; (build_global_string (intlist_to_s node) (intlist_to_s node) builder); next_char_32 ]) "printf_result" builder in *)

		let is_input_char_zero = build_icmp Icmp.Eq next_char (const_int (i8_type context) 0) "is-input-char-zero" builder in
		(* Note that we still need to build the conditional jump that returns based on the output of this test, but we'll have to return to that after creating the the other blocks. *)

		let if_done_block = append_block context ((intlist_to_s node) ^ "-if-done") matchregex_f in
		(* When we're done, return true if the node is a final node *)
		let is_final_node_as_int = if List.mem ~equal:(=) finals node then 1 else 0 in
		position_at_end if_done_block builder;
		build_ret (const_int (i32_type context) is_final_node_as_int) builder;

		(* Now we create the extended if-else-if to handle each transition *)
		let transitions = edges
      |> IntListMap.find_opt node
      |> Option.value ~default:[]
      (* Sort order is important so that we take any character transitions before any wildcard transitions *)
      |> List.sort ~cmp:compare_transition_pair
      |> List.rev in
		let transitions_block = List.fold_left transitions
      ~init:fail_block (* This is the final else case of all the transitions -- if there is no matching transition, we "fail", i.e., report that there is no match *)
      ~f:(fun else_block (dest_node, transition) ->
        let dest_block = IntListMap.find dest_node node_block_map in

        (* Before jumping to the destination, advance to the next character on the input *)
        let pre_dest_block = append_block context ((intlist_to_s node) ^ "-to-" ^ (intlist_to_s node) ^ "-pre-dest") matchregex_f in
        position_at_end pre_dest_block builder;
        build_increment_input_ptr builder;
        build_br dest_block builder;

        let transition_block = append_block context ((intlist_to_s node) ^ "-to-" ^ (intlist_to_s node)) matchregex_f in
  			position_at_end transition_block builder;

  			(match transition with
  			| `Value c ->
  					let transition_test = build_icmp Icmp.Eq next_char (const_int (i8_type context) (int_of_char c)) "does-next-char-match" builder in
  					build_cond_br transition_test pre_dest_block else_block builder
  			| `Any -> build_br pre_dest_block builder);
  			(* We return the transition block so that it can be the else case of the another transition test *)
  			transition_block
  		) in

		position_at_end block builder;
		build_cond_br is_input_char_zero if_done_block transitions_block builder; ()
	) node_block_map;

	let result = string_of_llmodule the_module in

  (* DEBUG print *)
	(* Printf.printf "%s\n" result; *)

	let oc = open_out output_path in
	fprintf oc "%s\n" result;
	close_out oc


let compile_flarex flarex output_path =
	(* Do the parsing and DFA conversion immediately *)
	let f = parse_flarex_exn flarex in

  (* DEBUG *)
  (* output_nfa (parse_regex_to_nfa_exn r) [] 0;
  output_dfa d 0; *)

	(* Declare printf for debugging *)
	let printf = declare_function "printf" (var_arg_function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ])) the_module in

	(* let main_f_type = function_type int_type (Array.of_list [ int_type; pointer_type (pointer_type (i8_type context)) ]) in
	let main_f = declare_function "main" main_f_type the_module in
	let main_f_params = params main_f in
	let argc = main_f_params.(0) in
	let argv = main_f_params.(1) in
	set_value_name "argc" argc;
	set_value_name "argv" argv; *)

  let matchregex_f_type = function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ]) in
  let matchregex_f = declare_function "matchregex" matchregex_f_type the_module in
  let matchregex_f_params = params matchregex_f in
  let matchregex_input_ptr_param = matchregex_f_params.(0) in
  set_value_name "matchregex_input_ptr_param" matchregex_input_ptr_param;

  (* Start creating the body of matchregex *)
  let matchregex_block = append_block context "matchregexblock" matchregex_f in
  position_at_end matchregex_block builder;
  let format_string = build_global_string "%20s --'%c'->\n" "formatstring" builder in

  (* Start by storing the parameter *)
  let matchregex_input_ptr = build_alloca (pointer_type (i8_type context)) "matchregex_input_ptr" builder in
  let _ = build_store matchregex_input_ptr_param matchregex_input_ptr builder in

  (* This is a utility function for advancing our pointer into the string *)
  let build_increment_input_ptr builder =
    let input_ptr_value = build_load matchregex_input_ptr "input_ptr_value" builder in
    let incremented_ptr_value = build_in_bounds_gep input_ptr_value (Array.of_list [ (const_int (i64_type context) 1) ]) "incremented_ptr_value" builder in
    build_store incremented_ptr_value matchregex_input_ptr builder; in

	let {edges; start; finals} = d in
	let node_block_map = List.fold_left (all_supernodes edges) ~init:IntListMap.empty ~f:(fun acc supernode ->
		IntListMap.add supernode (append_block context (intlist_to_s supernode) matchregex_f) acc
	) in

	(* TODO entry point, index advancement *)
	let _ = build_br (IntListMap.find start node_block_map) builder in

  (* Add a block for failures to the end, then return to the starting block for matchregex *)
	let fail_block = append_block context "fail-block" matchregex_f in
	position_at_end fail_block builder;
	build_ret (const_int (i32_type context) 0) builder;
	position_at_end matchregex_block builder;

	IntListMap.iter (fun node block ->
		(* Start by checking if we're done processing the string, in which case we'll just return whether or not this is an accepting state *)
		position_at_end block builder;

    (* Retrieve the next character on the input *)
    let input_ptr_value = build_load matchregex_input_ptr "input_ptr_value" builder in
    let next_char = build_load input_ptr_value "next_char" builder in

    (* DEBUG print the transition *)
    (* let next_char_32 = build_sext next_char (i32_type context) "next_char_32" builder in
    let format_string3 = const_in_bounds_gep format_string (Array.of_list [ const_int (i32_type context) 0; const_int (i32_type context) 0 ]) in
    let _ = build_call printf (Array.of_list [ format_string3; (build_global_string (intlist_to_s node) (intlist_to_s node) builder); next_char_32 ]) "printf_result" builder in *)

		let is_input_char_zero = build_icmp Icmp.Eq next_char (const_int (i8_type context) 0) "is-input-char-zero" builder in
		(* Note that we still need to build the conditional jump that returns based on the output of this test, but we'll have to return to that after creating the the other blocks. *)

		let if_done_block = append_block context ((intlist_to_s node) ^ "-if-done") matchregex_f in
		(* When we're done, return true if the node is a final node *)
		let is_final_node_as_int = if List.mem ~equal:(=) finals node then 1 else 0 in
		position_at_end if_done_block builder;
		build_ret (const_int (i32_type context) is_final_node_as_int) builder;

		(* Now we create the extended if-else-if to handle each transition *)
		let transitions = edges
      |> IntListMap.find_opt node
      |> Option.value ~default:[]
      (* Sort order is important so that we take any character transitions before any wildcard transitions *)
      |> List.sort ~cmp:compare_transition_pair
      |> List.rev in
		let transitions_block = List.fold_left transitions
      ~init:fail_block (* This is the final else case of all the transitions -- if there is no matching transition, we "fail", i.e., report that there is no match *)
      ~f:(fun else_block (dest_node, transition) ->
        let dest_block = IntListMap.find dest_node node_block_map in

        (* Before jumping to the destination, advance to the next character on the input *)
        let pre_dest_block = append_block context ((intlist_to_s node) ^ "-to-" ^ (intlist_to_s node) ^ "-pre-dest") matchregex_f in
        position_at_end pre_dest_block builder;
        build_increment_input_ptr builder;
        build_br dest_block builder;

        let transition_block = append_block context ((intlist_to_s node) ^ "-to-" ^ (intlist_to_s node)) matchregex_f in
  			position_at_end transition_block builder;

  			(match transition with
  			| `Value c ->
  					let transition_test = build_icmp Icmp.Eq next_char (const_int (i8_type context) (int_of_char c)) "does-next-char-match" builder in
  					build_cond_br transition_test pre_dest_block else_block builder
  			| `Any -> build_br pre_dest_block builder);
  			(* We return the transition block so that it can be the else case of the another transition test *)
  			transition_block
  		) in

		position_at_end block builder;
		build_cond_br is_input_char_zero if_done_block transitions_block builder; ()
	) node_block_map;

	let result = string_of_llmodule the_module in

  (* DEBUG print *)
	(* Printf.printf "%s\n" result; *)

	let oc = open_out output_path in
	fprintf oc "%s\n" result;
	close_out oc
