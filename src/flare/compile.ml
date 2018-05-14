open Core
open Llvm
open ParseTools
open Regex

let context = global_context ()
let the_module = create_module context "regex matcher"
let builder = builder context
let int_type = i32_type context

let matchregex_f_type = function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ])


let compile_regex_and_get_fn r =
  let d = (nfa_to_dfa (regex_to_nfa r)) in

  let printf = Option.value_exn (lookup_function "printf" the_module) in

  let matchregex_f = define_function "matchregex" matchregex_f_type the_module in
  let matchregex_f_params = params matchregex_f in
  let matchregex_input_ptr_param = matchregex_f_params.(0) in
  set_value_name "matchregex_input_ptr_param" matchregex_input_ptr_param;

  let matchregex_entry_block = entry_block matchregex_f in
  let _ = position_at_end matchregex_entry_block builder in

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
	position_at_end matchregex_entry_block builder;

	IntListMap.iter (fun node block ->
		(* Start by checking if we're done processing the string, in which case we'll just return whether or not this is an accepting state *)
		position_at_end block builder;

    (* Retrieve the next character on the input *)
    let input_ptr_value = build_load matchregex_input_ptr "input_ptr_value" builder in
    let next_char = build_load input_ptr_value "next_char" builder in

    (* DEBUG print the transition *)
    (* TODO make this optional, a verbosity flag in flarec *)
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
      |> intlistmapfindopt node
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
  (* Return the function (equivalently its address) *)
  matchregex_f

let compile_regex s output_path =
  (* DEBUG *)
  (* output_nfa (parse_regex_to_nfa_exn r) [] 0;
  output_dfa d 0; *)

	(* Declare printf for debugging *)
	let printf = declare_function "printf" (var_arg_function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ])) the_module in

  let _ = compile_regex_and_get_fn (parse_regex_exn s) in

	let result = string_of_llmodule the_module in

  (* DEBUG print *)
	(* Printf.printf "%s\n" result; *)

	let oc = open_out output_path in
	fprintf oc "%s\n" result;
	close_out oc

let rec get_all_regexes_in_flarex = function
  | `FConcat (f1, f2) -> (get_all_regexes_in_flarex f1) @ (get_all_regexes_in_flarex f2)
  | `FCell (r, _, _, _) -> [r]
  | `FAlternation (f1, f2) -> (get_all_regexes_in_flarex f1) @ (get_all_regexes_in_flarex f2)
  | `FEmpty -> []

let compile_all_regexes_in_flarex flarex = flarex
  |> get_all_regexes_in_flarex
  |> List.map ~f:compile_regex_and_get_fn

type numbered_flarex = [
  (* TODO Could optimize/simplify by insisting that left element of an FConcat is an FCell *)
  | `NFConcat of numbered_flarex * numbered_flarex
  | `NFCell of int * regex * bool * horizontal_constraint * vertical_constraint
  | `NFAlternation of numbered_flarex * numbered_flarex
  | `NFEmpty
]

let flarex_to_numbered_flarex f =
  let id_counter = ref (-1) in
  let rec f2nf f =
    match f with
    | `FConcat (left, right) ->
      let nleft = f2nf left in
      let nright = f2nf right in
      `NFConcat (nleft, nright)
    | `FCell (r, c, h, v) -> (id_counter := !id_counter + 1); `NFCell (!id_counter, r, c, h, v)
    | `FAlternation (f1, f2) -> `NFAlternation ((f2nf f1), (f2nf f2))
    | `FEmpty -> `NFEmpty in
  f2nf f

type flare_node = {
  id: int;
  is_capture: bool;
  h_constraint: horizontal_constraint;
  v_constraint: vertical_constraint;
  successors: int list;
  description: string;
}

let numbered_flarex_to_flare_node_list numbered_f =
  let rec accumulate_flare_nodes_and_return_firsts nf firsts_afterward =
  match nf with
  | `NFConcat (left, right) ->
    let (right_firsts, right_out) = accumulate_flare_nodes_and_return_firsts right firsts_afterward in
    let (left_firsts, left_out) = accumulate_flare_nodes_and_return_firsts left right_firsts in
    (left_firsts, (left_out @ right_out))
  | `NFCell (i, r, c, h, v) -> ([i], [{id=i; is_capture=c; h_constraint=h; v_constraint=v; successors=firsts_afterward; description=(regex_to_s r)}])
  | `NFAlternation (a, b) ->
    let (a_firsts, a_out) = accumulate_flare_nodes_and_return_firsts a firsts_afterward in
    let (b_firsts, b_out) = accumulate_flare_nodes_and_return_firsts b firsts_afterward in
    (a_firsts @ b_firsts, a_out @ b_out)
  | `NFEmpty -> ([], []) in
  let (start_node_id, flare_node_list) = accumulate_flare_nodes_and_return_firsts numbered_f [] in
  (start_node_id, flare_node_list)

let serialize_horizontal_constraint = function
  | `NoHorizontal -> "\"NoHorizontal\""
  | `LeftAny -> "\"LeftAny\""
  | `Left (n) -> Printf.sprintf "{\"Left\": %d}" n
  | `RightAny  -> "\"RightAny\""
  | `Right (n) -> Printf.sprintf "{\"Right\": %d}" n

let serialize_vertical_constraint = function
  | `NoVertical -> "\"NoVertical\""
  | `UpAny -> "\"UpAny\""
  | `Up (n) -> Printf.sprintf "{\"Up\": %d}" n
  | `DownAny  -> "\"DownAny\""
  | `Down (n) -> Printf.sprintf "{\"Down\": %d}" n

let serialize_successors l = l
  |> List.map ~f:(Printf.sprintf "%d")
  |> String.concat ~sep:", "
  |> Printf.sprintf "[%s]"

let serialize_flare_node {id; is_capture; h_constraint; v_constraint; successors; description} =
  Printf.sprintf "{\"id\": %d, \"is_capture\": %s, \"horizontal_constraint\": %s, \"vertical_constraint\": %s, \"successors\": %s, \"description\": \"%s\"}"
    id
    (if is_capture then "true" else "false")
    (serialize_horizontal_constraint h_constraint)
    (serialize_vertical_constraint v_constraint)
    (serialize_successors successors)
    description

let compile_flarex flarex output_path =
	(* Do the parsing and DFA conversion immediately *)
	let f = parse_flarex_exn flarex in

  (* Declare printf for debugging *)
	let printf = declare_function "printf" (var_arg_function_type (i32_type context) (Array.of_list [ pointer_type (i8_type context) ])) the_module in

  (* Compile all the regexes in the Flare program *)
  let fptrs = compile_all_regexes_in_flarex f in

  (* Once we've compiled them, label the Flare nodes and sort them. Zip together with fptrs to ensure that indices correspond in both lists. *)
  let _, flare_node_list = f |> flarex_to_numbered_flarex |> numbered_flarex_to_flare_node_list in
  let (flare_node_list, fptrs) = List.zip flare_node_list fptrs
  |> Option.value ~default:[] (* TODO We should throw an exception, but... I'm lazy. *)
  |> List.sort ~cmp:(fun (a, _) (b, _) -> a.id - b.id)
  |> List.unzip in

  (* Build a global array of the pointers to the compiled functions *)
  let num_fptrs = List.length fptrs in
  let matchregex_fptr_type = pointer_type matchregex_f_type in
  let fptrs_const_array = const_array matchregex_fptr_type (Array.of_list fptrs) in
  let fptrs_global_type = array_type matchregex_fptr_type num_fptrs in
  let fptrs_global_ptr = declare_global fptrs_global_type "regexptrsglobalarray" the_module in
  let _ = set_initializer fptrs_const_array fptrs_global_ptr in

  (* Create a function that returns a pointer to the global array *)
  let fptrs_global_fptr_type = pointer_type fptrs_global_type in
  let getregexpointers_f_type = function_type fptrs_global_fptr_type (Array.of_list []) in
  let getregexpointers_f = declare_function "getregexpointers" getregexpointers_f_type the_module in
  let getregexpointers_block = append_block context "getregexpointersblock" getregexpointers_f in
  let _ = position_at_end getregexpointers_block builder in
  let _ = build_ret fptrs_global_ptr builder in

  (* Create another function that returns the length of that same global array (from above) *)
  let getregexpointerslength_f_type = function_type (i32_type context) (Array.of_list []) in
  let getregexpointerslength_f = declare_function "getregexpointerslength" getregexpointerslength_f_type the_module in
  let getregexpointerslength_block = append_block context "getregexpointerslengthblock" getregexpointerslength_f in
  let _ = position_at_end getregexpointerslength_block builder in
  let _ = build_ret (const_int (i32_type context) num_fptrs) builder in

  (* Also create a function that returns a serialized JSON representation of the tree structure of the Flare program. *)
  let cstr_type = pointer_type (i8_type context) in
  let getflarenodes_f_type = function_type cstr_type (Array.of_list []) in
  let getflarenodes_f = declare_function "getflarenodes" getflarenodes_f_type the_module in
  let getflarenodes_block = append_block context "getflarenodesblock" getflarenodes_f in
  let _ = position_at_end getflarenodes_block builder in
  let flare_nodes_string = flare_node_list
    |> List.map ~f:serialize_flare_node
    |> String.concat ~sep:", "
    |> Printf.sprintf "[%s]" in
  let flarenodes_string_ptr = build_global_stringptr flare_nodes_string "flarenodes_string" builder in
  let _ = build_ret flarenodes_string_ptr builder in
	let result = string_of_llmodule the_module in


	let oc = open_out output_path in
	fprintf oc "%s\n" result;
	close_out oc
