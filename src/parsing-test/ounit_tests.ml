open OUnit;;
open Json;;


let test1 test_ctxt = assert_equal "x" "x";;

let test2 test_ctxt = assert_equal 100 100;;

let repeat_test test_ctxt = assert_equal "heyheyhey" (repeat "hey" 3);;
let repeat_zero_test test_ctxt = assert_equal "" (repeat "hey" 0);;

let list_map_append_test test_ctxt =
  let e = IntMap.empty in
  let expected = IntMap.add 0 ["hey"; "hi"; "hello"] e in
  let actual = IntMap.add 0 ["hey"] e in
  let actual = list_map_append 0 ["hi"; "hello"] actual in
  assert_equal expected actual;;

let regex_to_nfa_test test_ctxt =
  let edges = IntMap.empty in
  let edges = IntMap.add 0 [(1, Some 'a')] edges in
  let desired = { edges; source = 0; sink = 1 } in
  assert_equal desired (regex_to_nfa (`Char 'a'));;

let eval1 test_ctxt =
assert_equal true (eval "a" (regex_to_nfa (`Char 'a')))

let eval2 test_ctxt =
  assert_equal true (eval "ab" (regex_to_nfa (`Concat (`Char 'a', `Char 'b')) ))


(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "eval1">:: eval1;
  "eval2">:: eval2;
  "list_map_append_test">:: list_map_append_test;
  "repeat_test">:: repeat_test;
  "repeat_zero_test">:: repeat_zero_test;
  "regex_to_nfa_test">:: regex_to_nfa_test
 ]
;;

let _ =
  run_test_tt_main suite
;;
