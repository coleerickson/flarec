open OUnit
open Json
open Test

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
  let edges = IntMap.add 0 [(1, `Value 'a')] edges in
  let desired = { edges; start = 0; final = 1 } in
  assert_equal desired (regex_to_nfa (`Char 'a'));;

let eval1 test_ctxt =
  assert_equal true (eval "a" (regex_to_nfa (`Char 'a')))

let eval2 test_ctxt =
  assert_equal true (eval "ab" (regex_to_nfa (`Concat (`Char 'a', `Char 'b')) ))

let eval3 test_ctxt =
  assert_equal false (eval "b" (regex_to_nfa (`Char 'a')))

let eval4 test_ctxt =
  assert_equal false (eval "ba" (regex_to_nfa (`Concat (`Char 'a', `Char 'b')) ))

let eval5 test_ctxt =
  assert_equal false (eval "a" (regex_to_nfa (`Concat (`Char 'a', `Char 'b')) ))

let eval_wildcard test_ctxt =
  let nfa = regex_to_nfa `Wildcard in
  assert_equal false (eval "" nfa);
  assert_equal true (eval " " nfa);
  assert_equal true (eval "a" nfa);
  assert_equal false (eval "aa" nfa)

let eval_repetition test_ctxt =
  let nfa = regex_to_nfa (`Repetition (`Char 'a')) in
  assert_equal true (eval "" nfa);
  assert_equal true (eval "a" nfa);
  assert_equal true (eval "aaaaaaa" nfa);
  assert_equal false (eval "c" nfa)

let eval_alternation test_ctxt =
  let nfa = regex_to_nfa (`Alternation (`Char 'a', `Char 'b')) in
  assert_equal true (eval "a" nfa);
  assert_equal true (eval "b" nfa);
  assert_equal false (eval "aa" nfa);
  assert_equal false (eval "c" nfa);
  assert_equal false (eval "" nfa)

let parse_match_test test_ctxt =
  assert_equal true (match_regex "/ab*/" "abbb")

let parse_match_question_mark_test test_ctxt =
  assert_equal true (match_regex "/ab?/" "ab");
  assert_equal true (match_regex "/ab?/" "a");
  assert_equal false (match_regex "/ab?/" "abb")


let parse_match_big_test test_ctxt =
  assert_equal true (match_regex "/abc(hi|hey|hello)*ab*c/" "abchellohiheyheyabbbc");
  assert_equal false (match_regex "/abc(hi|hey|hello)*ab*c/" "abchellohiheyeheyabbbc");
  assert_equal false (match_regex "/abc(hi|hey|hello)*ab*c/" "abc");
  assert_equal true (match_regex "/abc(hi|hey|hello)*ab*c/" "abcabc")

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "eval1">:: eval1;
  "eval2">:: eval2;
  "eval3">:: eval3;
  "eval4">:: eval4;
  "eval5">:: eval5;
  "eval_wildcard">:: eval_wildcard;
  "eval_repetition">:: eval_repetition;
  "eval_alternation">:: eval_alternation;
  "parse_match_test">:: parse_match_test;
  "parse_match_question_mark_test">:: parse_match_question_mark_test;
  "parse_match_big_test">:: parse_match_big_test;
  "list_map_append_test">:: list_map_append_test;
  "repeat_test">:: repeat_test;
  "repeat_zero_test">:: repeat_zero_test;
  "regex_to_nfa_test">:: regex_to_nfa_test
 ]
;;

let _ =
  run_test_tt_main suite
;;
