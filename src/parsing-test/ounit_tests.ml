open OUnit;;
open Json;;


let test1 test_ctxt = assert_equal "x" "x";;

let test2 test_ctxt = assert_equal 100 100;;

let repeat_test test_ctxt = assert_equal "heyheyhey" (repeat "hey" 3);;
let repeat_zero_test test_ctxt = assert_equal "" (repeat "hey" 0);;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "repeat_test">:: repeat_test;
  "repeat_zero_test">:: repeat_zero_test
 ]
;;

let _ =
  run_test_tt_main suite
;;
