open Search
open OUnit2

let suite = "A4 test suite" >:::
  Test_data.tests @ Test_engine.tests

(* The following line must be the one and only place
 * in your entire source code that calls [OUnit2.run_test_tt_main]. *)
let _ = run_test_tt_main suite