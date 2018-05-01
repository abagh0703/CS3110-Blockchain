open Crypto
open User

open OUnit2

let tests =
  [
    "trivial" >:: (fun _ -> assert_equal 1 1);
  ]

let suite =
  "tests" >::: tests

let _ = run_test_tt_main suite
          
