open! OUnit2
open! Approx

let float_cmp a b = (a -. b) *. (a -. b) < 0.01

let test_linear _ =
  let points = [(3., 3.); (2., 2.); (1., 1.)] in
  let f = Approx.linear points in
  OUnit2.assert_equal ~cmp:float_cmp 4. (f 4.)

let test_segment _ =
  let points = [(3., 4.); (2., 4.); (1., 1.)] in
  let f = Approx.segment points in
  OUnit2.assert_equal ~cmp:float_cmp 4. (f 2.5) ;
  OUnit2.assert_equal ~cmp:float_cmp 2.5 (f 1.5)

let test =
  "test" >::: ["test_linear" >:: test_linear; "test_segment" >:: test_segment]

let () = run_test_tt_main test
