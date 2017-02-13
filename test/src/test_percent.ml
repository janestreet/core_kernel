open! Core_kernel
open  Expect_test_helpers_kernel

let%expect_test _ =
  print_and_check_comparable_sexps [%here] (module Percent) [
    Percent.zero;
    Percent.of_bp         15.;
    Percent.of_percentage 15.;
    Percent.of_mult       15.;
  ] ~cr:CR_soon ~hide_positions:true;
  [%expect {|
    (Set (0 0.0015 0.15 15))
    ("set sexp does not match sorted list sexp"
      (set_sexp         (0  0.0015 0.15 15))
      (sorted_list_sexp (0x 15bp   15%  15x)))
    (Map (
      (0      0)
      (0.0015 1)
      (0.15   2)
      (15     3)))
    ("map sexp does not match sorted alist sexp"
     (map_sexp (
       (0      0)
       (0.0015 1)
       (0.15   2)
       (15     3)))
     (sorted_alist_sexp (
       (0x   0)
       (15bp 1)
       (15%  2)
       (15x  3)))) |}];
;;
