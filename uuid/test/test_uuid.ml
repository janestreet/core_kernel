open! Core_kernel
open! Uuid

let%expect_test _ =
  Quickcheck.test_distinct_values
    ~sexp_of:Unstable.sexp_of_t
    quickcheck_generator
    ~trials:1_024
    ~distinct_values:512
    ~compare;
  [%expect {||}]
;;

let%expect_test _ =
  Quickcheck.test ~sexp_of:sexp_of_t quickcheck_generator ~f:invariant;
  [%expect {||}]
;;
