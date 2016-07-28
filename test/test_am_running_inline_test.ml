open! Core_kernel.Std
open! Expect_test_helpers_kernel.Std

let%expect_test "" =
  print_s [%message (am_running_inline_test : bool)];
  [%expect {|
    (am_running_inline_test true) |}];
;;
