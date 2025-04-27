open! Core
include Expect_test_helpers_core

let () = Dynamic.set_root Sexp.of_int_style `Underscores
