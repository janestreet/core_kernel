open Core_kernel
open Expect_test_helpers_kernel

open Md5

let%expect_test "digest_bin_prot" =
  let test (type t) (t : t) (module M : Binable.S with type t = t) =
    print_s [%sexp (digest_bin_prot M.bin_writer_t t : Md5.t)]
  in
  test () (module Unit);
  test 1337 (module Int);
  [%expect {|
    93b885adfe0da089cdf634904fd59f71
    22eaa1d1a43daf2abf8bb3f7b8d0128c |}];
;;

let%test_module "Md5.As_binary_string.V1" = (module Stable_unit_test.Make(struct
    include As_binary_string.Stable.V1
    let equal = [%compare.equal: t]
    let tests =
      [ digest_string ""
      , {|"\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~"|}
      , "\016\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~";
        digest_string "x"
      , {|"\157\212\228a&\140\1284\245\200VN\021\\g\166"|}
      , "\016\157\212\228a&\140\1284\245\200VN\021\\g\166"
      ]
  end))

let%test_module "Md5.V1" = (module Stable_unit_test.Make(struct
    include Stable.V1
    let equal = [%compare.equal: t]
    let tests =
      [ digest_string ""
      , "d41d8cd98f00b204e9800998ecf8427e"
      , "\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~";
        digest_string "x"
      , "9dd4e461268c8034f5c8564e155c67a6"
      , "\157\212\228a&\140\1284\245\200VN\021\\g\166"
      ]
  end))
