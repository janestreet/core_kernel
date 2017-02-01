open! Core_kernel
open! Import

let%expect_test "[Pervasives.float_of_string] supports underscores" =
  print_endline
    (Pervasives.string_of_float
       (Pervasives.float_of_string "1_234.567_8"));
  [%expect {|
    1234.5678 |}];
;;

let%expect_test "[Sexp.of_float_style] is respected by the various names for [float]" =
  let f = 1234.5678 in
  let print () =
    print_s [%sexp (f : float)];
    print_s [%sexp (f : Float.t)];
    print_s [%sexp (f : Core_kernel.Core_kernel_stable.float)];
  in
  print ();
  [%expect {|
    1234.5678
    1234.5678
    1234.5678 |}];
  Ref.set_temporarily Sexp.of_float_style `Underscores ~f:print;
  [%expect {|
    1_234.5678
    1_234.5678
    1_234.5678 |}];
;;

let%expect_test "[Sexp.of_float_style = `Underscores]" =
  let check f =
    let sexp style =
      Ref.set_temporarily Sexp.of_float_style style ~f:(fun () -> [%sexp (f : float)])
    in
    print_s [%sexp (sexp `No_underscores : Sexp.t),
                   (sexp `Underscores    : Sexp.t)];
    if not (Float.is_nan f)
    then (
      require [%here]
        (Float.equal f (sexp `Underscores |> [%of_sexp: Float.t]))) in
  List.iter
    [ 0.
    ; Float.min_positive_subnormal_value
    ; Float.min_positive_normal_value
    ; 1E-7
    ; 1.
    ; 12.
    ; 123.
    ; 1234.
    ; 12345.
    ; 1234E100
    ; Float.max_value
    ; Float.nan ]
    ~f:(fun f -> check f; check (-. f));
  [%expect {|
    (0 0)
    (-0 -0)
    (4.94065645841247E-324 4.94065645841247E-324)
    (-4.94065645841247E-324 -4.94065645841247E-324)
    (2.2250738585072014E-308 2.2250738585072014E-308)
    (-2.2250738585072014E-308 -2.2250738585072014E-308)
    (1E-07 1E-07)
    (-1E-07 -1E-07)
    (1 1)
    (-1 -1)
    (12 12)
    (-12 -12)
    (123 123)
    (-123 -123)
    (1234 1_234)
    (-1234 -1_234)
    (12345 12_345)
    (-12345 -12_345)
    (1.234E+103 1.234E+103)
    (-1.234E+103 -1.234E+103)
    (INF INF)
    (-INF -INF)
    (NAN NAN)
    ({-,}NAN {-,}NAN) (glob) |}];
;;

let%test_unit "round_nearest_half_to_even quickcheck" =
  Quickcheck.test
    ~trials:200
    (Int.gen_incl (-100_000_000) 100_000_000)
    ~f:(fun i ->
      let x = float i /. 10. in
      let y = Float.round_nearest_half_to_even x in
      let f = Float.round_nearest x in
      let is_tie = Int.(%) i 10 = 5 in
      assert (
        is_tie && Float.mod_float y 2. = 0. && Float.abs (y -. x) = 0.5
        || (not is_tie) && y = f
      );
      let x'  = Float.one_ulp `Up   x in
      let x'' = Float.one_ulp `Down x in
      assert (Float.round_nearest_half_to_even x'  = Float.round_nearest x');
      assert (Float.round_nearest_half_to_even x'' = Float.round_nearest x''))
;;

let%expect_test "robust_sign" =
  let test n = print_s [%sexp (Float.robust_sign n : Sign.t)] in
  test 1e-6;
  [%expect "Pos"];
  test 1e-8;
  [%expect "Zero"];
  test (-1e-6);
  [%expect "Neg"];
  test (-1e-8);
  [%expect "Zero"];
  test (-0.);
  [%expect "Zero"];
  test 0.;
  [%expect "Zero"];
  test Float.neg_infinity;
  [%expect "Neg"];
  (* preserve this old behavior of [sign] *)
  test Float.nan;
  [%expect "Zero"]

(* Make sure float comparison didn't accidentally get redefined using [compare]. *)
let%test _ =
  not (Float.(<) Float.nan 0.)
