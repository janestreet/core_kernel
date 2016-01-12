open Std

let%test_unit "sexp_of_int respects sexp_of_int_style" =
  let r = Int_conversions.sexp_of_int_style in
  let old = !r in
  r := `Underscores;
  [%test_result: Sexp.t] (1234 |> [%sexp_of: int]) ~expect:(Atom "1_234");
  r := `No_underscores;
  [%test_result: Sexp.t] (1234 |> [%sexp_of: int]) ~expect:(Atom "1234");
  r := old;
;;
