open Std_kernel
open Or_error

let%test_unit "[error_s] produces a value with the expected [sexp_of_t]" =
  let sexp = [%sexp "foo"] in
  match [%sexp (error_s sexp : _ t)] with
  | List [ Atom "Error"; sexp2 ] -> assert (phys_equal sexp sexp2);
  | _ -> assert false;
;;
