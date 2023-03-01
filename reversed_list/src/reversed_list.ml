type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving equal]

open Base

let of_list_rev = List.rev
let rev = List.rev
let rev_append = List.rev_append
let rev_map = List.rev_map
let rev_filter_map = List.rev_filter_map
let is_empty = List.is_empty
let length = List.length

module With_sexp_of = struct
  type nonrec 'a t = 'a t

  let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a t

  let%expect_test _ =
    Stdlib.print_endline (Sexp.to_string [%sexp ([ 1; 2 ] : int t)]);
    [%expect {| (1 2) |}]
  ;;
end

module With_rev_sexp_of = struct
  type nonrec 'a t = 'a t

  let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (rev t)

  let%expect_test _ =
    Stdlib.print_endline (Sexp.to_string [%sexp ([ 1; 2 ] : int t)]);
    [%expect {| (2 1) |}]
  ;;
end
