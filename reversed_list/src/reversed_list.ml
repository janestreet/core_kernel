type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving sexp_of]

let rev = List.rev
let rev_append = List.rev_append
