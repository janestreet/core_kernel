type 'a t = 'a list =
  | []
  | ( :: ) of 'a * 'a t
[@@deriving sexp_of]

let rev = List.rev
let rev_append = List.rev_append
let rev_filter_map = Base.List.rev_filter_map
let is_empty = Base.List.is_empty
