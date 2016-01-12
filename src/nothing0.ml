(* To break the circular dependency *)

type t [@@deriving sexp, bin_io, compare]

let all = []
