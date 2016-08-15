type never_returns = Nothing.t [@@deriving sexp_of]

let never_returns (_ : never_returns) = assert false
