type never_returns = Nothing0.t [@@deriving sexp_of]

let never_returns (_ : never_returns) = assert false
