open! Import

include Span_intf.S
  with type underlying = float

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
  end
  module V2 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare, hash]
  end
end
