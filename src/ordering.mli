open! Import

type t = Base.Ordering.t =
  | Less
  | Equal
  | Greater
[@@deriving bin_io, compare, hash, sexp]

include module type of Base.Ordering with type t := t
