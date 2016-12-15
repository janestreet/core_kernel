open! Import

type t = bool [@@deriving bin_io, typerep]

include (Base.Bool : module type of struct include Base.Bool end with type t := t)

include Hashable.Make (Base.Bool)
include Comparable.Make_using_comparator (Base.Bool)

let gen =
  Quickcheck.Generator.doubleton true false

let obs =
  Quickcheck.Observer.doubleton Fn.id

let shrinker =
  Quickcheck.Shrinker.empty ()
