open! Import

type t = bool [@@deriving bin_io, typerep]

include (Base.Bool : module type of struct include Base.Bool end with type t := t)

include Hashable.Make (Base.Bool)
include Comparable.Make_using_comparator (Base.Bool)

let quickcheck_generator = Base_quickcheck.Generator.bool
let quickcheck_observer = Base_quickcheck.Observer.bool
let quickcheck_shrinker = Base_quickcheck.Shrinker.bool
