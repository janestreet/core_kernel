open! Import

type t = unit [@@deriving typerep]

include Identifiable.Extend(Base.Unit)(struct
    type t = unit [@@deriving bin_io]
  end)

include (Base.Unit
         : module type of struct include Base.Unit end with type t := t)

let quickcheck_generator = Base_quickcheck.Generator.unit
let quickcheck_observer = Base_quickcheck.Observer.unit
let quickcheck_shrinker = Base_quickcheck.Shrinker.unit

module type S = sig end

type m = (module S)
