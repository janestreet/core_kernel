open! Import

type 'a t = 'a option [@@deriving bin_io, typerep]

include (Base.Option : module type of struct include Base.Option end
         with type 'a t := 'a t)

include Comparator.Derived(struct
    type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
  end)

let gen = Base_quickcheck.Generator.option
let obs = Base_quickcheck.Observer.option
let shrinker = Base_quickcheck.Shrinker.option
