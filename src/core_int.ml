open Import
open Bin_prot.Std
open Typerep_lib.Std

module Stable_workaround = struct
  module V1 = struct
    module T = struct
      type t = int [@@deriving hash, bin_io, sexp]
      include (Base.Int : Base.Comparable.S
               with type t := t
               with type comparator_witness = Base.Int.comparator_witness)
    end
    include T
    include Comparable.Stable.V1.Make (T)
  end
end

module Stable = struct
  module V1 = struct
    include Stable_workaround.V1
  end
end

type t = int [@@deriving typerep]

include Identifiable.Extend (Base.Int) (struct
    type t = int [@@deriving bin_io]
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Int.Hex
           : module type of struct include Base.Int.Hex end with type t := t)
end

include (Base.Int
         : (module type of struct include Base.Int end
             with type t := t
             with module Hex := Hex))

include Quickcheck.Make_int (struct
    type nonrec t = t [@@deriving sexp, compare, hash]
    include (Replace_polymorphic_compare
             : Polymorphic_compare_intf.Infix with type t := t)
    let min_value         = min_value
    let max_value         = max_value
    let succ              = succ
    let pred              = pred
    let splittable_random = Splittable_random.int
  end)
