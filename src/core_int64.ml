open! Import

type t = int64 [@@deriving typerep]

include Identifiable.Extend (Base.Int64) (struct
    type t = int64 [@@deriving bin_io]
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Int64.Hex
           : module type of struct include Base.Int64.Hex end with type t := t)
end

include (Base.Int64
         : (module type of struct include Base.Int64 end
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
    let splittable_random = Splittable_random.int64
  end)
