open! Import

type t = nativeint [@@deriving typerep]

include Identifiable.Extend (Base.Nativeint) (struct
    type t = nativeint [@@deriving bin_io]
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Nativeint.Hex
           : module type of struct include Base.Nativeint.Hex end with type t := t)
end

include (Base.Nativeint
         : (module type of struct include Base.Nativeint end
             with type t := t
             with module Hex := Hex))

include Quickcheck.Make_int (struct
    type nonrec t = t [@@deriving sexp, compare, hash]
    include (Base.Nativeint
             : Polymorphic_compare_intf.Infix with type t := t)
    let min_value         = min_value
    let max_value         = max_value
    let succ              = succ
    let pred              = pred
    let splittable_random = Splittable_random.nativeint
  end)
