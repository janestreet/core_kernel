open Import
open Bin_prot.Std
open Typerep_lib.Std

type t = int32 [@@deriving typerep]

include Identifiable.Extend (Base.Int32) (struct
    type t = int32 [@@deriving bin_io]
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Int32.Hex
           : module type of struct include Base.Int32.Hex end with type t := t)
end

include (Base.Int32
         : (module type of struct include Base.Int32 end
             with type t := t
             with module Hex := Hex))

include Quickcheck.Make_int (struct
    type nonrec t = t [@@deriving sexp, compare, hash]
    include (Base.Int32
             : Polymorphic_compare_intf.Infix with type t := t)
    let min_value         = min_value
    let max_value         = max_value
    let succ              = succ
    let pred              = pred
    let splittable_random = Splittable_random.int32
  end)
