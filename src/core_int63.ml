open! Import

#import "config.h"

module Bin = struct
#ifdef JSC_ARCH_SIXTYFOUR
  include Binable0.Of_binable(Core_int)(struct
    type t = Base.Int63.t
    let of_binable = Base.Int63.of_int
    let to_binable = Base.Int63.to_int_exn
  end)
#else
  include Binable0.Of_binable(Core_int64)(struct
    type t = Base.Int63.t
    let of_binable = Base.Not_exposed_properly.Int63_emul.W.wrap_exn
    let to_binable = Base.Not_exposed_properly.Int63_emul.W.unwrap
  end)
#endif
  let bin_shape_t = Bin_prot.Shape.bin_shape_int63
end

module Stable_workaround = struct
  module V1 = struct
    module T = struct
      type t = Base.Int63.t [@@deriving hash, sexp]
      include Bin
      include (Base.Int63 : Base.Comparable.S
               with type t := t
               with type comparator_witness = Base.Int63.comparator_witness)
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

module type Typerepable = sig
  type t [@@deriving typerep]
end
type 'a typerepable = (module Typerepable with type t = 'a)
let typerep_of_repr : type a. a Base.Int63.Private.Repr.t -> a typerepable = function
  | Base.Int63.Private.Repr.Int   -> (module Core_int)
  | Base.Int63.Private.Repr.Int64 -> (module Core_int64)
include (val (typerep_of_repr Base.Int63.Private.repr : Base.Int63.t typerepable))

include Identifiable.Extend (Base.Int63) (struct
    type nonrec t = t
    include Bin
  end)

module Hex = struct
  type nonrec t = t [@@deriving typerep, bin_io]
  include (Base.Int63.Hex : module type of struct include Base.Int63.Hex end
           with type t := t)
end

include (Base.Int63
         : (module type of struct include Base.Int63 end
             with type t := t
             with module Hex := Hex))

#ifdef JSC_ARCH_SIXTYFOUR
let splittable_random random ~lo ~hi =
  of_int (Splittable_random.int random ~lo:(to_int_exn lo) ~hi:(to_int_exn hi))
#else
let splittable_random random ~lo ~hi =
  let open Base.Not_exposed_properly.Int63_emul.W in
  wrap_exn (Splittable_random.int64 random ~lo:(unwrap lo) ~hi:(unwrap hi))
#endif

include Quickcheck.Make_int (struct
    type nonrec t = t [@@deriving sexp, compare, hash]
    include (Replace_polymorphic_compare
             : Polymorphic_compare_intf.Infix with type t := t)
    let min_value         = min_value
    let max_value         = max_value
    let succ              = succ
    let pred              = pred
    let splittable_random = splittable_random
  end)
