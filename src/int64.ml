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
             with module Hex := Base.Int64.Hex))

include Quickcheck.Make_int (struct
    include Base.Int64
    let splittable_random = Splittable_random.int64
    let splittable_random_log_uniform = Splittable_random.Log_uniform.int64
  end)
