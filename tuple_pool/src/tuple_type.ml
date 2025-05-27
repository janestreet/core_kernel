open! Core
open! Import
include Tuple_type_intf

module Slots = struct
  type u_ = { slots_per_tuple : int } [@@deriving sexp_of]
  type ('tuple, 'variant) u = u_ [@@deriving sexp_of]
  type t0 = [ `Slots of u_ ] [@@deriving sexp_of]
  type t_ = t0 Modes.Immutable_data.t

  let sexp_of_t_ { immutable_data = `Slots u } = sexp_of_t0 (`Slots u)

  type ('tuple, 'variant) t = t_ [@@deriving sexp_of]

  let slots_per_tuple { immutable_data = `Slots { slots_per_tuple = n } } = n

  type 'a0 t1 = t_ [@@deriving sexp_of]
  type ('a0, 'a1) t2 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2) t3 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3) t4 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3, 'a4) t5 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) t6 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) t7 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) t8 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) t9 = t_ [@@deriving sexp_of]
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9) t10 = t_ [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10) t11 = t_
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11) t12 = t_
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11, 'a12) t13 = t_
  [@@deriving sexp_of]

  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11, 'a12, 'a13) t14 = t_
  [@@deriving sexp_of]

  let t1 = { immutable_data = `Slots { slots_per_tuple = 1 } }
  let t2 = { immutable_data = `Slots { slots_per_tuple = 2 } }
  let t3 = { immutable_data = `Slots { slots_per_tuple = 3 } }
  let t4 = { immutable_data = `Slots { slots_per_tuple = 4 } }
  let t5 = { immutable_data = `Slots { slots_per_tuple = 5 } }
  let t6 = { immutable_data = `Slots { slots_per_tuple = 6 } }
  let t7 = { immutable_data = `Slots { slots_per_tuple = 7 } }
  let t8 = { immutable_data = `Slots { slots_per_tuple = 8 } }
  let t9 = { immutable_data = `Slots { slots_per_tuple = 9 } }
  let t10 = { immutable_data = `Slots { slots_per_tuple = 10 } }
  let t11 = { immutable_data = `Slots { slots_per_tuple = 11 } }
  let t12 = { immutable_data = `Slots { slots_per_tuple = 12 } }
  let t13 = { immutable_data = `Slots { slots_per_tuple = 13 } }
  let t14 = { immutable_data = `Slots { slots_per_tuple = 14 } }
end
