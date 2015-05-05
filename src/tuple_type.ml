open Sexplib.Conv

include Tuple_type_intf

module Slots = struct

  type u_ =
    { slots_per_tuple : int;
    }
  with sexp_of

  type ('tuple, 'variant) u = u_ with sexp_of

  type t_ = [ `Slots of u_ ] with sexp_of

  type ('tuple, 'variant) t = t_ with sexp_of

  let slots_per_tuple (`Slots { slots_per_tuple = n }) = n

  type 'a0 t1                                           = t_ with sexp_of
  type ('a0, 'a1) t2                                    = t_ with sexp_of
  type ('a0, 'a1, 'a2) t3                               = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3) t4                          = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4) t5                     = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) t6                = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) t7           = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) t8      = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) t9 = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9) t10
    = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10) t11
    = t_ with sexp_of
  type ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8, 'a9, 'a10, 'a11) t12
    = t_ with sexp_of

  let t1  = `Slots { slots_per_tuple = 1  }
  let t2  = `Slots { slots_per_tuple = 2  }
  let t3  = `Slots { slots_per_tuple = 3  }
  let t4  = `Slots { slots_per_tuple = 4  }
  let t5  = `Slots { slots_per_tuple = 5  }
  let t6  = `Slots { slots_per_tuple = 6  }
  let t7  = `Slots { slots_per_tuple = 7  }
  let t8  = `Slots { slots_per_tuple = 8  }
  let t9  = `Slots { slots_per_tuple = 9  }
  let t10 = `Slots { slots_per_tuple = 10 }
  let t11 = `Slots { slots_per_tuple = 11 }
  let t12 = `Slots { slots_per_tuple = 12 }

end
