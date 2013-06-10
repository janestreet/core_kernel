open Std_internal
open Import

module type Flat_array = module type of Flat_array

module Debug (Flat_array : Flat_array) : Flat_array = struct

  module Debug = Debug (struct end)

  include Debug

  open Flat_array

  module Slots = Slots
  module Slot  = Slot

  type nonrec 'a t = 'a t with sexp_of

  let create    = create
  let invariant = invariant
  let length    = length

  let debug x = debug (invariant ignore) "Flat_array." x

  let copy t =
    debug "copy" [t] () <:sexp_of< unit >> <:sexp_of< _ t >>
      (fun () -> copy t)
  ;;

  let debug_get name t i slot =
    debug name [t] (i, slot) <:sexp_of< int * (_, _) Slot.t >> <:sexp_of< _ >>
      (fun () -> get t i slot)
  ;;

  let get        t i slot = debug_get "get"        t i slot
  let unsafe_get t i slot = debug_get "unsafe_get" t i slot

  let debug_set name t i slot a =
    debug name [t] (i, slot) <:sexp_of< int * (_, _) Slot.t >> <:sexp_of< _ >>
      (fun () -> set t i slot a)
  ;;

  let set        t i slot a = debug_set "set"        t i slot a
  let unsafe_set t i slot a = debug_set "unsafe_set" t i slot a

  let get_tuple t i =
    debug "get_tuple" [t] i <:sexp_of< int >> <:sexp_of< _ >>
      (fun () -> get_tuple t i)
  ;;

end
