open! Std_internal
open! Int.Replace_polymorphic_compare

module Debug (Flat_queue : module type of Flat_queue) = struct

  include Debug.Make ()

  open Flat_queue

  module Slots = Slots
  module Slot  = Slot

  type nonrec 'a t = 'a t [@@deriving sexp_of]

  let invariant = invariant

  let debug x = debug (invariant ignore) ~module_name:"Flat_queue" x

  let create ?capacity slots =
    debug "create" [] slots [%sexp_of: (_, _) Slots.t] [%sexp_of: _ t]
      (fun () -> create ?capacity slots)
  ;;

  let capacity t =
    debug "capacity" [t] t [%sexp_of: _ t] [%sexp_of: int]
      (fun () -> capacity t)
  ;;

  let set_capacity t capacity =
    debug "set_capacity" [t] (t, capacity) [%sexp_of: _ t * int] [%sexp_of: unit]
      (fun () -> set_capacity t capacity)
  ;;

  let length t =
    debug "length" [t] t [%sexp_of: _ t] [%sexp_of: int]
      (fun () -> length t)
  ;;

  let is_empty t =
    debug "is_empty" [t] t [%sexp_of: _ t] [%sexp_of: bool]
      (fun () -> is_empty t)
  ;;

  let clear t =
    debug "clear" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> clear t)
  ;;

  let drop_front ?n t =
    debug "drop_front" [t] (t, n) [%sexp_of: _ t * int option] [%sexp_of: unit]
      (fun () -> drop_front t ?n)
  ;;

  let enqueue1 t a0 =
    debug "enqueue1" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue1 t a0)
  ;;

  let enqueue2 t a0 a1 =
    debug "enqueue2" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue2 t a0 a1)
  ;;

  let enqueue3 t a0 a1 a2 =
    debug "enqueue3" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue3 t a0 a1 a2)
  ;;

  let enqueue4 t a0 a1 a2 a3 =
    debug "enqueue4" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue4 t a0 a1 a2 a3)
  ;;

  let enqueue5 t a0 a1 a2 a3 a4 =
    debug "enqueue5" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue5 t a0 a1 a2 a3 a4)
  ;;

  let enqueue6 t a0 a1 a2 a3 a4 a5 =
    debug "enqueue6" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue6 t a0 a1 a2 a3 a4 a5)
  ;;

  let enqueue7 t a0 a1 a2 a3 a4 a5 a6 =
    debug "enqueue7" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue7 t a0 a1 a2 a3 a4 a5 a6)
  ;;

  let enqueue8 t a0 a1 a2 a3 a4 a5 a6 a7 =
    debug "enqueue8" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue8 t a0 a1 a2 a3 a4 a5 a6 a7)
  ;;

  let enqueue9 t a0 a1 a2 a3 a4 a5 a6 a7 a8 =
    debug "enqueue9" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> enqueue9 t a0 a1 a2 a3 a4 a5 a6 a7 a8)
  ;;

  let debug_get name t i slot =
    debug name [t] (t, i, slot) [%sexp_of: _ t * int * (_, _) Slot.t] [%sexp_of: _]
      (fun () -> get t i slot)
  ;;

  let get        t i slot = debug_get "get"        t i slot
  let unsafe_get t i slot = debug_get "unsafe_get" t i slot

  let debug_set name t i slot a =
    debug name [t] (t, i, slot) [%sexp_of: _ t * int * (_, _) Slot.t] [%sexp_of: unit]
      (fun () -> set t i slot a)
  ;;

  let set        t i slot a = debug_set "set"        t i slot a
  let unsafe_set t i slot a = debug_set "unsafe_set" t i slot a

  let get_all_slots t i =
    debug "get_all_slots" [t] (t, i) [%sexp_of: _ t * int] [%sexp_of: _]
      (fun () -> get_all_slots t i)
  ;;

  let set_all_slots t i tuple =
    debug "set_all_slots" [t] (t, i) [%sexp_of: _ t * int] [%sexp_of: _]
      (fun () -> set_all_slots t i tuple)
  ;;

  let fold t ~init ~f =
    debug "fold" [t] t [%sexp_of: _ t] [%sexp_of: _]
      (fun () -> fold t ~init ~f)
  ;;

  let iter t ~f =
    debug "iter" [t] t [%sexp_of: _ t] [%sexp_of: unit]
      (fun () -> iter t ~f)
  ;;
end
