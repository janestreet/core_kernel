open! Std_internal
open! Int.Replace_polymorphic_compare

module type Flat_array = module type of Flat_array

module Debug (Flat_array : Flat_array) : Flat_array = struct

  include Debug.Make ()

  open Flat_array

  module Slots = Slots
  module Slot  = Slot

  type nonrec 'a t = 'a t [@@deriving sexp_of]

  let create    = create
  let invariant = invariant
  let length    = length
  let slots     = slots

  let debug x = debug (invariant ignore) ~module_name:"Flat_array" x

  let set_to_init t i =
    debug "set_to_init" [t] i [%sexp_of: int] [%sexp_of: unit]
      (fun () -> set_to_init t i)
  ;;

  let is_init t i =
    debug "is_init" [t] i [%sexp_of: int] [%sexp_of: bool]
      (fun () -> is_init t i)
  ;;

  let copy t =
    debug "copy" [t] () [%sexp_of: unit] [%sexp_of: _ t]
      (fun () -> copy t)
  ;;

  let debug_get name t i slot =
    debug name [t] (i, slot) [%sexp_of: int * (_, _) Slot.t] [%sexp_of: _]
      (fun () -> get t i slot)
  ;;

  let get        t i slot = debug_get "get"        t i slot
  let unsafe_get t i slot = debug_get "unsafe_get" t i slot

  let debug_set name t i slot a =
    debug name [t] (i, slot) [%sexp_of: int * (_, _) Slot.t] [%sexp_of: _]
      (fun () -> set t i slot a)
  ;;

  let set        t i slot a = debug_set "set"        t i slot a
  let unsafe_set t i slot a = debug_set "unsafe_set" t i slot a

  let get_all_slots t i =
    debug "get_all_slots" [t] i [%sexp_of: int] [%sexp_of: _]
      (fun () -> get_all_slots t i)
  ;;

  let set_all_slots t i v =
    debug "set_all_slots" [t] i [%sexp_of: int] [%sexp_of: _]
      (fun () -> set_all_slots t i v)
  ;;

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    debug "blit" [ src; dst ]
      (`src_pos src_pos, `len len, `dst_pos dst_pos)
      [%sexp_of:
        [ `src_pos of int ]
        * [ `len of int ]
        * [ `dst_pos of int ]
        ]
      [%sexp_of: unit]
      (fun () -> blit ~src ~src_pos ~dst ~dst_pos ~len)
  ;;

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    debug "unsafe_blit" [ src; dst ]
      (`src_pos src_pos, `len len, `dst_pos dst_pos)
      [%sexp_of:
        [ `src_pos of int ]
        * [ `len of int ]
        * [ `dst_pos of int ]
        ]
      [%sexp_of: unit]
      (fun () -> unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len)
  ;;

  let blito ~src ?src_pos ?src_len ~dst ?dst_pos () =
    debug "blito" [ src; dst ]
      (`src_pos src_pos, `src_len src_len, `dst_pos dst_pos)
      [%sexp_of:
        [ `src_pos of int option ]
        * [ `src_len of int option ]
        * [ `dst_pos of int option ]
        ]
      [%sexp_of: unit]
      (fun () -> blito ~src ?src_pos ?src_len ~dst ?dst_pos ())
  ;;

  let sub t ~pos ~len =
    debug "sub" [ t ] (`pos pos, `len len)
      [%sexp_of: [ `pos of int ] * [ `len of int ]]
      [%sexp_of: _ t]
      (fun () -> sub t ~pos ~len)
  ;;

  let subo ?pos ?len t =
    debug "subo" [ t ]
      (`pos pos, `len len)
      [%sexp_of: [ `pos of int option ] * [ `len of int option ]]
      [%sexp_of: _ t]
      (fun () -> subo ?pos ?len t)
  ;;
end
