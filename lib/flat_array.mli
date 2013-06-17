(** An array of tuples, represented as an [Obj_array]. *)

open Import

module Slots : Tuple_type.Slots
module Slot  : Tuple_type.Slot

(** The type of a flat tuple array.  ['slots] will look like [('a1, ..., 'an) Slots.tn],
    and the array holds tuples of type ['a1 * ... * 'an]. *)
type 'slots t with sexp_of

include Blit.S1 with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t

(** [create slots ~len init] creates an array of [len] tuples, each initialized to
    [init].  [create] raises if [len < 0]. *)
val create : (('tuple, _) Slots.t as 'slots) -> len:int -> 'tuple -> 'slots t

(** [copy a] returns a shallow copy of [a], that is, a fresh array containing the same
    elements as [a]. *)
val copy : 'slots t -> 'slots t

(** accessors *)
val length : _ t -> int
val slots : 'slots t -> 'slots

(** [get t i slot] returns tuple [i]'s [slot].  [set t i slot a] sets tuple [i]'s [slot]
    to [a].  As with a regular [Array], after [set t i slot a], one knows that [phys_equal
    a (get t i slot a)]. *)
val get        : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a
val unsafe_get : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a
val set        : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a -> unit
val unsafe_set : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a -> unit

(** [get_tuple t i] allocates an OCaml tuple isomorphic to the tuple at index [i] in
    [t]. *)
val get_tuple : (('tuple, _) Slots.t) t -> int -> 'tuple
val set_tuple : (('tuple, _) Slots.t) t -> int -> 'tuple -> unit
