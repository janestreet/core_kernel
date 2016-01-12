(** An array of flat tuples.

    A flat tuple is like an ordinary OCaml tuple, except it is second class and mutable.
    The flat tuples in a [Flat_array.t] are layed out sequentially in a single array, with
    each flat tuple's components immediately following the components of the prior flat
    tuple.  A flat tuple is not first class -- one can only refer to a flat tuple via its
    index in the array holding it.  Flat tuples are mutable via [Flat_array.set].
*)

module Slots : Tuple_type.Slots
module Slot  : Tuple_type.Slot

(** The type of a flat-tuple array.  ['slots] will look like [('a1, ..., 'an) Slots.tn],
    and the array holds flat tuples of type ['a1 * ... * 'an]. *)
type 'slots t [@@deriving sexp_of]

include Blit.     S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t

(** [create slots ~len init] creates an array of flat tuples, whose slots are initialized
    to the slots of [init], which is an ordinary OCaml tuple.  [create] raises if [len <
    0]. *)
val create : (('tuple, _) Slots.t as 'slots) -> len:int -> 'tuple -> 'slots t

(** [copy a] returns a shallow copy of [a], that is, a fresh array containing the same
    elements as [a]. *)
val copy : 'slots t -> 'slots t

(** accessors *)
val length : _ t -> int
val slots : 'slots t -> 'slots

(** These functions get and set individual slots of flat tuple [i] in array [t].

    It is required that [0 <= i < length t].*)
val get        : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a
val unsafe_get : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a
val set        : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a -> unit
val unsafe_set : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a -> unit

(** [set_to_init t i] sets flat tuple [i] to the [init] that was supplied to [create]. *)
val set_to_init : _ t -> int -> unit

(** [is_init t i] returns [true] iff flat tuple [i]'s slots are identical to those of
    the [init] supplied to [create]. *)
val is_init : _ t -> int -> bool

(** [get_all_slots t i] allocates a new ordinary OCaml tuple whose components are equal to
    the slots of the flat tuple at index [i] of [t].  This is esentially an allocation
    plus a blit from [t] to the newly allocated tuple.

    [set_all_slots t i tuple] sets all slots of the flat tuple at index [i] of [t] to
    their corresponding components of [tuple].  This is essentially a blit from [tuple] to
    [t].

    It is required that [0 <= i < length t]. *)
val get_all_slots : (('tuple, _) Slots.t) t -> int -> 'tuple
val set_all_slots : (('tuple, _) Slots.t) t -> int -> 'tuple -> unit
