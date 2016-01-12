(** A queue of flat tuples, represented in a {!Flat_array}.

    The elements of a queue are numbered 0, 1, ..., [length t - 1], where element [0] is
    at the front of the queue.  One can access the [j]'th component of the [i]'th element
    using [get t i Slot.tj].

    A flat tuple is like an ordinary OCaml tuple, except it is second class and mutable.
    The flat tuples in a flat queue are layed out sequentially, with each flat tuple's
    components immediately following the components of the prior flat tuple.  A flat tuple
    is not first class -- one can only refer to a flat tuple via its index in the queue
    holding it.  Flat tuples are mutable via [Flat_queue.set].
*)

module Slots : Tuple_type.Slots
module Slot  : Tuple_type.Slot

(** The type of a flat queue.  ['slots] will look like [('a1, ..., 'an) Slots.tn], and the
    queue holds flat tuples of type ['a1 * ... * 'an]. *)
type 'slots t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** [create ?capacity slots] creates an empty queue with capacity at least the supplied
    [capacity].  It is an error if [capacity <= 0]. *)
val create
  :  ?capacity : int
  -> ((_, _) Slots.t as 'slots)
  -> 'slots t

(** [capacity t] returns the length of the array backing [t].  Enqueueing values will not
    cause the array to grow as long as [length t <= capacity t].  A queue at capacity
    will automatically increase capacity when enqueueing.  The capacity never decreases
    automatically; one can only decrease capacity via [set_capacity]. *)
val capacity : _ t -> int

(** [set_capacity t capacity] sets the length of the array backing [t] to as small as
    value as possible that is not less than [max capacity (length t)].  To shrink as much
    as possible, do [set_capacity t 0]. *)
val set_capacity : _ t -> int -> unit

val length : _ t -> int

val is_empty : _ t -> bool

(** These functions get and set individual slots of flat tuple [i] in queue [t].

    It is required that [0 <= i < length t]. *)
val get        : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a
val unsafe_get : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a
val set        : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a -> unit
val unsafe_set : ((_, 'v) Slots.t) t -> int -> ('v, 'a) Slot.t -> 'a -> unit

(** [drop_front ?n t] drops the the first [n] elements of [t].  It raises if [n < 0 || n >
    length t].

    [Flat_queue] does not have [dequeue] or [dequeue_exn] because the expected usage is to
    use [get t 0 Slot.tj] to access the front of the queue, and then to use [drop_front]
    to remove it.  This usage avoids ever allocating an ordinary OCaml tuple. *)
val drop_front
  :  ?n : int  (** default is 1. *)
  -> _ t
  -> unit

(** [clear t] removes all elements from [t]. *)
val clear : _ t -> unit

(** There is an [enqueueN] function for each possible arity of a flat queue. *)
val enqueue1
  :  'a0 Slots.t1 t
  -> 'a0 -> unit
val enqueue2
  :  ('a0, 'a1) Slots.t2 t
  -> 'a0 -> 'a1 -> unit
val enqueue3
  :  ('a0, 'a1, 'a2) Slots.t3 t
  -> 'a0 -> 'a1 -> 'a2 -> unit
val enqueue4
  :  ('a0, 'a1, 'a2, 'a3) Slots.t4 t
  -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> unit
val enqueue5
  :  ('a0, 'a1, 'a2, 'a3, 'a4) Slots.t5 t
  -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> unit
val enqueue6
  :  ('a0, 'a1, 'a2, 'a3, 'a4, 'a5) Slots.t6 t
  -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> unit
val enqueue7
  :  ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) Slots.t7 t
  -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> unit
val enqueue8
  :  ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) Slots.t8 t
  -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> unit
val enqueue9
  :  ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7, 'a8) Slots.t9 t
  -> 'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> unit

(** The functions below deal with Flat-array tuples as ordinary OCaml tuples.  These
    are intended for convenience but not for performance-critical code, due to the
    tuple allocation. *)

(** [get_all_slots t i] allocates a new ordinary OCaml tuple whose components are equal to
    the slots of the flat tuple at index [i] of [t].  This is esentially an allocation
    plus a blit from [t] to the newly allocated tuple.

    [set_all_slots t i tuple] sets all slots of the flat tuple at index [i] of [t] to
    their corresponding components of [tuple].  This is essentially a blit from [tuple] to
    [t].

    It is required that [0 <= i < length t]. *)
val get_all_slots : (('tuple, _) Slots.t) t -> int -> 'tuple
val set_all_slots : (('tuple, _) Slots.t) t -> int -> 'tuple -> unit

(** In [iter t ~f] and [fold t ~init ~f], if [f] mutates [t], then the iteration will
    raise. *)
val fold : (('tuple, _) Slots.t) t -> init:'a -> f:('a -> 'tuple -> 'a  ) -> 'a
val iter : (('tuple, _) Slots.t) t            -> f:(      'tuple -> unit) -> unit
