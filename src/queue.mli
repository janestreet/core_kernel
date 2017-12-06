(** A queue implemented with an array.

    The implementation will grow the array as necessary.  The array will never
    automatically be shrunk, but the size can be interrogated and set with [capacity] and
    [set_capacity].

    Iteration functions ([iter], [fold], [map], [concat_map], [filter], [filter_map],
    [filter_inplace], and some functions from [Container.S1]) will raise if the queue is
    modified during iteration.

    Also see [Linked_queue], which has different performance characteristics.
*)

open! Import

type 'a t [@@deriving compare]

include Queue_intf.S with type 'a t := 'a t (** @inline *)

include Binary_searchable.S1 with type 'a t := 'a t
include Equal.            S1 with type 'a t := 'a t
include Invariant.        S1 with type 'a t := 'a t

(** Create an empty queue. *)
val create
  :  ?capacity : int  (** default is [1]. *)
  -> unit
  -> _ t

(** [last t] returns the most recently enqueued element in [t], if any. *)
val last     : 'a t -> 'a option
val last_exn : 'a t -> 'a

(** Transfers up to [len] elements from the front of [src] to the end of [dst], removing
    them from [src].  It is an error if [len < 0].

    Aside from a call to [set_capacity dst] if needed, runs in O([len]) time *)
val blit_transfer
  :  src  : 'a t
  -> dst  : 'a t
  -> ?len : int  (** default is [length src] *)
  -> unit
  -> unit

(** [get t i] returns the [i]'th element in [t], where the 0'th element is at the front of
    [t] and the [length t - 1] element is at the back. *)
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit

(** Returns the current length of the backing array. *)
val capacity : _ t -> int

(** [set_capacity t c] sets the capacity of [t]'s backing array to at least [max c (length
    t)].  If [t]'s capacity changes, then this involves allocating a new backing array and
    copying the queue elements over.  [set_capacity] may decrease the capacity of [t], if
    [c < capacity t]. *)
val set_capacity : _ t -> int -> unit

module Stable : sig
  module V1 : Stable_module_types.S1 with type 'a t = 'a t
end

