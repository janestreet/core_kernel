(** A queue implemented with an array.

    The implementation will grow the array as necessary.  The array will never
    automatically be shrunk, but the size can be interrogated and set with [capacity] and
    [set_capacity].

    Iteration functions ([iter], [fold], [map], [concat_map], [filter], [filter_map],
    [filter_inplace], and some functions from [Container.S1]) will raise if the queue is
    modified during iteration.

    Differences from the standard module:
      [enqueue] replaces [push] and [add], and takes the queue first.
      [dequeue] replaces [pop] and [take], and returns an option rather than raising
        [Empty].
      [dequeue_exn] is available if you want to raise [Empty].
      [iter] and [fold] take labeled arguments.
      [blit_transfer] replaces [transfer] but is markedly different; see below.
*)


type 'a t with bin_io, sexp

include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t

(** Create an empty queue. *)
val create
  :  ?capacity:int  (** default is [1]. *)
  -> unit
  -> _ t

val singleton : 'a -> 'a t

val enqueue : 'a t -> 'a -> unit

val dequeue     : 'a t -> 'a option
val dequeue_exn : 'a t -> 'a

val peek     : 'a t -> 'a option
val peek_exn : 'a t -> 'a

val clear : _ t -> unit

val copy : 'a t -> 'a t

(** Transfers up to [len] elements from the front of [src] to the end of [dst], removing
    them from [src].  It is an error if [len < 0].

    Aside from a call to [set_capacity dst] if needed, runs in O([len]) time *)
val blit_transfer
  :  src:'a t
  -> dst:'a t
  -> ?len:int (** default is [len = length src] *)
  -> unit
  -> unit

(** [of_list list] returns a queue [t] with the elements of [list] in the same order as
    the elements of [list] (i.e. the first element of [t] is the first element of the
    list). *)
val of_list : 'a list -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

(** creates a new queue with elements equal to [List.concat_map ~f (to_list t)]. *)
val concat_map : 'a t -> f:('a -> 'b list) -> 'b t

(** [filter_map] creates a new queue with elements equal to [List.filter_map ~f (to_list
    t)]. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

(** [filter] is like [filter_map], except with [List.filter]. *)
val filter : 'a t -> f:('a -> bool) -> 'a t

(** [filter_inplace t ~f] removes all elements of [t] that don't satisfy [f].  If [f]
    raises, [t] is unchanged.  This is inplace in that it modifies [t]; however, it uses
    space linear in the final length of [t]. *)
val filter_inplace : 'a t -> f:('a -> bool) -> unit

val of_array : 'a array -> 'a t

(** [get t i] returns the [i]'th element in [t], where the 0'th element is at the front of
    [t] and the [length t - 1] element is at the back. *)
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit

(** Returns the current length of the backing array. *)
val capacity : _ t -> int

(** [set_capacity t capacity] sets the capacity of [t]'s backing array to at least
    [max capacity (length t)].  If the capacity changes, then this involves allocating
    a new backing array and copying the queue elements over. *)
val set_capacity : _ t -> int -> unit

