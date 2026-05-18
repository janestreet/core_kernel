@@ portable

(** A thread-safe non-blocking queue of unbounded size.

    The implementation does not use mutexes, and so is safe to use in situations when one
    doesn't want to block, e.g., a finalizer or an async job. *)

open! Core
open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

(** [create ()] returns an empty queue. *)
val create : unit -> 'a t

val length : _ t -> int
val enqueue : 'a t -> 'a -> unit

(** [dequeue t] returns [Null] if [length t = 0], and [This] otherwise. To dequeue a
    single result, match on this result.

    To dequeue until empty, use [dequeue_until_empty] below.

    Note that testing the length of the queue immediately before calling [dequeue] does
    not guarantee it will return [Null]. This is because the OCaml compiler is allowed to
    insert a polling point (also called a "safe point") at the beginning of [dequeue],
    which may give other threads an opportunity to run. You must always handle the
    possibility of [This] when calling this function. *)
val dequeue : 'a t -> 'a or_null
[@@zero_alloc]

(** [dequeue_until_empty ~f t] iteratively dequeues elements of [t] and applies [f] to
    them until the queue is empty.

    Recall that if you want [dequeue_until_empty]'s arguments to be locally allocated, it
    can not be a tailcall. To achieve this for a call to [dequeue_until_empty] in tail
    position, mark the call with [@nontail]. *)
val dequeue_until_empty : f:local_ ('a -> unit) -> 'a t -> unit

(** The queue maintains an internal pool of unused elements, which are used by [enqueue]
    and returned to the pool by [dequeue_exn]. [enqueue] creates a new element if the pool
    is empty. Nothing shrinks the pool automatically. One can call [clear_internal_pool]
    to clear the pool, so that all unused elements will be reclaimed by the garbage
    collector. *)
val clear_internal_pool : _ t -> unit
