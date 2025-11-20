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

module Dequeue_result : sig
  type 'a t =
    | Empty
    | Not_empty of { global_ elt : 'a }
  [@@deriving sexp, compare ~localize]
end

(** [dequeue t] returns [Dequeue_result.Empty] if [length t = 0], and
    [Dequeue_result.Not_empty] otherwise. To dequeue a single result, match on this
    result. The return value is locally allocated, so this can be used safely in
    zero_alloc code.

    To dequeue until empty, use [dequeue_until_empty] below.

    Note that testing the length of the queue immediately before calling [dequeue] does
    not guarantee it will return [Not_empty]. This is because the OCaml compiler is
    allowed to insert a polling point (also called a "safe point") at the beginning of
    [dequeue], which may give other threads an opportunity to run. You must always handle
    the possibility of [Not_empty] when calling this function. *)
val dequeue : 'a t -> local_ 'a Dequeue_result.t

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

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  module Uopt : sig
    type 'a t [@@deriving sexp_of]

    val none : _ t
    val some : 'a -> 'a t
    val is_none : _ t -> bool
    val is_some : _ t -> bool
  end
end
