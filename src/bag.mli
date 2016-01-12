(** Imperative set-like data structure.

    Primary differences from a simple set:

    - It doesn't require anything (hashable, comparable) of elements in the bag.
    - Duplicates are allowed.
    - Addition and removal are constant time.

    It is an error to modify a bag ([add], [remove], [remove_one], ...) during iteration
    ([fold], [iter], ...).  *)
open Std_internal

module Elt : sig
  type 'a t

  val equal : 'a t -> 'a t -> bool
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val value : 'a t -> 'a
end

type 'a t [@@deriving sexp]

include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t

(** [create ()] returns an empty bag. *)
val create : unit -> 'a t

(** [add t v] adds [v] to the bag [t], returning an element that can
    later be removed from the bag.  [add] runs in constant time. *)
val add : 'a t -> 'a -> 'a Elt.t

(** [mem_elt t elt] returns whether or not [elt] is in [t].  It is like [mem] (included
    from [Container]), but it takes an ['a Elt.t] instead of an ['a] and runs in constant
    time instead of linear time. *)
val mem_elt : 'a t -> 'a Elt.t -> bool

(** [remove t elt] removes [elt] from the bag [t], raising an exception if [elt]
    is not in the bag.  [remove] runs in constant time. *)
val remove : 'a t -> 'a Elt.t -> unit

(** [choose t] returns some element in the bag. *)
val choose : 'a t -> 'a Elt.t option

(** [remove_one t] removes some element from the bag, and returns its value.
    [remove_one] runs in constant time. *)
val remove_one : 'a t -> 'a option

(** [clear t] removes all elements from the bag.  [clear] runs in O(1) time. *)
val clear : 'a t -> unit

(** [filter_inplace t ~f] removes all the elements from [t] that don't satisfy [f]. *)
val filter_inplace : 'a t -> f:('a -> bool) -> unit

val iter_elt : 'a t -> f:('a Elt.t -> unit) -> unit

(** [find_elt t ~f] looks at elements in the bag one-by-one until it finds one
    [elt] such that [f (Elt.value elt)], in which case it returns [Some elt].
    If there is no element satisfying [f], then [find_elt] returns [None]. *)
val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

(** [until_empty t f] repeatedly removes a value [v] from [t] and runs [f v],
    continuing until [t] is empty.  Running [f] may add elements to [t] if it
    wants. *)
val until_empty : 'a t -> ('a -> unit) -> unit

(** [transfer ~src ~dst] moves all of the elements from [src] to [dst] in constant
    time. *)
val transfer : src:'a t -> dst:'a t -> unit

val of_list : 'a list -> 'a t
val elts : 'a t -> 'a Elt.t list

(** [unchecked_iter t ~f] behaves like [iter t ~f] except that [f] is allowed to modify
    [t].  Elements added by [f] may or may not be visited, elements removed by [f] that
    have not been visited will not be visited.  It is an (undetected) error to delete the
    current element. *)
val unchecked_iter : 'a t -> f:('a -> unit) -> unit
