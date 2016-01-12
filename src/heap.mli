(** Heap implementation based on a pairing-heap.

    This heap implementations supports an arbitrary element type, via a comparison
    function.  If you need a heap with elements ordered by integers, then it may be more
    efficient to use a [Timing_wheel.Priority_queue], which is a heap implementation
    specialized to integer keys, and with some other performance differences and usage
    restrictions. *)

include Heap_intf.S

(** Removable augments a heap with the ability to remove elements from the heap in lg(n)
    (amortized) time at any point after they have been added.  Elements within a Removable
    heap consume 4 words more memory and all heap operations will be somewhat slower. *)
module Removable : sig

  include Heap_intf.S

  module Elt : sig
    type 'a t [@@deriving sexp_of]

    (** [value_exn t] return the value in the heap controlled by this token if the value
        is still in the heap.  Raise otherwise. *)
    val value_exn : 'a t -> 'a
  end

  (** [add_removable t v] adds [v] to [t], returning a token that can be used to delete [v]
      from [t] in lg(n) amortized time. *)
  val add_removable : 'a t -> 'a -> 'a Elt.t

  (** If [t] and [token] are mismatched then behavior is undefined.  [remove] may safely
      be called on a token more than once.  This doesn't free all the memory associated
      with the Elt until some number of [pop] operations later -- see Heap_intf for
      details. *)
  val remove : 'a t -> 'a Elt.t -> unit

  (** [update t token v] is shorthand for [remove t token; add_removable t v] *)
  val update : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t

  (** [find_elt t ~f].  If [f] is true for some element in [t], return a [Elt.t] for
      that element.  This operation is O(n). *)
  val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option
end
