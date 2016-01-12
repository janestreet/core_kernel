(** Module types for a [binary_search] function for a sequence, and functors for building
    [binary_search] functions. *)

(** An [Indexable] type is a finite sequence of elements indexed by consecutive integers
    [0] ... [length t - 1].  [get] and [length] must be O(1) for the resulting
    [binary_search] to be lg(n). *)
module type Indexable_without_tests = sig
  type elt
  type t

  val get : t -> int -> elt
  val length : t -> int
end

module type Indexable = sig
  include Indexable_without_tests

  (** To implement the test provided by [Binary_searchable], we need to be able to
      construct [t] with two different values [small < big].  We also need to be able to
      build a [t] from an [elt array]. *)
  module For_test : sig
    val small    : elt
    val big      : elt
    val of_array : elt array -> t
  end
end

module type Indexable1_without_tests = sig
  type 'a t

  val get    : 'a t -> int -> 'a
  val length : _ t -> int
end

module type Indexable1 = sig
  include Indexable1_without_tests

  module For_test : sig
    val of_array : bool array -> bool t
  end
end

type ('t, 'elt) binary_search =
     ?pos:int
  -> ?len:int
  -> 't
  -> compare:('elt -> 'elt -> int)
  -> [ `Last_strictly_less_than         (** {v | < elt X |                       v} *)
     | `Last_less_than_or_equal_to      (** {v |      <= elt       X |           v} *)
     | `Last_equal_to                   (** {v           |   = elt X |           v} *)
     | `First_equal_to                  (** {v           | X = elt   |           v} *)
     | `First_greater_than_or_equal_to  (** {v           | X       >= elt      | v} *)
     | `First_strictly_greater_than     (** {v                       | X > elt | v} *)
     ]
  -> 'elt
  -> int option

type ('t, 'elt) binary_search_segmented =
     ?pos:int
  -> ?len:int
  -> 't
  -> segment_of:('elt -> [ `Left | `Right ])
  -> [ `Last_on_left | `First_on_right ]
  -> int option

module type S = sig
  type elt
  type t

  (**
    {0:Examples}

    The functions produced by this functor are very powerful, but also somewhat
    complex to read.  Here are some simple examples to clarify the use cases.
    Below we assume that the function [compare] is in scope:

    {[
      (* find the index of an element [e] in [t] *)
      binary_search t ~compare `First_equal_to e;

      (* find the index where an element [e] should be inserted *)
      binary_search t ~compare `First_greater_than_or_equal_to e;

      (* find the index in [t] where all elements to the left are less than [e] *)
      binary_search_segmented t ~segment_of:(fun e' ->
        if compare e' e <= 0 then `Left else `Right) `First_on_right
    ]}
  *)

  (** [binary_search ?pos ?len t ~compare which elt] takes [t] that is sorted in
      nondecreasing order according to [compare], where [compare] and [elt] divide [t]
      into three (possibly empty) segments:

      {v
        |  < elt  |  = elt  |  > elt  |
      v}

      [binary_search] returns the index in [t] of an element on the boundary of segments
      as specified by [which].  See the diagram below next to the [which] variants.

      By default, [binary_search] searches the entire [t].  One can supply [?pos] or
      [?len] to search a slice of [t].

      [binary_search] does not check that [compare] orders [t], and behavior is
      unspecified if [compare] doesn't order [t].  Behavior is also unspecified if
      [compare] mutates [t]. *)
  val binary_search : (t, elt) binary_search

  (** [binary_search_segmented ?pos ?len t ~segment_of which] takes an [segment_of]
      function that divides [t] into two (possibly empty) segments:

      {v
        | segment_of elt = `Left | segment_of elt = `Right |
      v}

      [binary_search_segmented] returns the index of the element on the boundary of the
      segments as specified by [which]: [`Last_on_left] yields the index of the last
      element of the left segment, while [`First_on_right] yields the index of the first
      element of the right segment.  It returns [None] if the segment is empty.

      By default, [binary_search] searches the entire [t].  One can supply [?pos] or
      [?len] to search a slice of [t].

      [binary_search_segmented] does not check that [segment_of] segments [t] as in the
      diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
      is also unspecified if [segment_of] mutates [t]. *)
  val binary_search_segmented : (t, elt) binary_search_segmented
end

module type S1 = sig
  type 'a t

  val binary_search           : ('a t, 'a) binary_search
  val binary_search_segmented : ('a t, 'a) binary_search_segmented
end

module type S1_permissions = sig
  open Perms.Export

  type ('a, -'perms) t

  val binary_search           : (('a, [> read]) t, 'a) binary_search
  val binary_search_segmented : (('a, [> read]) t, 'a) binary_search_segmented
end

module type Binary_searchable = sig
  module type S              = S
  module type S1             = S1
  module type S1_permissions = S1_permissions
  module type Indexable  = Indexable
  module type Indexable1 = Indexable1

  module Make  (T : Indexable)  : S  with type    t :=    T.t with type elt := T.elt
  module Make1 (T : Indexable1) : S1 with type 'a t := 'a T.t
  module Make_without_tests (T : Indexable_without_tests) : S
    with type t   := T.t
    with type elt := T.elt
  module Make1_without_tests (T : Indexable1_without_tests) : S1
    with type 'a t := 'a T.t
end
