(** This module defines the [Set] module for [Core.Std].  We use "core_set" as the file
    name rather than "set" to avoid conflicts with OCaml's standard set module.

    This module uses the same organizational approach as [Core_map].  See the
    documentation in core_map.mli for a description of the approach.

    Functions that construct a set take as an argument the comparator for the element
    type.
*)

open Core_set_intf

(** The type of a set.  The first type parameter identifies the type of the element, and
    the second identifies the comparator, which determines the comparison function that is
    used for ordering elements in this set.  Many operations (e.g., {!union}), require
    that they be passed sets with the same element type and the same comparator type. *)
type ('elt, 'cmp) t [@@deriving compare]

module Tree : sig
  (** A [Tree.t] contains just the tree data structure that a set is based on, without
      including the comparator.  Accordingly, any operation on a [Tree.t] must also take
      as an argument the corresponding comparator. *)
  type ('a, 'cmp) t [@@deriving sexp_of]

  include Creators_and_accessors2_with_comparator
    with type ('a, 'b) set  := ('a, 'b) t
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) t
end

(** Tests internal invariants of the set data structure.  Returns true on success. *)
val invariants : (_, _) t -> bool

val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t

(** Creates an empty set based on the provided comparator. *)
val empty : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) t

(** Creates a set based on the provided comparator that contains only the provided
    element. *)
val singleton : comparator:('a, 'cmp) Comparator.t -> 'a -> ('a, 'cmp) t

(** Returns the number of elements in the set.  [O(1)]. *)
val length : (_, _) t -> int

(** [is_empty t] is [true] iff [t] is empty.  [O(1)]. *)
val is_empty : (_, _) t -> bool

(** [mem t a] returns [true] iff [a] is in [t].  [O(log n)]. *)
val mem : ('a, _) t -> 'a -> bool

(** [add t a] returns a new set with [a] added to [t], or returns [t] if [mem t a].
    [O(log n)]. *)
val add : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t

(** [remove t a] returns a new set with [a] removed from [t] if [mem t a], or returns [t]
    otherwise.  [O(log n)]. *)
val remove : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t

(** [union t1 t2] returns the union of the two sets.  [O(length t1 + length t2)]. *)
val union : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [union ~comparator list] returns the union of all the sets in [list].  The
    [comparator] argument is required for the case where [list] is empty.
    [O(max(List.length list, n log n))], where [n] is the sum of sizes of the input sets.
*)
val union_list : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) t list -> ('a, 'cmp) t

(** [inter t1 t2] computes the intersection of sets [t1] and [t2].  [O(length t1 +
    length t2)]. *)
val inter : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [diff t1 t2] computes the set difference [t1 - t2], i.e., the set containing all
    elements in [t1] that are not in [t2].  [O(length t1 + length t2)]. *)
val diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [symmetric_diff t1 t2] returns a sequence of changes between [t1] and [t2]. It is
    intended to be efficient in the case where [t1] and [t2] share a large amount of
    structure. *)
val symmetric_diff
  :  ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> ('a, 'a) Either.t Sequence.t

(** [compare_direct t1 t2] compares the sets [t1] and [t2].  It returns the same result
    as [compare], but unlike compare, doesn't require arguments to be passed in for the
    type parameters of the set.  [O(length t1 + length t2)]. *)
val compare_direct : ('a, 'cmp) t -> ('a, 'cmp) t -> int

(** [equal t1 t2] returns [true] iff the two sets have the same elements.  [O(length t1 +
    length t2)] *)
val equal : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

(** [exists t ~f] returns [true] iff there exists an [a] in [t] for which [f a].  [O(n)],
    but returns as soon as it finds an [a] for which [f a]. *)
val exists : ('a, _) t -> f:('a -> bool) -> bool

(** [for_all t ~f] returns [true] iff for all [a] in [t], [f a].  [O(n)], but returns as
    soon as it finds an [a] for which [not (f a)]. *)
val for_all : ('a, _) t -> f:('a -> bool) -> bool

(** [count t] returns the number of elements of [t] for which [f] returns [true].
    [O(n)]. *)
val count : ('a, _) t -> f:('a -> bool) -> int

(** [sum t] returns the sum of [f t] for each [t] in the set.
    [O(n)]. *)
val sum
  : (module Commutative_group.S with type t = 'sum)
  -> ('a, _) t -> f:('a -> 'sum) -> 'sum

(** [find t f] returns an element of [t] for which [f] returns true, with no guarantee as
    to which element is returned.  [O(n)], but returns as soon as a suitable element is
    found. *)
val find : ('a, _) t -> f:('a -> bool) -> 'a option

(** [find_map t f] returns [b] for some [a] in [t] for which [f a = Some b].  If no such
    [a] exists, then [find] returns [None].  [O(n)], but returns as soon as a suitable
    element is found. *)
val find_map : ('a, _) t -> f:('a -> 'b option) -> 'b option

(** Like [find], but throws an exception on failure. *)
val find_exn : ('a, _) t -> f:('a -> bool) -> 'a

(** [find_index t i] returns the [i]th smallest element of [t], in [O(log n)] time.  The
    smallest element has [i = 0].  Returns [None] if [i < 0] or [i >= length t]. *)
val find_index : ('a, _) t -> int -> 'a option

(** [remove_index t i] returns a version of [t] with the [i]th smallest element removed,
    in [O(log n)] time.  The smallest element has [i = 0].  Returns [t] if [i < 0] or
    [i >= length t]. *)
val remove_index : ('a, 'cmp) t -> int -> ('a, 'cmp) t

(** [subset t1 t2] returns true iff [t1] is a subset of [t2]. *)
val subset : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

(** The list or array given to [of_list] and [of_array] need not be sorted. *)
val of_list  : comparator:('a, 'cmp) Comparator.t -> 'a list  -> ('a, 'cmp) t
val of_array : comparator:('a, 'cmp) Comparator.t -> 'a array -> ('a, 'cmp) t

(** [to_list] and [to_array] produce sequences sorted in ascending order according to the
    comparator. *)
val to_list  : ('a, _) t -> 'a list
val to_array : ('a, _) t -> 'a array

val to_tree : ('a, 'cmp) t -> ('a, 'cmp) Tree.t
val of_tree : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) Tree.t -> ('a, 'cmp) t

(** Create set from sorted array.  The input must be sorted (either in ascending or
    descending order as given by the comparator) and contain no duplicates, otherwise the
    result is an error.  The complexity of this function is [O(n)]. *)
val of_sorted_array
  :  comparator:('a, 'cmp) Comparator.t
  -> 'a array
  -> ('a, 'cmp) t Or_error.t

(** Similar to [of_sorted_array], but without checking the input array. *)
val of_sorted_array_unchecked
  :  comparator:('a, 'cmp) Comparator.t
  -> 'a array
  -> ('a, 'cmp) t

(** [stable_dedup_list] is here rather than in the [List] module because the
    implementation relies crucially on sets, and because doing so allows one to avoid uses
    of polymorphic comparison by instantiating the functor at a different implementation
    of [Comparator] and using the resulting [stable_dedup_list]. *)
val stable_dedup_list : comparator:('a, _) Comparator.t -> 'a list -> 'a list

(** [map ~comparator t ~f] returns a new set created by applying [f] to every element in
    [t].  The returned set is based on the provided [comparator].  [O(n log n)]. *)
val map
  :  comparator:('b, 'cmp) Comparator.t
  -> ('a, _) t
  -> f:('a -> 'b)
  -> ('b, 'cmp) t

(** Like {!map}, except elements for which [f] returns [None] will be dropped.  *)
val filter_map
  :  comparator:('b, 'cmp) Comparator.t
  -> ('a, _) t
  -> f:('a -> 'b option)
  -> ('b, 'cmp) t

(** [filter t ~f] returns the subset of [t] for which [f] evaluates to true.  [O(n log
    n)]. *)
val filter :  ('a, 'cmp) t -> f:('a -> bool) -> ('a, 'cmp) t

(** [fold t ~init ~f] folds over the elements of the set from smallest to largest. *)
val fold
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum)
  -> 'accum

(** Like {!fold}, except that it will terminate early, if [f] returns [`Stop]. *)
val fold_until
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> [ `Continue of 'accum | `Stop of 'accum ])
  -> 'accum

(** Like {!fold}, except that it goes from the largest to the smallest element. *)
val fold_right
  :  ('a, _) t
  -> init:'accum
  -> f:('a -> 'accum -> 'accum)
  -> 'accum

(** [iter t ~f] calls [f] on every element of [t], going in order from the smallest to
    largest.  *)
val iter : ('a, _) t -> f:('a -> unit) -> unit

(** Iterate two sets side by side.  Complexity is [O(m+n)] where [m] and [n] are the sizes
    of the two input sets.  As an example, with the inputs [0; 1] and [1; 2], [f] will be
    called with [`Left 0]; [`Both (1, 1)]; and [`Right 2]. *)
val iter2
  :  ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> f:([`Left of 'a | `Right of 'a | `Both of 'a * 'a] -> unit)
  -> unit

(** if [a, b = partition_tf set ~f] then [a] is the elements on which [f] produced [true],
    and [b] is the elements on which [f] produces [false]. *)
val partition_tf
  :  ('a, 'cmp) t
  -> f:('a -> bool)
  -> ('a, 'cmp) t * ('a, 'cmp) t

(** Same as {!to_list}. *)
val elements : ('a, _) t -> 'a list

(** Returns the smallest element of the set.  [O(log n)]. *)
val min_elt : ('a, _) t -> 'a option
(** Like {!min_elt}, but throws an exception when given an empty set. *)
val min_elt_exn : ('a, _) t -> 'a

(** Returns the largest element of the set.  [O(log n)].  *)
val max_elt : ('a, _) t -> 'a option
(** Like {!max_elt}, but throws an exception when given an empty set. *)
val max_elt_exn : ('a, _) t -> 'a

(** returns an arbitrary element, or [None] if the set is empty. *)
val choose : ('a, _) t -> 'a option
(** Like {!choose}, but throws an exception on an empty set. *)
val choose_exn : ('a, _) t -> 'a

(** [split t x] produces a triple [(t1, maybe_x, t2)] where [t1] is the set of elements
    strictly less than [x], [maybe_x] is the member (if any) of [t] which compares equal
    to [x], and [t2] is the set of elements strictly larger than [x]. *)
val split : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * 'a option * ('a, 'cmp) t

(** if [equiv] is an equivalence predicate, then [group_by set ~equiv] produces a list
    of equivalence classes (i.e., a set-theoretic quotient).  E.g.,

    {[
      let chars = Set.of_list ['A'; 'a'; 'b'; 'c'] in
      let equiv c c' = Char.equal (Char.uppercase c) (Char.uppercase c') in
      group_by chars ~equiv
    ]}

    produces:

    {[
      [Set.of_list ['A';'a']; Set.singleton 'b'; Set.singleton 'c']
    ]}

    [group_by] runs in O(n^2) time, so if you have a comparison function, it's usually
    much faster to use [Set.of_list]. *)
val group_by :  ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list

(** [to_sequence t] converts the set [t] to a sequence of the elements between
    [greater_or_equal_to] and [less_or_equal_to] inclusive in the order indicated by
    [order].  If [greater_or_equal_to > less_or_equal_to] the sequence is empty.  Cost is
    O(log n) up front and amortized O(1) for each element produced. *)
val to_sequence
  :  ?order               : [ `Increasing (** default *) | `Decreasing ]
  -> ?greater_or_equal_to : 'a
  -> ?less_or_equal_to    : 'a
  -> ('a, 'cmp) t
  -> 'a Sequence.t

(** Produces the elements of the two sets between [greater_or_equal_to] and
    [less_or_equal_to] in [order], noting whether each element appears in the left set,
    the right set, or both.  In the both case, both elements are returned, in case the
    caller can distinguish between elements that are equal to the sets' comparator.  Runs
    in O(length t + length t'). *)
module Merge_to_sequence_element : sig
  type 'a t = 'a Sequence.Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'a
    | Both of 'a * 'a
  [@@deriving bin_io, compare, sexp]
end
val merge_to_sequence
  :  ?order               : [ `Increasing (** default *) | `Decreasing ]
  -> ?greater_or_equal_to : 'a
  -> ?less_or_equal_to    : 'a
  -> ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> 'a Merge_to_sequence_element.t Sequence.t

(** Convert a set to or from a map.  [to_map] takes a function to produce data for each
    key.  Both functions run in O(n) time (assuming the function passed to [to_map] runs
    in constant time). *)
val to_map : ('key, 'cmp) t -> f:('key -> 'data) -> ('key, 'data, 'cmp) Map.t
val of_map_keys : ('key, _, 'cmp) Map.t -> ('key, 'cmp) t

val gen
  :  comparator:('key, 'cmp) Comparator.t
  -> 'key Quickcheck.gen
  -> ('key, 'cmp) t Quickcheck.gen
val obs : 'key Quickcheck.obs -> ('key, 'cmp) t Quickcheck.obs
val shrinker : 'key Quickcheck.shr -> ('key, 'cmp) t Quickcheck.shr

(** {1 Polymorphic sets}

    Module {!Poly} deals with sets that use OCaml's polymorphic comparison to compare
    elements.
*)

module Poly : sig
  type ('a, 'b) set

  module Tree : sig
    type 'elt t = ('elt, Comparator.Poly.comparator_witness) Tree.t [@@deriving sexp]

    include Creators_and_accessors1
      with type ('a, 'b) set := ('a, 'b) Tree.t
      with type 'elt t       := 'elt t
      with type 'elt tree    := 'elt t
      with type comparator_witness := Comparator.Poly.comparator_witness
  end

  type 'elt t = ('elt, Comparator.Poly.comparator_witness) set [@@deriving bin_io, compare, sexp]

  include Creators_and_accessors1
    with type ('a, 'b) set := ('a, 'b) set
    with type 'elt t       := 'elt t
    with type 'elt tree    := 'elt Tree.t
    with type comparator_witness := Comparator.Poly.comparator_witness
end
  with type ('a, 'b) set := ('a, 'b) t

(** {1 Signatures and functors for building [Set] modules}  *)

(** The signature that something needs to match in order to be used as a set element. *)
module type Elt = Elt

(** The signature that something needs to match in order to be used as a set element if
    the resulting set is going to support [bin_io]. *)
module type Elt_binable = Elt_binable

(** Module signature for a Set. *)
module type S = S0
  with type ('a, 'b) set  := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) Tree.t

(** Module signature for a Set that supports [bin_io]. *)
module type S_binable = S0_binable
  with type ('a, 'b) set  := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) Tree.t

(** [Make] builds a set from an element type that has a [compare] function but doesn't
    have a comparator.  This generates a new comparator.

    [Make_binable] is similar, except the element and set types support [bin_io]. *)
module Make         (Elt : Elt)         : S         with type Elt.t = Elt.t
module Make_binable (Elt : Elt_binable) : S_binable with type Elt.t = Elt.t

(** [Make_using_comparator] builds a set from an element type that has a comparator.

    [Make_binable_using_comparator] is similar, except the element and set types support
    [bin_io]. *)
module Make_using_comparator (Elt : sig
  type t [@@deriving sexp]
  include Comparator.S with type t := t
end)
  : S
    with type Elt.t = Elt.t
    with type Elt.comparator_witness = Elt.comparator_witness

module Make_binable_using_comparator (Elt : sig
  type t [@@deriving bin_io, sexp]
  include Comparator.S with type t := t
end)
  : S_binable
    with type Elt.t = Elt.t
    with type Elt.comparator_witness = Elt.comparator_witness
