open! Core

module type S = sig
  type index
  type 'a t [@@deriving compare, equal, sexp]

  include Invariant.S1 with type 'a t := 'a t

  val create : ?initial_capacity:int -> unit -> 'a t

  (** [init n ~f] returns a fresh vector of length [n], with element number [i]
      initialized to the result of [f i]. In other words, [init n ~f] tabulates the
      results of [f] applied to the integers [0] to [n-1].

      Raise Invalid_argument if [n < 0].
  *)
  val init : int -> f:(int -> 'a) -> 'a t

  (** Raises if the index is invalid. *)
  val get : 'a t -> index -> 'a

  val maybe_get : 'a t -> index -> 'a option
  val maybe_get_local : 'a t -> index -> 'a Gel.t option

  (** Raises if the index is invalid. *)
  val set : 'a t -> index -> 'a -> unit

  include Container.S1 with type 'a t := 'a t
  include Blit.S1 with type 'a t := 'a t

  (** Finds the first 'a for which f is true **)
  val find_exn : 'a t -> f:('a -> bool) -> 'a

  (** [sort] uses constant heap space.
      To sort only part of the array, specify [pos] to be the index to start sorting from
      and [len] indicating how many elements to sort. *)
  val sort : ?pos:int -> ?len:int -> 'a t -> compare:('a -> 'a -> int) -> unit

  val is_sorted : 'a t -> compare:('a -> 'a -> int) -> bool
  val next_free_index : 'a t -> index
  val push_back : 'a t -> 'a -> unit
  val push_back_index : 'a t -> 'a -> index

  (** Grows the vec to the specified length if it is currently shorter. Sets all new
      indices to [default]. *)
  val grow_to : 'a t -> len:int -> default:'a -> unit

  (** Equivalent to [grow_to t (index + 1) ~default]. *)
  val grow_to_include : 'a t -> index -> default:'a -> unit

  (** Grows the vec to the specified length if it is currently shorter. Sets all new
      indices to [default idx]. *)
  val grow_to' : 'a t -> len:int -> default:(index -> 'a) -> unit

  (** Equivalent to [grow_to' t (index + 1) ~default]. *)
  val grow_to_include' : 'a t -> index -> default:(index -> 'a) -> unit

  (** Shortens the vec to the specified length if it is currently longer. Raises if [len <
      0]. *)
  val shrink_to : 'a t -> len:int -> unit

  (** [remove vec i] Removes the i-th element of the vector. This is not a fast
      implementation, and runs in O(N) time. (ie: it calls caml_modify under the hood)
  *)
  val remove_exn : 'a t -> int -> unit

  (** Find the first element that satisfies [f]. If exists, remove the element from the
      vector and return it. This is not a fast implementation, and runs in O(N) time.
  *)
  val find_and_remove : 'a t -> f:('a -> bool) -> 'a option

  val pop_back_exn : 'a t -> 'a
  val pop_back_unit_exn : 'a t -> unit
  val peek_back : 'a t -> 'a option
  val peek_back_exn : 'a t -> 'a
  val foldi : 'a t -> init:'accum -> f:(index -> 'accum -> 'a -> 'accum) -> 'accum

  val foldi_local_accum
    :  'a t
    -> init:'accum
    -> f:(index -> 'accum -> 'a -> 'accum)
    -> 'accum

  val iteri : 'a t -> f:(index -> 'a -> unit) -> unit
  val to_list : 'a t -> 'a list
  val to_local_list : 'a t -> 'a list
  val to_alist : 'a t -> (index * 'a) list

  (** The input vec is copied internally so that future modifications of it do not change
      the sequence. *)
  val to_sequence : 'a t -> 'a Sequence.t

  (** The input vec is shared with the sequence and modifications of it will result in
      modification of the sequence. *)
  val to_sequence_mutable : 'a t -> 'a Sequence.t

  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t
  val of_sequence : 'a Sequence.t -> 'a t

  (** [take_while t ~f] returns a fresh vec containing the longest prefix of [t] for which
      [f] is [true]. *)
  val take_while : 'a t -> f:('a -> bool) -> 'a t

  module Inplace : sig
    (** [sub] is like [Blit.sub], but modifies the vec in place. *)
    val sub : 'a t -> pos:index -> len:int -> unit

    (** [take_while t ~f] shortens the vec in place to the longest prefix of [t] for which
        [f] is [true]. *)
    val take_while : 'a t -> f:('a -> bool) -> unit

    (** Remove all elements from [t] that don't satisfy [f]. Shortens the vec in place. *)
    val filter : 'a t -> f:('a -> bool) -> unit

    (** Modifies a vec in place, applying [f] to every element of the vec. *)
    val map : 'a t -> f:('a -> 'a) -> unit

    (** Same as [map], but [f] also takes the index. *)
    val mapi : 'a t -> f:(index -> 'a -> 'a) -> unit
  end

  (** The number of elements we can hold without growing. *)
  val capacity : _ t -> int

  (** [clear t] discards all elements from [t] in O(length) time. *)
  val clear : _ t -> unit

  (** [clear_imm t] discards all elements from ['a t] in O(1) time if ['a] is immediate. *)
  val clear_imm : 'a t -> 'a Type_immediacy.Always.t -> unit

  (** [copy t] returns a copy of [t], that is, a fresh vec containing the same elements as
      [t]. *)
  val copy : 'a t -> 'a t

  (** [exists t ~f] returns true if [f] evaluates true on any element, else false *)
  val exists : 'a t -> f:('a -> bool) -> bool

  (** swap the values at the provided indices *)
  val swap : _ t -> index -> index -> unit

  (** [swap_to_last_and_pop t i] is equivalent to [swap t i (length t - 1); pop_back_exn t].
      It raises if [i] is out of bounds. *)
  val swap_to_last_and_pop : 'a t -> index -> 'a

  module With_structure_details : sig
    (** [[%sexp_of : t]] above only prints the elements. This gives various data structure
        details. *)
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end

  val unsafe_get : 'a t -> index -> 'a
  val unsafe_set : 'a t -> index -> 'a -> unit

  module Expert : sig
    val unsafe_inner : 'a t -> Obj.t Uniform_array.t
  end

  module Stable : sig
    module V1 : sig
      type nonrec 'a t = 'a t [@@deriving bin_io, compare, sexp]
    end
  end
end

module type Vec = sig
  (** A growable array of ['a]. Designed for efficiency and simplicity.

      This interface is generated lazily: if you need a standard function we haven't
      added, feel free to add or ping the authors.

      By default, [Vec] operations use integers as indices. The functor [Make] can be used
      to create a specialized version from any module implementing [Intable.S]. *)
  include S with type index := int

  module type S = S

  (** Generate a specialised version of [Vec] with a custom index type. *)
  module Make (M : Intable.S) : S with type index := M.t
end
