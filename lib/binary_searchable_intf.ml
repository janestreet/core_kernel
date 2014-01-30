(** Module types for a [binary_search] function for a sequence, and functors for building
    [binary_search] functions. *)

(** An [Indexable] type is a finite sequence of elements indexed by consecutive integers
    [0] ... [length t - 1]. *)
module type Indexable = sig
  type elt
  type t

  val get : t -> int -> elt
  val length : t -> int
end

module type Indexable1 = sig
  type 'a t

  val get    : 'a t -> int -> 'a
  val length : _ t -> int
end

module type S = sig
  type elt
  type t

  (** [binary_search ?pos ?len t ~compare v] returns the index in [t] holding a value
      equal to [v] according to [compare].  It assumes that [t] is sorted in nondecreasing
      order according to [compare].  By default, it does a binary search of the entire
      [t].  One can supply [?pos] [?len] to search a slice of [t]. *)
  val binary_search
    :  ?pos:int
    -> ?len:int
    -> t
    -> compare:(elt -> elt -> int)
    -> elt
    -> int option
end

module type S1 = sig
  type 'a t

  val binary_search
    :  ?pos:int
    -> ?len:int
    -> 'a t
    -> compare:('a -> 'a -> int)
    -> 'a
    -> int option
end

module type Binary_searchable = sig
  module type S          = S
  module type S1         = S1
  module type Indexable  = Indexable
  module type Indexable1 = Indexable1
  module Make  (T : Indexable)  : S  with type    t :=    T.t with type elt := T.elt
  module Make1 (T : Indexable1) : S1 with type 'a t := 'a T.t
end
