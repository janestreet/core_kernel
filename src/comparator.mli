(** A type-indexed value that allows one to compare (and for generating error messages,
    serialize) values of the type in question.

    One of the type parameters is a phantom parameter used to distinguish comparators
    potentially built on different comparison functions.  In particular, we want to
    distinguish those using polymorphic compare and those using a monomorphic compare. *)

open Sexplib

type ('a, 'witness) t =
  private
  { compare   : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }

type ('a, 'b) comparator = ('a, 'b) t

module type S = sig
  type t
  type comparator_witness
  val comparator : (t, comparator_witness) comparator
end

module type S1 = sig
  type 'a t
  type comparator_witness
  val comparator : ('a t, comparator_witness) comparator
end

module Poly : S1 with type 'a t = 'a

module S_to_S1 (S : S) : S1
  with type 'a t = S.t
  with type comparator_witness = S.comparator_witness

(** [Make] creates a [comparator] value and its phantom [comparator_witness] type for a
    nullary type. *)
module Make (M : sig type t [@@deriving compare, sexp_of] end) : S with type t := M.t

(** [Make1] creates a [comparator] value and its phantom [comparator_witness] type for a
    unary type.  It takes a [compare] and [sexp_of_t] that have
    non-standard types because the [Comparator.t] type doesn't allow passing in
    additional values for the type argument. *)
module Make1 (M : sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val sexp_of_t : _ t -> Sexp.t
end) : S1 with type 'a t := 'a M.t
