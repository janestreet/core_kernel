open! Import

type ('a, 'witness) t = ('a, 'witness) Base.Comparator.t =
  private
  { compare   : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Sexp.t
  }

include module type of Base.Comparator
  with type ('a, 'witness) t := ('a, 'witness) t

module Stable : sig
  module V1 : sig
    type nonrec ('a, 'b) t = ('a, 'b) t =
      private
      { compare   : 'a -> 'a -> int
      ; sexp_of_t : 'a -> Sexp.t
      }

    type ('a, 'b) comparator = ('a, 'b) t

    (** The following module types and functors may be used to define stable modules *)

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

    val make
      :  compare:('a -> 'a -> int)
      -> sexp_of_t:('a -> Sexp.t)
      -> (module S_fc with type comparable_t = 'a)

    module Make  : module type of Make
    module Make1 : module type of Make1
  end
end
