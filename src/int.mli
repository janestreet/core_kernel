(** This module extends {!Base.Int}. *)

(** Note that [int] is already stable by itself, since as a primitive type it is an
    integral part of the sexp / bin_io protocol.  [Int.Stable] exists only to introduce
    [Int.Stable.Set] and [Int.Stable.Map], and provide interface uniformity with other
    stable types. *)

include module type of struct include Base.Int end
  with module Hex := Base.Int.Hex (** @open *)

include Int_intf.Extension_with_stable
  with type t := t
   and type comparator_witness := comparator_witness
