(** Process ID. *)

open! Import

type t [@@deriving bin_io, hash, sexp]

include Identifiable.S with type t := t

val of_int : int -> t
val to_int : t -> int

val init : t (** The pid of the "init" process, which is [1] by convention. *)

module Stable : sig
  module V1 : sig
    type nonrec t                  = t
    type nonrec comparator_witness = comparator_witness

    include Stable_module_types.S0
      with type t                  := t
       and type comparator_witness := comparator_witness

    include Comparable.Stable.V1.S
      with type comparable         := t
       and type comparator_witness := comparator_witness
  end
end
