(** This module extends the Base [String] module *)

type t = string [@@deriving bin_io, typerep]

module Caseless : sig
  type nonrec t = t [@@deriving bin_io, hash, sexp]

  include Comparable.S_binable with type t := t
  include Hashable.S_binable   with type t := t

  val is_suffix : t -> suffix:t -> bool
  val is_prefix : t -> prefix:t -> bool
end

include module type of struct include Base.String end
  with type t := t
  with module Caseless := Base.String.Caseless

include Hexdump.S        with type t := t
include Identifiable.S   with type t := t and type comparator_witness := comparator_witness
include Quickcheckable.S with type t := t

(** [gen' ?length char_gen] generates strings using the given distributions for string
    length and each character. *)
val gen'
  :  ?length : int Quickcheck.Generator.t
  (** defaults to size passed to generator *)
  -> char Quickcheck.Generator.t
  -> t    Quickcheck.Generator.t

(** Note that [string] is already stable by itself, since as a primitive type it is an
    integral part of the sexp / bin_io protocol. [String.Stable] exists only to introduce
    [String.Stable.Set] and [String.Stable.Map], and provide interface uniformity with
    other stable types. *)
module Stable : sig
  module V1 : sig
    type nonrec t = t
    type nonrec comparator_witness = comparator_witness
    include Stable_module_types.S0
      with type t := t
      with type comparator_witness := comparator_witness
    include Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness := comparator_witness
  end
end
