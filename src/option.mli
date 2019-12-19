(** This module extends {{!Base.Option}[Base.Option]} with bin_io, quickcheck, and support
    for ppx_optional. *)

type 'a t = 'a Base.Option.t [@@deriving bin_io, typerep]

(** @open *)
include module type of struct
  include Base.Option
end
with type 'a t := 'a option

include Comparator.Derived with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, equal, sexp]
  end
end

(** [Optional_syntax] allows [Option.t]s in the same matched expression as other types
    using [Optional_syntax]. *)
module Optional_syntax :
  Optional_syntax.S1 with type 'a t := 'a t and type 'a value := 'a
