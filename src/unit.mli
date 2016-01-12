(** Module for the type [unit].  This is mostly useful for building functor arguments. *)

type t = unit [@@deriving typerep]

include Identifiable.S   with type t := t
include Invariant.S      with type t := t
include Quickcheckable.S with type t := t

module type S = sig end

type m = (module S)
