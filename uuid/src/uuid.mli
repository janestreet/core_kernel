@@ portable

(** Thread-safe generation of random identifiers in the UUID format.

    This library is not RFC 4122 compliant: the version is set in the output, but the
    variant is not. *)

open! Core

(** When [am_running_test], [sexp_of_t] masks the UUID, showing only
    "<uuid-omitted-in-test>". You can use [Unstable.sexp_of_t] if you definitely want to
    see it within your tests. *)
type t : immutable_data [@@deriving hash, sexp_of]

include%template Comparator.S [@modality portable] with type t := t

include Identifiable.S with type t := t and type comparator_witness := comparator_witness
include Invariant.S with type t := t
include Quickcheckable.S with type t := t

val t_of_sexp : Sexp.t -> t
[@@deprecated "[since 2017-11] Use a [Stable] or [Unstable] [t_of_sexp]."]

val create_random : Random.State.t -> t
val arg_type : t Command.Arg_type.t

module Unstable : sig
  (** Unlike the toplevel [sexp_of_t], [Unstable.sexp_of_t] shows the uuid even when
      [am_running_test]. Unlike [Stable] deserializers, [Unstable.t_of_sexp] validates the
      input. *)
  type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar]

  include Comparator.S with type t := t with type comparator_witness = comparator_witness
end

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving equal, hash, sexp_grammar]

    include
      Stable_comparable.With_stable_witness.V1
      with type t := t
      with type comparator_witness = comparator_witness

    include Stringable.S with type t := t

    val for_testing : t
  end
end

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val is_valid_exn : t -> unit
  val nil : t
  val create : hostname:string -> pid:int -> t
  val bottom_4_bits_to_hex_char : int -> char
end
