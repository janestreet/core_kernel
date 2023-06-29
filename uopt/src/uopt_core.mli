open! Core

(** @@inline *)
include module type of Uopt with type 'a t = 'a Uopt.t

include Binable.S1 with type 'a t := 'a t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, sexp, stable_witness]
  end
end
