(** This module implements an option ref that starts out as None, and
    may be set only once. If one tries to set it twice a run time
    error is generated. *)

open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t (** Passes when unset. *)

val create : unit -> 'a t

val set : 'a t -> 'a -> (unit, string) Result.t
val set_exn : 'a t -> 'a -> unit

val get : 'a t -> 'a option
val get_exn : 'a t -> 'a

module Unstable : sig
  type nonrec 'a t = 'a t [@@deriving bin_io, sexp]
end

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, sexp]
  end
end
