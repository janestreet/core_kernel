open! Import
open Std_internal

module type S = sig
  type underlying
  type t = private underlying [@@deriving bin_io, compare, hash, typerep]

  include Comparable.S_common with type t := t
  include Robustly_comparable with type t := t

  module Span  : Span_intf.Span with type underlying = underlying
  module Ofday : Ofday_intf.Ofday with type underlying := underlying
                                   and module Span := Span

  val add  : t -> Span.t -> t
  val sub  : t -> Span.t -> t
  val diff : t -> t -> Span.t

  (** [next t] returns the next t (forwards in time) *)
  val next : t -> t

  (** [prev t] returns the previous t (backwards in time) *)
  val prev : t -> t

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t

  (** [Date0.t] is the same as [Date.t] *)
  val utc_mktime : Date0.t -> Ofday.t -> t

  (** returns the amount of time since the epoch in seconds / 86_400 (which is one day
      in unix time terms) and the (non-negative) remainder as a Span *)
  val to_days_since_epoch_and_remainder : t -> (int * Span.t)

  val now : unit -> t
end
