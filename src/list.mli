(** This module extends {!Base.List} with bin_io and quickcheck. *)

open! Import

type 'a t = 'a list [@@deriving bin_io, typerep]

module Assoc : sig
  type ('a, 'b) t = ('a, 'b) Base.List.Assoc.t [@@deriving bin_io]

  val compare : [%compare: 'a] -> [%compare: 'b] -> [%compare: ('a, 'b) t]
  [@@deprecated
    "[since 2016-06] This does not respect the equivalence class promised by List.Assoc.\n\
     Use List.compare directly if that's what you want."]

  include (module type of struct include Base.List.Assoc end
            with type ('a, 'b) t := ('a, 'b) t)
end

(** {2 The interface from Base} *)

include module type of struct include Base.List end
  with type 'a t := 'a t
  with module Assoc := Assoc (** @open *)

(** {2 Extensions} *)

(** [stable_dedup] Same as [dedup] but maintains the order of the list and doesn't allow
    compare function to be specified (otherwise, the implementation in terms of Set.t
    would hide a heavyweight functor instantiation at each call). *)
val stable_dedup : 'a t -> 'a t

val stable_dedup_staged
  :  compare:('a -> 'a -> int)
  -> ('a list -> 'a list) Staged.t

include Comparator.Derived with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

val to_string : f:('a -> string) -> 'a t -> string

(** Like [gen], but never generates the empty list. *)
val gen_non_empty : 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t

(** Like [gen], but generates lists with the given length. *)
val gen_with_length : int -> 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t

(** [gen_permutations t] generates all permutations of [list].  If [t] contains duplicate
    values, then [gen_permutations t] will produce duplicate lists. *)
val gen_permutations : 'a t -> 'a t Quickcheck.Generator.t
