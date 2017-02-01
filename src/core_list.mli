(** This module extends the Base [List] module with bin_io and quickcheck *)

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

include module type of struct include Base.List end
  with type 'a t := 'a t
  with module Assoc := Assoc

include Quickcheckable.S1 with type 'a t := 'a t

val to_string : f:('a -> string) -> 'a t -> string

(** Quickcheck generator for lists with additional customization.

    [List.gen' t] produces a generator for arbitrary lists of values from [t].

    - Adding [~length:(`Exactly n)] produces only lists of length [n].
    - Adding [~length:(`At_least n)] produces only lists of length [n] or greater.
    - Adding [~length:(`At_most n)] produces only lists of length [n] or less.
    - Adding [~length:(`Between_inclusive (m,n))] produces only lists of length [k] such
    that [m <= k] and [k <= n].

    The lists in the output of [list t] are generated uniquely, so long as the values in
    [t] are generated uniquely. *)
val gen'
  :  ?length : [ `Exactly           of int
               | `At_least          of int
               | `At_most           of int
               | `Between_inclusive of int * int ]
  -> 'a Quickcheck.Generator.t
  -> 'a t Quickcheck.Generator.t

(** [gen_permutations t] generates all permutations of [list].  If [t] contains duplicate
    values, then [gen_permutations t] will produce duplicate lists. *)
val gen_permutations : 'a t -> 'a t Quickcheck.Generator.t
