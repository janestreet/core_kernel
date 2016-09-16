(** This module extends the Base [Option] module with bin_io and quickcheck *)

type 'a t = 'a Base.Option.t [@@deriving bin_io, typerep]

include module type of struct include Base.Option end with type 'a t := 'a t

include Quickcheckable.S1 with type 'a t := 'a t

