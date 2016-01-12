module type S0 = sig
  type t [@@deriving bin_io, compare, sexp]
end

(** The polymorphic signatures require a mapping function so people can write conversion
    functions without either (1) re-implementing the mapping function inline or (2)
    reaching into the unstable part of the module. *)

module type S1 = sig
  type 'a t [@@deriving bin_io, compare, sexp]
  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module type S2 = sig
  type ('a1, 'a2) t [@@deriving bin_io, compare, sexp]
  val map : ('a1, 'a2) t -> f1:('a1 -> 'b1) -> f2:('a2 -> 'b2) -> ('b1, 'b2) t
end
