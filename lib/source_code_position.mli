open Sexplib

type t = Lexing.position with bin_io, sexp

type t_hum = t with bin_io, compare, sexp_of

include Comparable.S with type t := t
include Hashable.S   with type t := t

(** [to_sexp_hum] is the same as [sexp_of_t_hum].  It is needed because the [<:test< >>]
    macro generates it. *)
val to_sexp_hum : t -> Sexp.t

val to_string : t -> string

