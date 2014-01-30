(** One typically obtains a [Source_code_position.t] using a [_here_] expression, which is
    implemented by the [pa_here] preprocessor. *)

open Sexplib

type t = Lexing.position with bin_io, sexp

type t_hum = t with bin_io, compare, sexp_of

include Comparable.S with type t := t
include Hashable.S   with type t := t

(** [to_string t] converts [t] to the form ["FILE:LINE:COL"]. *)
val to_string : t -> string

