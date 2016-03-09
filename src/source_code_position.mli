(** One typically obtains a [Source_code_position.t] using a [[%here]] expression, which
    is implemented by the [ppx_here] preprocessor. *)


type t
  = Lexing.position
  (** See INRIA's OCaml documentation for a description of these fields. *)
  = { pos_fname : string
    ; pos_lnum  : int
    ; pos_bol   : int
    ; pos_cnum  : int
    }
    (** [sexp_of_t] uses the form ["FILE:LINE:COL"], and does not have a corresponding
        [of_sexp]. *)
  [@@deriving sexp_of]

include Comparable.S with type t := t
include Hashable.S   with type t := t

(** [to_string t] converts [t] to the form ["FILE:LINE:COL"]. *)
val to_string : t -> string


module Stable : sig
  module V1 : Stable_module_types.S0 with type t = t
end
