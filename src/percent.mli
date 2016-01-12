(** An abstract type of scale factors *)

open Std_internal

type t

(** of_string and t_of_sexp disallow nan, inf, etc. *)
include Stringable with type t := t
(** sexps are of the form 5bp or 0.05% or 0.0005x *)
include Sexpable   with type t := t
include Binable    with type t := t
include Comparable with type t := t
include Comparable.With_zero with type t := t

(** {6 Arithmetic} *)

include Commutative_group.S with type t := t
val ( * ) : t -> t -> t

val neg : t -> t
val abs : t -> t


val is_zero : t -> bool
val is_nan : t -> bool
val is_inf : t -> bool

(** [apply t x] multiplies the percent [t] by [x], returning a float *)
val apply : t -> float -> float

(** [scale t x] scales the percent [t] by [x], returning a new [t] *)
val scale : t -> float -> t

(** [of_mult 5.] is 5x = 500% = 50_000bp *)
val of_mult : float -> t
val to_mult : t -> float

(** [of_percentage 5.] is 5% = 0.05x = 500bp *)
val of_percentage : float -> t
val to_percentage : t -> float

(** [of_bp 5.] is 5bp = 0.05% = 0.0005x *)
val of_bp : float -> t
val to_bp : t -> float

val of_bp_int : int -> t
val to_bp_int : t -> int  (** rounds down *)

val t_of_sexp_allow_nan_and_inf : Sexp.t -> t
val of_string_allow_nan_and_inf : string -> t

val validate : t -> Validate.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]
  end
end


