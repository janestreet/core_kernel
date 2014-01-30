(** For representing a day of the week. *)

type t with bin_io, sexp

include Hashable.S with type t := t
include Comparable.S with type t := t

(** [of_string s] accepts three-character abbreviations with any capitalization
*)
include Stringable.S with type t := t

val invariant : t -> unit

val sun : t
val mon : t
val tue : t
val wed : t
val thu : t
val fri : t
val sat : t

type variant = [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ]

val get : t -> variant

val create : variant -> t

(** conversion between ints and weekdays uses the same mapping as Unix.tm_wday.
    0 <-> Sun, ... 6 <-> Sat *)

(** [of_int i] returns i'th weekday if [i] is in 0,1,...,6.  Otherwise it returns
    None. *)
val of_int : int -> t option

(** [of_int_exn i] should have i in 0,1,...,6 and returns the i'th weekday. *)
val of_int_exn : int -> t

(** [to_int t] returns an int in 0,1,...6. *)
val to_int : t -> int

(** [shift t i] goes forward (or backward) the specified number of weekdays *)
val shift : t -> int -> t

(** [num_days ~from ~to_] gives the number of days that must elapse from a [from] to get
    to a [to_], i.e. the smallest non-negative number [i] such that [shift from i = to_].
*)
val num_days : from:t -> to_:t -> int

(** [is_sun_or_sat] returns true if t is Sunday or Saturday *)
val is_sun_or_sat : t -> bool

module Stable : sig
  module V1 : sig
    type nonrec t = t with bin_io, sexp, compare
  end
end

