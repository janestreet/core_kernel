open! Import

type t = float [@@deriving typerep]

module Robust_compare : sig
  module type S = sig
    (** intended to be a tolerance on human-entered floats *)
    val robust_comparison_tolerance : float
    include Robustly_comparable.S with type t := float
  end

  module Make (T : sig val robust_comparison_tolerance : float end) : S
end

(** The results of robust comparisons on [nan] should be considered undefined. *)
include Robust_compare.S

module O : sig
  include module type of struct include Base.Float.O end
  include Robustly_comparable.S with type t := t
end

module Terse : sig
  type nonrec t = t [@@deriving bin_io]
  include module type of struct include Base.Float.Terse end
  with type t := t
end

include module type of struct include Base.Float end
  with type t := t
  with module O := Base.Float.O
  with module Terse := Base.Float.Terse

include Identifiable.S
  with type t := t
   and type comparator_witness := comparator_witness

include Quickcheckable.S with type t := t

(*_ Caution: If we remove this sig item, [sign] will still be present from
  [Comparable.With_zero]. *)
val sign : t -> Sign.t
  [@@deprecated "[since 2016-01] Replace [sign] with [robust_sign] or [sign_exn]"]

(** (Formerly [sign]) Uses robust comparison (so sufficiently small numbers are mapped
    to [Zero]).  Also maps nan to [Zero].  Using this function is weakly discouraged. *)
val robust_sign : t -> Sign.t

(** [gen_between ~nan ~lower_bound ~upper_bound] creates a Quickcheck generator
    producing [t] values that are either finite numbers satisfying [lower_bound] and
    [upper_bound], or NaN values satisfying the [nan] distribution.  Raises an exception
    if no values satisfy [lower_bound] and [upper_bound].  [~nan:Without] produces no
    NaN values, [~nan:With_single] produces only the single NaN value [Float.nan], and
    [~nan:With_all] produces all valid IEEE NaN values. *)
val gen_between
  :  nan : Nan_dist.t
  -> lower_bound : t Maybe_bound.t
  -> upper_bound : t Maybe_bound.t
  -> t Quickcheck.Generator.t

(** [gen_finite] produces all finite [t] values, excluding infinities and all NaN
    values. *)
val gen_finite : t Quickcheck.Generator.t

(** [gen_without_nan] produces all finite and infinite [t] values, excluding all NaN
    values. *)
val gen_without_nan : t Quickcheck.Generator.t
