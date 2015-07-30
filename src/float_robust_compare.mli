(* This is factored out of float.ml in order to break a module dependency cycle.  *)

module type S = sig
  (** intended to be a tolerance on human-entered floats *)
  val robust_comparison_tolerance : float
  include Robustly_comparable.S with type t := float
end

module Make(T : sig val robust_comparison_tolerance : float end) : S
