(* This is factored out of float.ml in order to break a module dependency cycle.  *)


module type S = sig
  (* intended to be a tolerance on human-entered floats *)
  val robust_comparison_tolerance : float
  include Robustly_comparable.S with type t := float
end

module Make(T : sig val robust_comparison_tolerance : float end) : S = struct
  let robust_comparison_tolerance = T.robust_comparison_tolerance
  let ( >=. ) x y = x >= y -. robust_comparison_tolerance
  let ( <=. ) x y = y >=. x
  let ( =. ) x y = x >=. y && y >=. x
  let ( >. ) x y = x > y +. robust_comparison_tolerance
  let ( <. ) x y = y >. x
  let ( <>. ) x y = not (x =. y)
  let robustly_compare x y =
    let d = x -. y in
    if      d < ~-. robust_comparison_tolerance then -1
    else if d >     robust_comparison_tolerance then  1
    else 0
end
