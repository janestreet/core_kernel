module Core_bool = Bool

open Import
open Bin_prot.Std
open Typerep_lib.Std

module Bool  = Core_bool
module Int63 = Core_int63
module Int   = Core_int
module List  = Core_list

#import "config.h"

module T = struct
  type t = float [@@deriving bin_io, typerep]

  include (Base.Float : module type of struct include Base.Float end
           with type t := t
           with module O := Base.Float.O
           with module Terse := Base.Float.Terse)
end
include T
include Hashable  .Make_binable                         (T)
include Comparable.Map_and_set_binable_using_comparator (T)
module Replace_polymorphic_compare = (T : Polymorphic_compare_intf.S with type t := t)

module Robust_compare = struct
  module type S = sig
    (* intended to be a tolerance on human-entered floats *)
    val robust_comparison_tolerance : float
    include Robustly_comparable.S with type t := float
  end

  module Make(T : sig val robust_comparison_tolerance : float end) : S = struct
    (* We have test in the tree that rely on these functions not allocating, even without
       X_LIBRARY_INLING. The only way to ensure that these don't create temporary boxed
       floats without X_LIBRARY_INLING is for this code to see the float operations as
       externals, as defined in [Pervasives]. That's why we open [Caml.Pervasives]
       here. *)
    open Caml.Pervasives
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
end

module Robustly_comparable =
  Robust_compare.Make (struct let robust_comparison_tolerance = 1E-7 end)
include Robustly_comparable

module O = struct
  include Base.Float.O
  include Robustly_comparable
end

module Terse = struct
  type nonrec t = t [@@deriving bin_io]
  include (Base.Float.Terse
           : module type of struct include Base.Float.Terse end
           with type t := t)
end

let robust_sign t : Sign.t =
  if t >. 0.
  then Pos
  else if t <. 0.
  then Neg
  else Zero

(* There are two issues:
   - Float.sign used to use robust comparison, and users of [Core] might have come to
     depend on this.
   - Robustness aside, what we get from Comparable.With_zero would map nan to Neg.
*)
let sign = robust_sign

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Monad_infix

  let min_positive_subnormal_value = min_positive_subnormal_value
  let max_positive_subnormal_value = one_ulp `Down min_positive_normal_value

  let min_normal_mantissa = ieee_mantissa min_positive_normal_value
  let max_normal_mantissa = ieee_mantissa max_finite_value

  let%test_unit _ =
    [%test_result: Core_int63.t]
      min_normal_mantissa
      ~expect:Core_int63.zero

  let min_normal_exponent = ieee_exponent min_positive_normal_value
  let max_normal_exponent = ieee_exponent max_finite_value

  let min_subnormal_mantissa = ieee_mantissa min_positive_subnormal_value
  let max_subnormal_mantissa = ieee_mantissa max_positive_subnormal_value

  let%test_unit _ =
    [%test_result: Core_int63.t]
      min_subnormal_mantissa
      ~expect:Core_int63.one

  let nan_exponent = ieee_exponent nan
  let min_nan_mantissa = min_normal_mantissa |> Core_int63.succ
  let max_nan_mantissa = max_normal_mantissa

  let%test_unit _ =
    [%test_result: Core_int63.t]
      min_nan_mantissa
      ~expect:Core_int63.one

  let maybe_union option_gens =
    match List.filter_opt option_gens with
    | []   -> None
    | gens -> Some (Generator.union gens)

  let maybe_weighted_union weighted_option_gens =
    List.filter_map weighted_option_gens ~f:(fun (wt, option_gen) ->
      Option.map option_gen ~f:(fun gen ->
        (wt, gen)))
    |> function
    | []   -> None
    | gens -> Some (Generator.weighted_union gens)

  let int63_pair_lexicographic
        ~fst_lower_bound ~fst_upper_bound
        ~snd_lower_bound ~snd_upper_bound
        ~snd_start ~snd_final
    =
    let open Core_int63 in
    gen_between
      ~lower_bound:(Incl fst_lower_bound)
      ~upper_bound:(Incl fst_upper_bound)
    >>= fun fst ->
    let lower_bound = if fst = fst_lower_bound then snd_start else snd_lower_bound in
    let upper_bound = if fst = fst_upper_bound then snd_final else snd_upper_bound in
    gen_between
      ~lower_bound:(Incl lower_bound)
      ~upper_bound:(Incl upper_bound)
    >>| fun snd ->
    (fst, snd)

  let gen_nan = function
    | Nan_dist.Without     -> None
    | Nan_dist.With_single -> Some (Generator.singleton nan)
    | Nan_dist.With_all    ->
      begin
        Core_int63.gen_between
          ~lower_bound:(Incl min_nan_mantissa)
          ~upper_bound:(Incl max_nan_mantissa)
        >>= fun mantissa ->
        Core_bool.gen
        >>| fun negative ->
        let exponent = nan_exponent in
        create_ieee_exn ~negative ~exponent ~mantissa
      end
      |> Option.return

  let gen_pos_range
        ~lower_bound
        ~upper_bound
        ~min_pos_candidate
        ~max_pos_candidate
    =
    if lower_bound > max_pos_candidate ||
       upper_bound < min_pos_candidate
    then None
    else
      let lower = max lower_bound min_pos_candidate in
      let upper = min upper_bound max_pos_candidate in
      begin
        int63_pair_lexicographic
          ~fst_lower_bound:(ieee_exponent lower |> Core_int63.of_int)
          ~fst_upper_bound:(ieee_exponent upper |> Core_int63.of_int)
          ~snd_lower_bound:(ieee_mantissa min_pos_candidate)
          ~snd_upper_bound:(ieee_mantissa max_pos_candidate)
          ~snd_start:(ieee_mantissa lower)
          ~snd_final:(ieee_mantissa upper)
        >>| fun (exponent, mantissa) ->
        let negative = false in
        let exponent = Core_int63.to_int_exn exponent in
        create_ieee_exn ~negative ~exponent ~mantissa
      end
      |> Option.return

  let gen_neg_range
        ~lower_bound
        ~upper_bound
        ~min_pos_candidate
        ~max_pos_candidate
    =
    gen_pos_range
      ~lower_bound:(neg upper_bound)
      ~upper_bound:(neg lower_bound)
      ~min_pos_candidate
      ~max_pos_candidate
    |> Option.map ~f:(Generator.map ~f:neg)

  let gen_range
        ~lower_bound
        ~upper_bound
        ~min_pos_candidate
        ~max_pos_candidate
    =
    maybe_union
      [ gen_pos_range
          ~lower_bound
          ~upper_bound
          ~min_pos_candidate
          ~max_pos_candidate
      ; gen_neg_range
          ~lower_bound
          ~upper_bound
          ~min_pos_candidate
          ~max_pos_candidate
      ]

  let gen_zero ~lower_bound ~upper_bound =
    gen_range ~lower_bound ~upper_bound
      ~min_pos_candidate:zero
      ~max_pos_candidate:zero

  let gen_subnormal ~lower_bound ~upper_bound =
    gen_range ~lower_bound ~upper_bound
      ~min_pos_candidate:min_positive_subnormal_value
      ~max_pos_candidate:max_positive_subnormal_value

  let gen_normal ~lower_bound ~upper_bound =
    gen_range ~lower_bound ~upper_bound
      ~min_pos_candidate:min_positive_normal_value
      ~max_pos_candidate:max_finite_value

  let gen_infinity ~lower_bound ~upper_bound =
    gen_range ~lower_bound ~upper_bound
      ~min_pos_candidate:infinity
      ~max_pos_candidate:infinity

  let gen_between_inclusive ~nan ~lower_bound ~upper_bound =
    assert (lower_bound <= upper_bound);
    maybe_weighted_union
      [ 20., gen_normal    ~lower_bound ~upper_bound
      ; 10., gen_subnormal ~lower_bound ~upper_bound
      ;  1., gen_zero      ~lower_bound ~upper_bound
      ;  1., gen_infinity  ~lower_bound ~upper_bound
      ;  1., gen_nan       nan
      ]
    |> Option.value_exn ~message:"Float.gen_between: no values satisfy given constraints"

  let gen =
    gen_between_inclusive
      ~nan:With_single
      ~lower_bound:neg_infinity
      ~upper_bound:infinity

  let gen_between ~nan ~lower_bound ~upper_bound =
    match
      (lower_bound : float Maybe_bound.t),
      (upper_bound : float Maybe_bound.t)
    with
    | (Excl lower, _) | (Incl lower, _) when is_nan lower ->
      failwith "Float.gen_between: lower bound = NaN"
    | (_, Excl upper) | (_, Incl upper) when is_nan upper ->
      failwith "Float.gen_between: upper bound = NaN"
    | Excl lower, _ when equal lower infinity ->
      failwith "Float.gen_between: lower bound > infinity"
    | _, Excl upper when equal upper neg_infinity ->
      failwith "Float.gen_between: upper bound < -infinity"
    | _ ->
      let lower_inclusive =
        match lower_bound with
        | Unbounded        -> neg_infinity
        | Incl lower_bound -> lower_bound
        | Excl lower_bound -> one_ulp `Up lower_bound
      in
      let upper_inclusive =
        match upper_bound with
        | Unbounded        -> infinity
        | Incl upper_bound -> upper_bound
        | Excl upper_bound -> one_ulp `Down upper_bound
      in
      if lower_inclusive > upper_inclusive then
        Error.failwiths "Float.gen_between: crossed bounds"
          (`lower_bound lower_bound, `upper_bound upper_bound)
          [%sexp_of: [`lower_bound of t Maybe_bound.t] *
                     [`upper_bound of t Maybe_bound.t]];
      gen_between_inclusive ~nan
        ~lower_bound:lower_inclusive
        ~upper_bound:upper_inclusive

  let gen_without_nan =
    gen_between
      ~nan:Without
      ~lower_bound:Unbounded
      ~upper_bound:Unbounded

  let gen_finite =
    gen_between
      ~nan:Without
      ~lower_bound:(Excl neg_infinity)
      ~upper_bound:(Excl infinity)

  (* obs_{zero,subnormal,normal,infinite,nan} are observers that
     distinguish floats already known to classify as their names suggest. E.g.,
     [obs_zero] only distinguishes between positive and negative zero.  *)

  let obs_zero =
    Observer.unmap Bool.obs
      ~f:ieee_negative

  let obs_subnormal =
    let mantissa =
      Core_int63.obs_between
        ~lower_bound:(Incl min_subnormal_mantissa)
        ~upper_bound:(Incl max_subnormal_mantissa)
    in
    Observer.unmap (Observer.tuple2 Bool.obs mantissa)
      ~f:(fun float ->
        ieee_negative float,
        ieee_mantissa float)

  let obs_normal =
    let exponent =
      Core_int.obs_between
        ~lower_bound:(Incl min_normal_exponent)
        ~upper_bound:(Incl max_normal_exponent)
    in
    let mantissa =
      Core_int63.obs_between
        ~lower_bound:(Incl min_normal_mantissa)
        ~upper_bound:(Incl max_normal_mantissa)
    in
    Observer.unmap (Observer.tuple3 Bool.obs exponent mantissa)
      ~f:(fun float ->
        ieee_negative float,
        ieee_exponent float,
        ieee_mantissa float)

  let obs_infinite =
    Observer.unmap Bool.obs
      ~f:ieee_negative

  let obs_nan =
    Observer.singleton ()

  let obs =
    Observer.unmap
      (Observer.variant5
         obs_zero
         obs_subnormal
         obs_normal
         obs_infinite
         obs_nan)
      ~f:(fun float ->
        match classify float with
        | Zero      -> `A float
        | Subnormal -> `B float
        | Normal    -> `C float
        | Infinite  -> `D float
        | Nan       -> `E float)

  let shrinker : float Shrinker.t =
    Shrinker.empty ()

end

let gen             = For_quickcheck.gen
let gen_between     = For_quickcheck.gen_between
let gen_without_nan = For_quickcheck.gen_without_nan
let gen_finite      = For_quickcheck.gen_finite
let obs             = For_quickcheck.obs
let shrinker        = For_quickcheck.shrinker

(* Additional tests of Base.Float requiring the Gc module *)

#ifdef JSC_ARCH_SIXTYFOUR
let%test _ =
  let before = Core_gc.minor_words () in
  assert (Int63.equal (int63_round_nearest_exn 0.8) (Int63.of_int_exn 1));
  let after = Core_gc.minor_words () in
  Int.equal before after
#endif

let%test_unit "Float.validate_positive doesn't allocate on success" =
  let initial_words = Core_gc.minor_words () in
  let _ : Validate.t = validate_positive 1. in
  let allocated = Int.(-) (Core_gc.minor_words ()) initial_words in
  [%test_result: int] allocated ~expect:0
;;
