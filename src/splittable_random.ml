(** This module implements "Fast Splittable Pseudorandom Number Generators" by Steele et.
    al. (1).  The paper's algorithm provides decent randomness for most purposes, but
    sacrifices cryptographic-quality randomness in favor of performance.  The original
    implementation was tested with DieHarder and BigCrush; see the paper for details.

    Our implementation is a port from Java to OCaml of the paper's algorithm.  Other than
    the choice of initial seed for [create], our port should be faithful.  We have not
    re-run the DieHarder or BigCrush tests on our implementation.  Our port is also not as
    performant as the original; two factors that hurt us are boxed [int64] values and lack
    of a POPCNT primitive.

    (1) http://2014.splashcon.org/event/oopsla2014-fast-splittable-pseudorandom-number-generators

    Beware when implementing this interface; it is easy to implement a [split] operation
    whose output is not as "independent" as it seems (2).  This bug caused problems for
    Haskell's Quickcheck library for a long time.

    (2) Schaathun, "Evaluation of splittable pseudo-random generators", JFP 2015.
    http://www.hg.schaathun.net/research/Papers/hgs2015jfp.pdf
*)

open! Import

open! Polymorphic_compare

module List  = Base.List
module Array = Base.Array
module Int64 = Caml.Int64

module Arith64 = struct
  open Int64

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( mod ) = rem
  let ( lsr ) = shift_right_logical
  let ( lsl ) = shift_left
  let ( lor ) = logor
  let ( land ) = logand
  let ( lxor ) = logxor

  let succ = succ
  let pred = pred
  let max_int = max_int
  let min_int = min_int

  let is_odd x = x lor 1L = x

  let popcount = Popcount.int64_popcount
end

open Arith64

module State = struct

  type t =
    { mutable seed : int64
    ; odd_gamma    : int64
    }

  let golden_gamma = 0x9e37_79b9_7f4a_7c15L

  let of_int seed =
    { seed      = Int64.of_int seed
    ; odd_gamma = golden_gamma
    }

  let copy { seed ; odd_gamma } = { seed ; odd_gamma }

  let mix_bits z n =
    z lxor (z lsr n)

  let mix64 z =
    let z = (mix_bits z 33) * 0xff51_afd7_ed55_8ccdL in
    let z = (mix_bits z 33) * 0xc4ce_b9fe_1a85_ec53L in
    mix_bits z 33

  let mix64_variant13 z =
    let z = (mix_bits z 30) * 0xbf58_476d_1ce4_e5b9L in
    let z = (mix_bits z 27) * 0x94d0_49bb_1331_11ebL in
    mix_bits z 31

  let mix_odd_gamma z =
    let z = (mix64_variant13 z) lor 1L in
    let n = popcount (z lxor (z lsr 1)) in
    if n >= 24
    then z lxor 0xaaaa_aaaa_aaaa_aaaaL
    else z

  let%test_unit "odd gamma" =
    for input = -1_000_000 to 1_000_000 do
      let output = mix_odd_gamma (Int64.of_int input) in
      if not (is_odd output) then
        Error.raise_s
          [%message
            "gamma value is not odd"
              (input  : int)
              (output : int64)]
    done

  let next_seed t =
    let next = t.seed + t.odd_gamma in
    t.seed <- next;
    next

  let create =
    let bits () = Int64.of_int (Core_random.bits ()) in
    let initial_seed = (bits () lsl 60) lxor (bits () lsl 30) lxor (bits ()) in
    let default_gen = ref initial_seed in
    fun () ->
      (* Note that the reference implementation uses getAndAdd here, which is distinct
         from addAndGet.  The getAndAdd operation increments a mutable integer by the
         given amount, and returns the value that it had prior to the increment. *)
      let s = !default_gen in
      default_gen := s + (golden_gamma * 2L);
      { seed      = mix64 s
      ; odd_gamma = mix_odd_gamma (s + golden_gamma)
      }

  let split t =
    let seed      = mix64         (next_seed t) in
    let odd_gamma = mix_odd_gamma (next_seed t) in
    { seed ; odd_gamma }

  let next_int64 t = mix64 (next_seed t)

  (* [perturb] is not from any external source, but provides a way to mix in external
     entropy with a pseudo-random state. *)
  let perturb t salt =
    let next = t.seed + mix64 (Int64.of_int salt) in
    t.seed <- next

end

let bool state = is_odd (State.next_int64 state)

let%test_unit "bool fairness" =
  for _ = 1 to 1_000 do
    let state = State.of_int 0 in
    let count = ref 0 in
    for _ = 1 to 1_000 do
      if bool state then incr count
    done;
    if !count < 450 || !count > 550 then
      Error.raise_s [%message "unfair boolean" (!count : int)]
  done

(* We abuse terminology and refer to individual values as biased or unbiased.  More
   properly, what is unbiased is the sampler that results if we keep only these "unbiased"
   values. *)
let remainder_is_unbiased
      ~draw
      ~remainder
      ~draw_maximum
      ~remainder_maximum
  =
  draw - remainder <= draw_maximum - remainder_maximum

let%test_unit _ =
  (* choosing a range of 10 values based on a range of 105 values *)
  let draw_maximum = 104L in
  let remainder_maximum = 9L in
  let is_unbiased draw =
    let remainder = draw mod (succ remainder_maximum) in
    remainder_is_unbiased ~draw ~remainder ~draw_maximum ~remainder_maximum
  in
  for i = 0 to 99 do
    [%test_result: bool]
      (is_unbiased (Int64.of_int i))
      ~expect:true
      ~message:(string_of_int i)
  done;
  for i = 100 to 104 do
    [%test_result: bool]
      (is_unbiased (Int64.of_int i))
      ~expect:false
      ~message:(string_of_int i)
  done

(* This implementation of bounded randomness is adapted from [Random.State.int*] in the
   OCaml standard library.  The purpose is to use the minimum number of calls to
   [next_int64] to produce a number uniformly chosen within the given range. *)
let int64 =
  let rec between state ~lo ~hi =
    let draw = State.next_int64 state in
    if lo <= draw && draw <= hi
    then draw
    else between state ~lo ~hi
  in
  let rec non_negative_up_to state maximum =
    let draw = State.next_int64 state land max_int in
    let remainder = draw mod (succ maximum) in
    if remainder_is_unbiased
         ~draw
         ~remainder
         ~draw_maximum:max_int
         ~remainder_maximum:maximum
    then remainder
    else non_negative_up_to state maximum
  in
  fun state ~lo ~hi ->
    if lo > hi then begin
      Error.raise_s [%message "int64: crossed bounds" (lo : int64) (hi : int64)]
    end;
    let diff = hi - lo in
    if diff = max_int
    then ((State.next_int64 state) land max_int) + lo
    else if diff >= 0L
    then (non_negative_up_to state diff) + lo
    else between state ~lo ~hi

let%test_module "random" =
  (module struct
    open Arith64

    let bounds =
      [ min_int
      ; min_int + 1L
      ; min_int + 2L
      ; -1_000_000_000L
      ; -1_000_000L
      ; -1_000L
      ; -100L
      ; -10L
      ; -2L
      ; -1L
      ; 0L
      ; 1L
      ; 2L
      ; 10L
      ; 100L
      ; 1_000L
      ; 1_000_000L
      ; 1_000_000_000L
      ; max_int - 2L
      ; max_int - 1L
      ; max_int
      ]

    let%test_unit "bounds" =
      let state = State.of_int 0 in
      List.iter bounds ~f:(fun lo ->
        List.iter bounds ~f:(fun hi ->
          if lo <= hi then
            for _ = 1 to 1_000 do
              let choice = int64 state ~lo ~hi in
              if choice < lo || choice > hi then
                Error.raise_s
                  [%message
                    "out of bounds"
                      (choice : int64)
                      (lo     : int64)
                      (hi     : int64)]
            done))

    let%test_unit "coverage" =
      let state = State.of_int 0 in
      List.iter [1L;10L;100L;1000L] ~f:(fun range ->
        let lo = 0L         in
        let hi = pred range in
        for _ = 1 to 100 do
          let count = Array.init (Int64.to_int range) ~f:(fun _ -> 0) in
          for _ = 1 to Int64.to_int (range * 100L) do
            let i = int64 state ~lo ~hi |> Int64.to_int in
            count.(i) <- Pervasives.succ count.(i)
          done;
          Array.iteri count ~f:(fun value count ->
            if count = 0 then
              Error.raise_s
                [%message
                  "failed to generate value"
                    (value : int)
                    (lo    : int64)
                    (hi    : int64)])
        done)

    (* This should return values with mean 0 and variance 1 if implementation
       is correct. *)
    let test_bias_of_mean ~lo ~hi ~sample_size state =
      assert (lo < hi);
      let lof = Int64.to_float lo in
      let hif = Int64.to_float hi in
      let delta = hif -. lof in
      let draw () = (Int64.to_float (int64 state ~lo ~hi) -. lof) /. delta in
      let rec loop iters accum =
        if iters <= 0L
        then accum
        else loop (iters - 1L) (accum +. draw ())
      in
      let sum = loop sample_size 0. in
      let mean = sum /. Int64.to_float sample_size in
      let n = delta +. 1. in
      (* We have n evenly spaced values from 0 to 1 inclusive, each with probability 1/n.
         This has variance (n+1)/(12(n-1)) per draw.  *)
      let standard_error =
        sqrt ((n +. 1.) /. (12. *. (n -. 1.) *. Int64.to_float sample_size))
      in
      (mean -. 0.5) /. standard_error

    let%test_unit "bias" =
      let hi = 3689348814741910528L in (* about 0.4 * max_int *)
      let z = test_bias_of_mean ~lo:0L ~hi ~sample_size:1000L (State.of_int 0) in
      assert (abs_float z < 3.)

  end)

let int state ~lo ~hi =
  let lo = Int64.of_int lo in
  let hi = Int64.of_int hi in
  Int64.to_int (int64 state ~lo ~hi)

let int32 state ~lo ~hi =
  let lo = Int64.of_int32 lo in
  let hi = Int64.of_int32 hi in
  Int64.to_int32 (int64 state ~lo ~hi)

let nativeint state ~lo ~hi =
  let lo = Int64.of_nativeint lo in
  let hi = Int64.of_nativeint hi in
  Int64.to_nativeint (int64 state ~lo ~hi)

let double_ulp = 2. ** -53.

let%test_unit _ =
  match Word_size.word_size with
  | W64 ->
    assert (1.0 -.  double_ulp         < 1.0);
    assert (1.0 -. (double_ulp /. 2.0) = 1.0)
  | W32 ->
    (* 32-bit OCaml uses a 64-bit float representation but 80-bit float instructions, so
       rounding works differently due to the conversion back and forth. *)
    assert (1.0 -.  double_ulp         <  1.0);
    assert (1.0 -. (double_ulp /. 2.0) <= 1.0)

let unit_float_from_int64 int64 =
  (Int64.to_float (int64 lsr 11)) *. double_ulp

let%test_unit _ = begin
  assert (unit_float_from_int64 0x0000_0000_0000_0000L = 0.);
  assert (unit_float_from_int64 0xffff_ffff_ffff_ffffL < 1.0);
  assert (unit_float_from_int64 0xffff_ffff_ffff_ffffL = (1.0 -. double_ulp));
end

let unit_float state =
  unit_float_from_int64 (State.next_int64 state)

(* Note about roundoff error:

   Although [float state ~lo ~hi] is nominally inclusive of endpoints, we are relying on
   the fact that [unit_float] never returns 1., because there are pairs [(lo,hi)] for
   which [lo +. 1. *. (hi -. lo) > hi].  There are also pairs [(lo,hi)] and values of [x]
   with [x < 1.] such that [lo +. x *. (hi -. lo) = hi], so it would not be correct to
   document this as being exclusive of [hi].
*)
let float state ~lo ~hi =
  lo +. (unit_float state *. (hi -. lo))

let%bench_fun "unit_float_from_int64" =
  let int64 = 1L in
  fun () -> unit_float_from_int64 int64

let%test_module "float" =
  (module struct

    let bounds =
      [ min_float
      ; -1_000_000_000.
      ; -1.
      ; -0.000_000_001
      ; 0.
      ; 0.000_000_001
      ; 1.
      ; 1_000_000_000.
      ; max_float
      ]

    let%test_unit "bounds" =
      let state = State.of_int 0 in
      List.iter bounds ~f:(fun lo ->
        List.iter bounds ~f:(fun hi ->
          if lo < hi then
            for _ = 1 to 1000 do
              let float = float state ~lo ~hi in
              if float < lo || float > hi then
                Error.raise_s
                  [%message
                    "float out of bounds"
                      (float : float)
                      (lo    : float)
                      (hi    : float)]
            done))

    let%test_unit "coverage" =
      let state = State.of_int 0 in
      List.iter bounds ~f:(fun lo ->
        List.iter bounds ~f:(fun hi ->
          if lo < hi then
            for _ = 1 to 100 do
              let hi'  = (lo *. 0.01) +. (hi *. 0.99) in
              let lo'  = (lo *. 0.99) +. (hi *. 0.01) in
              let mid1 = (lo *. 0.51) +. (hi *. 0.49) in
              let mid2 = (lo *. 0.49) +. (hi *. 0.51) in
              let saw_hi  = ref false in
              let saw_lo  = ref false in
              let saw_mid = ref false in
              for _ = 1 to 1000 do
                let float = float state ~lo ~hi in
                if float <                 lo'  then saw_lo  := true;
                if float >                 hi'  then saw_hi  := true;
                if float > mid1 && float < mid2 then saw_mid := true;
              done;
              if not (!saw_lo && !saw_mid && !saw_hi) then
                Error.raise_s
                  [%message
                    "did not get coverage of lo, mid, and hi values"
                      (lo       : float)
                      (hi       : float)
                      (!saw_lo  : bool)
                      (!saw_hi  : bool)
                      (!saw_mid : bool)]
            done))

  end)
