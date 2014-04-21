open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std
open Common

module T = struct
  type t = int with bin_io, sexp, typerep

  (* According to estokes,
     if i = j then 0 else if i < j then -1 else 1
     is only slightly faster, so we've decided to stick with
     Pervasives.compare *)
  let compare (x : t) y = compare x y
  let hash (x : t) = if x >= 0 then x else ~-x

  let of_string s =
    try
      int_of_string s
    with
    | _ -> failwithf "Int.of_string: %S" s ()

  let to_string = string_of_int
end

include T

let num_bits = Word_size.num_bits Word_size.word_size - 1

let of_float = Float.to_int
let to_float = Float.of_int

module Replace_polymorphic_compare = struct
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let compare = compare
  let ascending = compare
  let descending x y = compare y x
  let equal (x : t) y = x = y
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( =  ) (x : t) y = x =  y
  let ( >  ) (x : t) y = x >  y
  let ( <  ) (x : t) y = x <  y
  let ( <> ) (x : t) y = x <> y
  let between t ~low ~high = low <= t && t <= high
  let _squelch_unused_module_warning_ = ()
end

include Replace_polymorphic_compare

include Hashable.Make_binable (T)
include Comparable.Map_and_set_binable (T)

let zero = 0
let one = 1
let minus_one = -1

include Comparable.Validate_with_zero (struct
  include T
  let zero = zero
end)

let pred i = i - 1
let succ i = i + 1

let to_int i = i
let to_int_exn = to_int
let of_int i = i
let of_int_exn = of_int

let max_value = Pervasives.max_int
let min_value = Pervasives.min_int

module Conv = Int_conversions
let of_int32 = Conv.int32_to_int
let of_int32_exn = Conv.int32_to_int_exn
let to_int32 = Conv.int_to_int32
let to_int32_exn = Conv.int_to_int32_exn
let of_int64 = Conv.int64_to_int
let of_int64_exn = Conv.int64_to_int_exn
let to_int64 = Conv.int_to_int64
let of_nativeint = Conv.nativeint_to_int
let of_nativeint_exn = Conv.nativeint_to_int_exn
let to_nativeint = Conv.int_to_nativeint
let to_nativeint_exn = to_nativeint

include Conv.Make (T)

let abs x = abs x

let ( + ) x y = ( + ) x y
let ( - ) x y = ( - ) x y
let ( * ) x y = ( * ) x y
let ( / ) x y = ( / ) x y

let neg x = -x
let ( ~- ) = neg

TEST = (neg 5 + 5 = 0)

(* note that rem is not same as % *)
let rem a b = a mod b

let incr = Pervasives.incr
let decr = Pervasives.decr

let shift_right a b = a asr b
let shift_right_logical a b = a lsr b
let shift_left a b = a lsl b
let bit_not a = lnot a
let bit_or a b = a lor b
let bit_and a b = a land b
let bit_xor a b = a lxor b

let pow = Int_math.int_pow
TEST = pow min_value 1 = min_value
TEST = pow max_value 1 = max_value

include Int_pow2

include Pretty_printer.Register (struct
  type nonrec t = t
  let to_string = to_string
  let module_name = "Core.Std.Int"
end)

module Pre_O = struct
  let ( + ) = ( + )
  let ( - ) = ( - )
  let ( * ) = ( * )
  let ( / ) = ( / )
  let ( ~- ) = ( ~- )
  include (Replace_polymorphic_compare : Polymorphic_compare_intf.Infix with type t := t)
  let abs = abs
  let neg = neg
  let zero = zero
  let of_int_exn = of_int_exn
end

module O = struct
  include Pre_O
  module F = Int_math.Make (struct
    type nonrec t = t
    include Pre_O
    let rem = rem
    let to_float = to_float
    let of_float = of_float
    let of_string = T.of_string
    let to_string = T.to_string
  end)
  include F

  (* These inlined versions of (%), (/%), and (//) perform better than their functorized
     counterparts in [F] (see benchmarks below).

     The reason these functions are inlined in [Int] but not in any of the other integer
     modules is that they existed in [Int] and [Int] alone prior to the introduction of
     the [Int_math.Make] functor, and we didn't want to degrade their performance.

     We won't pre-emptively do the same for new functions, unless someone cares, on a case
     by case fashion.  *)

  let ( % ) x y =
    if y <= zero then
      invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string x) (to_string y) ();
    let rval = rem x y in
    if rval < zero
    then rval + y
    else rval
  ;;

  let ( /% ) x y =
    if y <= zero then
      invalid_argf
        "%s /%% %s in core_int.ml: divisor should be positive"
        (to_string x) (to_string y) ();
    if x < zero
    then (x + one) / y - one
    else x / y
  ;;

  let (//) x y = to_float x /. to_float y
  ;;
end

BENCH_MODULE "Core_int_inline_ops" = struct
  (* The [of_string] and [Random.bool] are so that the values won't get inlined. *)
  let small = of_string "37"
  let big   = of_string "123456789"

  let max = if Random.bool () then max_value else max_value
  let min = if Random.bool () then min_value else min_value

  BENCH "inlined  % 01" = O.(%)    big small
  BENCH "functor  % 01" = O.F.(%)  big small
  BENCH "inlined /% 01" = O.(/%)   big small
  BENCH "functor /% 01" = O.F.(/%) big small
  BENCH "inlined // 01" = O.(//)   big small
  BENCH "functor // 01" = O.F.(//) big small

  BENCH "inlined  % 11" = O.(%)    small big
  BENCH "functor  % 11" = O.F.(%)  small big
  BENCH "inlined /% 11" = O.(/%)   small big
  BENCH "functor /% 11" = O.F.(/%) small big
  BENCH "inlined // 11" = O.(//)   small big
  BENCH "functor // 11" = O.F.(//) small big

  BENCH "inlined  % 21" = O.(%)    max small
  BENCH "functor  % 21" = O.F.(%)  max small
  BENCH "inlined /% 21" = O.(/%)   max small
  BENCH "functor /% 21" = O.F.(/%) max small
  BENCH "inlined // 21" = O.(//)   max small
  BENCH "functor // 21" = O.F.(//) max small

  BENCH "inlined  % 31" = O.(%)    min small
  BENCH "functor  % 31" = O.F.(%)  min small
  BENCH "inlined /% 31" = O.(/%)   min small
  BENCH "functor /% 31" = O.F.(/%) min small
  BENCH "inlined // 31" = O.(//)   min small
  BENCH "functor // 31" = O.F.(//) min small

  BENCH "inlined  % 41" = O.(%)    max big
  BENCH "functor  % 41" = O.F.(%)  max big
  BENCH "inlined /% 41" = O.(/%)   max big
  BENCH "functor /% 41" = O.F.(/%) max big
  BENCH "inlined // 41" = O.(//)   max big
  BENCH "functor // 41" = O.F.(//) max big

  BENCH "inlined  % 51" = O.(%)    min big
  BENCH "functor  % 51" = O.F.(%)  min big
  BENCH "inlined /% 51" = O.(/%)   min big
  BENCH "functor /% 51" = O.F.(/%) min big
  BENCH "inlined // 51" = O.(//)   min big
  BENCH "functor // 51" = O.F.(//) min big
end

(*
Estimated testing time 6m (36 benchmarks x 10s). Change using -quota SECS.
┌─────────────────────────────────────────────────┬──────────┬─────────┬────────────┐
│ Name                                            │ Time/Run │ mWd/Run │ Percentage │
├─────────────────────────────────────────────────┼──────────┼─────────┼────────────┤
│ [core_int.ml:Core_int_inline_ops] inlined  % 01 │  19.89ns │         │     63.76% │
│ [core_int.ml:Core_int_inline_ops] functor  % 01 │  25.45ns │         │     81.58% │
│ [core_int.ml:Core_int_inline_ops] inlined /% 01 │  18.26ns │         │     58.54% │
│ [core_int.ml:Core_int_inline_ops] functor /% 01 │  23.72ns │         │     76.03% │
│ [core_int.ml:Core_int_inline_ops] inlined // 01 │   8.16ns │   2.00w │     26.16% │
│ [core_int.ml:Core_int_inline_ops] functor // 01 │  12.27ns │   6.00w │     39.34% │
│ [core_int.ml:Core_int_inline_ops] inlined  % 11 │  17.24ns │         │     55.26% │
│ [core_int.ml:Core_int_inline_ops] functor  % 11 │  23.86ns │         │     76.48% │
│ [core_int.ml:Core_int_inline_ops] inlined /% 11 │  17.10ns │         │     54.81% │
│ [core_int.ml:Core_int_inline_ops] functor /% 11 │  22.08ns │         │     70.77% │
│ [core_int.ml:Core_int_inline_ops] inlined // 11 │   8.13ns │   2.00w │     26.06% │
│ [core_int.ml:Core_int_inline_ops] functor // 11 │  12.20ns │   6.00w │     39.11% │
│ [core_int.ml:Core_int_inline_ops] inlined  % 21 │  21.37ns │         │     68.50% │
│ [core_int.ml:Core_int_inline_ops] functor  % 21 │  27.67ns │         │     88.69% │
│ [core_int.ml:Core_int_inline_ops] inlined /% 21 │  19.60ns │         │     62.82% │
│ [core_int.ml:Core_int_inline_ops] functor /% 21 │  25.78ns │         │     82.64% │
│ [core_int.ml:Core_int_inline_ops] inlined // 21 │   8.13ns │   2.00w │     26.06% │
│ [core_int.ml:Core_int_inline_ops] functor // 21 │  12.18ns │   6.00w │     39.05% │
│ [core_int.ml:Core_int_inline_ops] inlined  % 31 │  22.94ns │         │     73.53% │
│ [core_int.ml:Core_int_inline_ops] functor  % 31 │  31.20ns │         │    100.00% │
│ [core_int.ml:Core_int_inline_ops] inlined /% 31 │  20.74ns │         │     66.48% │
│ [core_int.ml:Core_int_inline_ops] functor /% 31 │  30.94ns │         │     99.18% │
│ [core_int.ml:Core_int_inline_ops] inlined // 31 │   8.14ns │   2.00w │     26.08% │
│ [core_int.ml:Core_int_inline_ops] functor // 31 │  12.25ns │   6.00w │     39.25% │
│ [core_int.ml:Core_int_inline_ops] inlined  % 41 │  20.75ns │         │     66.50% │
│ [core_int.ml:Core_int_inline_ops] functor  % 41 │  26.49ns │         │     84.91% │
│ [core_int.ml:Core_int_inline_ops] inlined /% 41 │  18.89ns │         │     60.55% │
│ [core_int.ml:Core_int_inline_ops] functor /% 41 │  24.83ns │         │     79.59% │
│ [core_int.ml:Core_int_inline_ops] inlined // 41 │   8.14ns │   2.00w │     26.10% │
│ [core_int.ml:Core_int_inline_ops] functor // 41 │  12.12ns │   6.00w │     38.85% │
│ [core_int.ml:Core_int_inline_ops] inlined  % 51 │  21.57ns │         │     69.15% │
│ [core_int.ml:Core_int_inline_ops] functor  % 51 │  29.50ns │         │     94.56% │
│ [core_int.ml:Core_int_inline_ops] inlined /% 51 │  20.03ns │         │     64.21% │
│ [core_int.ml:Core_int_inline_ops] functor /% 51 │  29.15ns │         │     93.45% │
│ [core_int.ml:Core_int_inline_ops] inlined // 51 │   8.14ns │   2.00w │     26.08% │
│ [core_int.ml:Core_int_inline_ops] functor // 51 │  12.12ns │   6.00w │     38.85% │
└─────────────────────────────────────────────────┴──────────┴─────────┴────────────┘
*)

include O (* [Int] and [Int.O] agree value-wise *)
