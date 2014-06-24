open Typerep_lib.Std
open Sexplib.Std
open Bin_prot.Std
open Result.Export
module List = ListLabels
module Sexp = Sexplib.Sexp
module String = Core_string
open Core_printf

type 'a bound = 'a Comparable.bound = Incl of 'a | Excl of 'a | Unbounded

let failwiths = Error.failwiths

module T = struct
  type t = float with sexp, bin_io, typerep
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  external hash : float -> int = "caml_hash_double" "noalloc"

  TEST_UNIT =
    List.iter ~f:(fun float -> assert (hash float = Caml.Hashtbl.hash float))
      [ 0.926038888360971146
      ; 34.1638588598232076
      ]
  ;;

end

include T
type outer = t with sexp, bin_io, typerep (* alias for use by sub-modules *)

let to_float x = x
let of_float x = x

let of_string s =
  try Pervasives.float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

external format_float : string -> float -> string = "caml_format_float"

(* Stolen from [pervasives.ml].  Adds a "." at the end if needed.  It is in
   [pervasives.mli], but it also says not to use it directly, so we copy and paste the
   code. It makes the assumption on the string passed in argument that it was returned by
   [format_float] *)
let valid_float_lexem s =
  let l = String.length s in
  let rec loop i =
    if i >= l then s ^ "." else
    match s.[i] with
    | '0' .. '9' | '-' -> loop (i + 1)
    | _ -> s
  in
  loop 0
;;

(* Standard 12 significant digits, exponential notation used as necessary, guaranteed to
   be a valid OCaml float lexem, not to look like an int.  *)
let to_string x = valid_float_lexem (format_float "%.12g" x);;

(* Let [y] be a power of 2.  Then the next representable float is:
     [z = y * (1 + 2 ** -52)]
   and the previous one is
     [x = y * (1 - 2 ** -53)]

   In general, every two adjacent floats are within a factor of between [1 + 2**-53]
   and [1 + 2**-52] from each other, that is within [1 + 1.1e-16] and [1 + 2.3e-16].

   So if the decimal representation of a float starts with "1", then its adjacent floats
   will usually differ from it by 1, and sometimes by 2, at the 17th significant digit
   (counting from 1).

   On the other hand, if the decimal representation starts with "9", then the adjacent
   floats will be off by no more than 23 at the 16th and 17th significant digits.

   E.g.:

   # sprintf "%.17g" (1024. *. (1. -. 2.** (-53.)));;
                           11111111
                 1234 5678901234567
   - : string = "1023.9999999999999"

   Printing a couple of extra digits reveals that the difference indeed is roughly 11 at
   digits 17th and 18th (that is, 13th and 14th after "."):

   # sprintf "%.19g" (1024. *. (1. -. 2.** (-53.)));;
                           1111111111
                 1234 567890123456789
   - : string = "1023.999999999999886"

   The ulp (the difference between adjacent floats) is twice as big on the other side of
   1024.:

   # sprintf "%.19g" (1024. *. (1. +. 2.** (-52.)));;
                           1111111111
                 1234 567890123456789
   - : string = "1024.000000000000227"

   Now take a power of 2 which starts with 99:

   # 2.**93. ;;
                        1111111111
               1 23456789012345678
   - : float = 9.9035203142830422e+27

   # 2.**93. *. (1. +. 2.** (-52.));;
   - : float = 9.9035203142830444e+27

   # 2.**93. *. (1. -. 2.** (-53.));;
   - : float = 9.9035203142830411e+27

   The difference between 2**93 and its two neighbors is slightly more than, respectively,
   1 and 2 at significant digit 16.

   Those examples show that:
    - 17 significant digits is always sufficient to represent a float without ambiguity
    - 15th significant digit can always be represented accurately
    - converting a decimal number with 16 significant digits to its nearest float and back
      can change the last decimal digit by no more than 1

   To make sure that floats obtained by conversion from decimal fractions (e.g. "3.14")
   are printed without trailing non-zero digits, one should choose the first among the
   '%.15g', '%.16g', and '%.17g' representations which does round-trip:

   # sprintf "%.15g" 3.14;;
   - : string = "3.14"                     (* pick this one *)
   # sprintf "%.16g" 3.14;;
   - : string = "3.14"
   # sprintf "%.17g" 3.14;;
   - : string = "3.1400000000000001"       (* do not pick this one *)

   # sprintf "%.15g" 8.000000000000002;;
   - : string = "8"                        (* do not pick this one--does not round-trip *)
   # sprintf "%.16g" 8.000000000000002;;
   - : string = "8.000000000000002"        (* prefer this one *)
   # sprintf "%.17g" 8.000000000000002;;
   - : string = "8.0000000000000018"       (* this one has one digit of junk at the end *)

   Skipping the '%.16g' in the above procedure saves us some time, but it means that, as
   seen in the second example above, occasionally numbers with exactly 16 significant
   digits will have an error introduced at the 17th digit.  That is probably OK for
   typical use, because a number with 16 significant digits is "ugly" already.  Adding one
   more doesn't make it much worse for a human reader.

   On the other hand, we cannot skip '%.15g' and only look at '%.16g' and '%.17g', since
   the inaccuracy at the 16th digit might introduce the noise we want to avoid:

   # sprintf "%.15g" 9.992;;
   - : string = "9.992"                    (* pick this one *)
   # sprintf "%.16g" 9.992;;
   - : string = "9.992000000000001"        (* do not pick this one--junk at the end *)
   # sprintf "%.17g" 9.992;;
   - : string = "9.9920000000000009"
*)
let to_string_round_trippable x =
  valid_float_lexem (
    let y = format_float "%.15g" x in
    if float_of_string y = x then
      y
    else
      format_float "%.17g" x)
;;

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float

let min_positive_subnormal_value = 2. ** -1074.
let min_positive_normal_value = 2. ** -1022.

let is_nan x = (x : t) <> x

(* An order-preserving bijection between all floats except for NaNs, and 99.95% of
   int64s.

   Note we don't distinguish 0. and -0. as separate values here, they both map to 0L, which
   maps back to 0.

   This should work both on little-endian and high-endian CPUs.  Wikipedia says: "on
   modern standard computers (i.e., implementing IEEE 754), one may in practice safely
   assume that the endianness is the same for floating point numbers as for integers"
   (http://en.wikipedia.org/wiki/Endianness#Floating-point_and_endianness).
*)
let to_int64_preserve_order t =
  if is_nan t then
    None
  else
  if t = 0. then (* also includes -0. *)
    Some 0L
  else
  if t > 0. then
    Some (Int64.bits_of_float t)
  else
    Some (Int64.neg (Int64.bits_of_float (~-. t)))
;;

let to_int64_preserve_order_exn x =
  Option.value_exn (to_int64_preserve_order x)
;;

let of_int64_preserve_order x =
    if x >= 0L then
      Int64.float_of_bits x
    else
      ~-. (Int64.float_of_bits (Int64.neg x))
;;

let one_ulp dir t =
  match to_int64_preserve_order t with
  | None -> nan
  | Some x ->
    of_int64_preserve_order (Int64.add x (match dir with `Up -> 1L | `Down -> -1L))
;;

TEST_MODULE = struct

  let test_both_ways a b =
    to_int64_preserve_order_exn a = b && of_int64_preserve_order b = a
  ;;

  TEST = test_both_ways          0.  0L
  TEST = test_both_ways        (-0.) 0L
  TEST = test_both_ways          1.  (Int64.of_int (   1023 lsl 52))
  TEST = test_both_ways        (-2.) (Int64.of_int (- (1024 lsl 52)))
  TEST = test_both_ways     infinity (Int64.shift_left 2047L 52)
  TEST = test_both_ways neg_infinity (Int64.neg (Int64.shift_left 2047L 52))

  TEST = one_ulp `Down infinity = max_finite_value
  TEST = is_nan (one_ulp `Up infinity)
  TEST = is_nan (one_ulp `Down neg_infinity)
  TEST = one_ulp `Up neg_infinity = ~-. max_finite_value

  (* Some tests to make sure that the compiler is generating code for handling subnormal
     numbers at runtime accurately. *)
  let x () = min_positive_subnormal_value
  let y () = min_positive_normal_value

  TEST = test_both_ways  (x ())  1L
  TEST = test_both_ways  (y ())  (Int64.of_int  (1 lsl 52))

  TEST = x () > 0.
  TEST = x () /. 2. = 0.

  TEST = one_ulp `Up 0. = x ()
  TEST = one_ulp `Down 0. = ~-. (x ())

  let are_one_ulp_apart a b = one_ulp `Up a = b

  TEST = are_one_ulp_apart (x ()) (2. *. x ())
  TEST = are_one_ulp_apart (2. *. x ()) (3. *. x ())

  let one_ulp_below_y () = y () -. x ()
  TEST = one_ulp_below_y () < y ()
  TEST = y () -. one_ulp_below_y () = x ()
  TEST = are_one_ulp_apart (one_ulp_below_y ()) (y ())

  let one_ulp_above_y () = y () +. x ()
  TEST = y () < one_ulp_above_y ()
  TEST = one_ulp_above_y () -. y () = x ()
  TEST = are_one_ulp_apart (y ()) (one_ulp_above_y ())

  TEST = not (are_one_ulp_apart (one_ulp_below_y ()) (one_ulp_above_y ()))

  (* [2 * min_positive_normal_value] is where the ulp increases for the first time. *)
  let z () = 2. *. y ()
  let one_ulp_below_z () = z () -. x ()
  TEST = one_ulp_below_z () < z ()
  TEST = z () -. one_ulp_below_z () = x ()
  TEST = are_one_ulp_apart (one_ulp_below_z ()) (z ())

  let one_ulp_above_z () = z () +. 2. *. x ()
  TEST = z () < one_ulp_above_z ()
  TEST = one_ulp_above_z () -. z () = 2. *. x ()
  TEST = are_one_ulp_apart (z ()) (one_ulp_above_z ())

end

let zero = 0.

TEST = to_string_round_trippable 3.14                             = "3.14"
TEST = to_string_round_trippable 3.1400000000000001               = "3.14"
TEST = to_string_round_trippable 3.1400000000000004               = "3.1400000000000006"
TEST = to_string_round_trippable 8.000000000000002                = "8.0000000000000018"
TEST = to_string_round_trippable 9.992                            = "9.992"
TEST = to_string_round_trippable (2.**63. *. (1. +. 2.** (-52.))) = "9.2233720368547779e+18"
TEST = to_string_round_trippable (-3.)                            = "-3."
TEST = to_string_round_trippable nan                              = "nan"
TEST = to_string_round_trippable infinity                         = "inf"
TEST = to_string_round_trippable neg_infinity                     = "-inf"
TEST = to_string_round_trippable 3e100                            = "3e+100"
TEST = to_string_round_trippable max_finite_value                 = "1.7976931348623157e+308"
TEST = to_string_round_trippable min_positive_subnormal_value     = "4.94065645841247e-324"

let frexp = Pervasives.frexp
let ldexp = Pervasives.ldexp

module Robustly_comparable = Float_robust_compare.Make (struct let epsilon = 1E-7 end)
include Robustly_comparable

let epsilon_float = Pervasives.epsilon_float

include Hashable.Make_binable (T)

let of_int = Pervasives.float_of_int

let to_int f =
  let module P = Pervasives in
  match P.classify_float f with
  | P.FP_normal | P.FP_subnormal | P.FP_zero -> int_of_float f
  | P.FP_infinite | P.FP_nan -> invalid_arg "Float.to_int on nan or inf"

let of_int64 i = Int64.to_float i

let to_int64 f =
  let module P = Pervasives in
  match P.classify_float f with
  | P.FP_normal | P.FP_subnormal | P.FP_zero -> Int64.of_float f
  | P.FP_infinite | P.FP_nan -> invalid_arg "Float.to_int64 on nan or inf"

(* max_int/min_int are architecture dependent, e.g. +/- 2^30, +/- 2^62 if 32-bit, 64-bit
   (respectively) while float is IEEE standard for double (52 significant bits).

   In both cases, we want to guarantee that if [iround_lbound <= x <= iround_ubound],
   then [int_of_float x] fits in an int.  2.0 ** 62.0 -. 512. is the greatest
   representable double <= max_int on a 64-bit box, so we choose that for the upper
   bound.  For the lower bound, [min_int] is already representable, so we use that.

   Minor point: [iround_lbound] and [iround_ubound] are integers (in the mathematical
   sense), so if [iround_lbound <= t <= iround_ubound], then
   [iround_lbound <= floor t <= ceil t <= iround_ubound].
*)
let iround_lbound = of_int min_int
let iround_ubound = min (of_int max_int) (2.0 ** 62.0 -. 512.)

(* The performance of the "exn" rounding functions is important, so they are written
   out separately, and tuned individually.  (We could have the option versions call
   the "exn" versions, but that imposes arguably gratuitous overhead---especially
   in the case where the capture of backtraces is enabled upon "with"---and that seems
   not worth it when compared to the relatively small amount of code duplication.) *)

let iround_up t =
  if t > 0.0 then begin
    if t <= iround_ubound then
      Some (int_of_float (ceil t))
    else
      None
  end
  else begin
    if t >= iround_lbound then
      Some (int_of_float t)
    else
      None
  end

let iround_up_exn t =
  if t > 0.0 then begin
    if t <= iround_ubound then
      int_of_float (ceil t)
    else
      invalid_argf "Float.iround_up_exn: argument (%f) is too large" t ()
  end
  else begin
    if t >= iround_lbound then
      int_of_float t
    else
      invalid_argf "Float.iround_up_exn: argument (%f) is too small or NaN" t ()
  end

let iround_down t =
  if t >= 0.0 then begin
    if t <= iround_ubound then
      Some (int_of_float t)
    else
      None
  end
  else begin
    if t >= iround_lbound then
      Some (int_of_float (floor t))
    else
      None
  end

let iround_down_exn t =
  if t >= 0.0 then begin
    if t <= iround_ubound then
      int_of_float t
    else
      invalid_argf "Float.iround_down_exn: argument (%f) is too large" t ()
  end
  else begin
    if t >= iround_lbound then
      int_of_float (floor t)
    else
      invalid_argf "Float.iround_down_exn: argument (%f) is too small or NaN" t ()
  end

let iround_towards_zero t =
  if t >= iround_lbound && t <= iround_ubound then
    Some (int_of_float t)
  else
    None

let iround_towards_zero_exn t =
  if t >= iround_lbound && t <= iround_ubound then
    int_of_float t
  else
    invalid_argf
      "Float.iround_towards_zero_exn: argument (%f) is out of range or NaN" t ()

(* Outside of the range [round_nearest_lb..round_nearest_ub], all representable doubles
   are integers in the mathematical sense, and [round_nearest] should be identity.

   However, for odd numbers with the absolute value between 2**52 and 2**53, the formula
   [round_nearest x = floor (x + 0.5)] does not hold:

   # let naive_round_nearest x = floor (x +. 0.5);;
   # let x = 2. ** 52. +. 1.;;
   val x : float = 4503599627370497.
   # naive_round_nearest x;;
   - :     float = 4503599627370498.
*)
let round_nearest_lb = -.(2. ** 52.)
let round_nearest_ub =    2. ** 52.

let iround_nearest t =
  if t >= 0. then
    if t <= round_nearest_ub then
      Some (int_of_float (t +. 0.5))
    else
      if t <= iround_ubound then
        Some (int_of_float t)
      else
        None
  else
    if t >= round_nearest_lb then
      Some (int_of_float (floor (t +. 0.5)))
    else
      if t >= iround_lbound then
        Some (int_of_float t)
      else
        None

let iround_nearest_exn t =
  if t >= 0. then
    if t <= round_nearest_ub then
      int_of_float (t +. 0.5)
    else
      if t <= iround_ubound then
        int_of_float t
      else
        invalid_argf "Float.iround_nearest_exn: argument (%f) is too large" t ()
  else
    if t >= round_nearest_lb then
      int_of_float (floor (t +. 0.5))
    else
      if t >= iround_lbound then
        int_of_float t
      else
        invalid_argf "Float.iround_nearest_exn: argument (%f) is too small or NaN" t ()

(* The following [iround_exn] and [iround] functions are slower than the ones above.
   Their equivalence to those functions is tested in the unit tests below. *)

let iround_exn ?(dir=`Nearest) t =
  match dir with
  | `Zero    -> iround_towards_zero_exn t
  | `Nearest -> iround_nearest_exn t
  | `Up      -> iround_up_exn t
  | `Down    -> iround_down_exn t

let iround ?(dir=`Nearest) t =
  try Some (iround_exn ~dir t)
  with _ -> None

let is_inf x = (Pervasives.classify_float x = Pervasives.FP_infinite);;

let min_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x < y then x else y

let max_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x > y then x else y

let add = (+.)
let sub = (-.)
let neg = (~-.)
let abs = Pervasives.abs_float
let scale = ( *. )

let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

module Parts : sig
  type t

  val fractional : t -> float
  val integral : t -> float
  val modf : float -> t
end = struct
  type t = float * float

  let fractional t = fst t
  let integral t = snd t
  let modf = modf
end
let modf = Parts.modf

let round_down = floor
TEST =
  round_down      3.6  =  3.
  && round_down (-3.6) = -4.

let round_up = ceil
TEST =
  round_up      3.6  =  4.
  && round_up (-3.6) = -3.

let round_towards_zero t =
  if t >= 0.
  then round_down t
  else round_up   t
TEST =
  round_towards_zero      3.6  =  3.
  && round_towards_zero (-3.6) = -3.

(* see the comment above [round_nearest_lb] and [round_nearest_ub] for an explanation *)
let round_nearest t =
  if t >= round_nearest_lb && t <= round_nearest_ub then
    floor (t +. 0.5)
  else
    t

TEST =
  round_nearest      3.6  =  4.
  && round_nearest (-3.6) = -4.

let round ?(dir=`Nearest) t =
  match dir with
  | `Nearest -> round_nearest      t
  | `Down    -> round_down         t
  | `Up      -> round_up           t
  | `Zero    -> round_towards_zero t

let mod_float = Pervasives.mod_float

module Class = struct
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero
  with sexp, bin_io

  let to_string t = Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.Atom s)
end

let classify t =
  let module C = Class in
  let module P = Pervasives in
  match P.classify_float t with
  | P.FP_normal    -> C.Normal
  | P.FP_subnormal -> C.Subnormal
  | P.FP_zero      -> C.Zero
  | P.FP_infinite  -> C.Infinite
  | P.FP_nan       -> C.Nan
;;

let is_finite t =
  let module C = Class in
  match classify t with
  | C.Normal | C.Subnormal | C.Zero -> true
  | C.Infinite | C.Nan -> false

let to_string_hum ?(delimiter='_') ?(decimals=3) ?(strip_zero=false) f =
  if decimals < 0 then
    invalid_argf "to_string_hum: invalid argument ~decimals=%d" decimals ();
  match classify f with
  | Class.Infinite -> if f >. 0. then "inf" else "-inf"
  | Class.Nan -> "nan"
  | Class.Normal
  | Class.Subnormal
  | Class.Zero ->
    let sprintf_result = sprintf "%.*f" decimals f in
    match String.lsplit2 sprintf_result ~on:'.' with
    | None ->
      assert (decimals = 0);
      Int_conversions.insert_delimiter sprintf_result ~delimiter
    | Some (left, right) ->
      let left = Int_conversions.insert_delimiter left ~delimiter in
      let right =
        if strip_zero
        then String.rstrip right ~drop:(fun c -> c = '0')
        else right
      in
      match right with
      | "" -> left
      | _ -> left ^ "." ^ right
;;

TEST_MODULE = struct
  let test ?delimiter ~decimals f s s_strip_zero =
    let s' = to_string_hum ?delimiter ~decimals ~strip_zero:false f in
    if s' <> s then
      failwiths "to_string_hum ~strip_zero:false"
        (`input f, `decimals decimals, `got s', `expected s)
        (<:sexp_of< ([ `input of float ]
                     * [ `decimals of int ]
                     * [ `got of string ]
                     * [ `expected of string ]) >>);
    let s_strip_zero' = to_string_hum ?delimiter ~decimals ~strip_zero:true f in
    if s_strip_zero' <> s_strip_zero then
      failwiths "to_string_hum ~strip_zero:true"
        (`input f, `decimals decimals, `got s_strip_zero, `expected s_strip_zero')
        (<:sexp_of< ([ `input of float ]
                     * [ `decimals of int ]
                     * [ `got of string ]
                     * [ `expected of string ]) >>);
  ;;

  TEST_UNIT = test ~decimals:3 0.99999 "1.000" "1"
  TEST_UNIT = test ~decimals:3 0.00001 "0.000" "0"
  TEST_UNIT = test ~decimals:3 ~-.12345.1 "-12_345.100" "-12_345.1"
  TEST_UNIT = test ~delimiter:',' ~decimals:3 ~-.12345.1 "-12,345.100" "-12,345.1"
  TEST_UNIT = test ~decimals:0 0.99999 "1" "1"
  TEST_UNIT = test ~decimals:0 0.00001 "0" "0"
  TEST_UNIT = test ~decimals:0 ~-.12345.1 "-12_345" "-12_345"
  TEST_UNIT = test ~decimals:0 (5.0 /. 0.0) "inf" "inf"
  TEST_UNIT = test ~decimals:0 (-5.0 /. 0.0) "-inf" "-inf"
  TEST_UNIT = test ~decimals:0 (0.0 /. 0.0) "nan" "nan"
  TEST_UNIT = test ~decimals:2 (5.0 /. 0.0) "inf" "inf"
  TEST_UNIT = test ~decimals:2 (-5.0 /. 0.0) "-inf" "-inf"
  TEST_UNIT = test ~decimals:2 (0.0 /. 0.0) "nan" "nan"
  TEST_UNIT = test ~decimals:5 (10_000.0 /. 3.0) "3_333.33333" "3_333.33333"
  TEST_UNIT = test ~decimals:2 ~-.0.00001 "-0.00" "-0"

  let rand_test n =
    let go () =
      let f = Random.float 1_000_000.0 -. 500_000.0 in
      let repeatable to_str =
        let s = to_str f in
        if (String.split s ~on:',' |! String.concat |! of_string |! to_str) <> s
        then failwithf "failed on testing %f" f ()
      in
      repeatable (to_string_hum ~decimals:3 ~strip_zero:false);
    in
    try
      for _i = 0 to n - 1 do go () done;
      true
    with e ->
      Printf.eprintf "%s\n%!" (Exn.to_string e);
      false
  ;;

  TEST = rand_test 10_000
  ;;
end
;;

let to_padded_compact_string t =

  (* Round a ratio toward the nearest integer, resolving ties toward the nearest even
     number.  For sane inputs (in particular, when [denominator] is an integer and
     [abs numerator < 2e52]) this should be accurate.  Otherwise, the result might be a
     little bit off, but we don't really use that case. *)
  let iround_ratio_exn ~numerator ~denominator =
    let k = floor (numerator /. denominator) in
    (* if [abs k < 2e53], then both [k] and [k +. 1.] are accurately represented, and in
       particular [k +. 1. > k].  If [denominator] is also an integer, and
       [abs (denominator *. (k +. 1)) < 2e53] (and in some other cases, too), then [lower]
       and [higher] are actually both accurate.  Since (roughly)
       [numerator = denominator *. k] then for [abs numerator < 2e52] we should be
       fine. *)
    let lower  = denominator *. k  in
    let higher = denominator *. (k +. 1.) in
    (* Subtracting numbers within a factor of two from each other is accurate.
       So either the two subtractions below are accurate, or k = 0, or k = -1.
       In case of a tie, round to even. *)
    let diff_right = higher -. numerator in
    let diff_left = numerator -. lower in
    let k = iround_nearest_exn k in
    if diff_right < diff_left then
      k + 1
    else if diff_right > diff_left then
      k
    else
      (* a tie *)
    if k mod 2 = 0 then k else k + 1
  in

  match classify t with
  | Class.Infinite -> if t < 0.0 then "-inf  " else "inf  "
  | Class.Nan -> "nan  "
  | Class.Subnormal | Class.Normal | Class.Zero ->
    let go t =
      let conv_one t =
        assert (0. <= t && t < 999.95);
        let x = format_float "%.1f" t in
        (* Fix the ".0" suffix *)
        if String.is_suffix x ~suffix:".0" then begin
          let n = String.length x in
          x.[n - 1] <- ' ';
          x.[n - 2] <- ' ';
        end;
        x
      in
      let conv mag t denominator =
        assert (denominator  = 100.     && t >= 999.95
             || denominator >= 100_000. && t >= round_nearest (denominator *. 9.999_5));
        assert (t < round_nearest (denominator *. 9_999.5));
        let i, d =
          let k = iround_ratio_exn ~numerator:t ~denominator in
          (* [mod] is okay here because we know i >= 0. *)
          k / 10, k mod 10
        in
        assert (0 <= i && i < 1000);
        assert (0 <= d && d < 10);
        if d = 0 then
          sprintf "%d%c " i mag
        else
          sprintf "%d%c%d" i mag d
      in
      (* While the standard metric prefixes (e.g. capital "M" rather than "m", [1]) are
         nominally more correct, this hinders readability in our case.  E.g., 10G6 and
         1066 look too similar.  That's an extreme example, but in general k,m,g,t,p
         probably stand out better than K,M,G,T,P when interspersed with digits.

         [1] http://en.wikipedia.org/wiki/Metric_prefix *)
      (* The trick here is that:
          - the first boundary (999.95) as a float is slightly over-represented (so it is
            better approximated as "1k" than as "999.9"),
          - the other boundaries are accurately represented, because they are integers.
         That's why the strict equalities below do exactly what we want. *)
      if t < 999.95E0       then conv_one t
      else if t < 999.95E3  then conv 'k' t 100.
      else if t < 999.95E6  then conv 'm' t 100_000.
      else if t < 999.95E9  then conv 'g' t 100_000_000.
      else if t < 999.95E12 then conv 't' t 100_000_000_000.
      else if t < 999.95E15 then conv 'p' t 100_000_000_000_000.
      else sprintf "%.1e" t
    in
    if t >= 0.
    then go t
    else "-" ^ (go ~-.t)

TEST_MODULE = struct
  let test f expect =
    let actual = to_padded_compact_string f  in
    if actual <> expect
    then failwithf "%f: expected '%s', got '%s'" f expect actual ()

  let both f expect =
    assert (f > 0.);
    test f expect;
    test (~-.f) ("-"^expect);
  ;;

  let decr = one_ulp `Down
  let incr = one_ulp `Up

  let boundary f ~closer_to_zero ~at =
    assert (f > 0.);
    (* If [f] looks like an odd multiple of 0.05, it might be slightly under-represented
       as a float, e.g.

       1. -. 0.95 = 0.0500000000000000444

       In such case, sadly, the right way for [to_padded_compact_string], just as for
       [sprintf "%.1f"], is to round it down.  However, the next representable number
       should be rounded up:

       # let x = 0.95 in sprintf "%.0f / %.1f / %.2f / %.3f / %.20f" x x x x x;;
       - : string = "1 / 0.9 / 0.95 / 0.950 / 0.94999999999999995559"

       # let x = incr 0.95 in sprintf "%.0f / %.1f / %.2f / %.3f / %.20f" x x x x x ;;
       - : string = "1 / 1.0 / 0.95 / 0.950 / 0.95000000000000006661"

    *)
    let f =
      if f >= 1000. then
        f
      else
        let x = sprintf "%.20f" f in
        let spot = String.index_exn x '.' in
        (* the following condition is only meant to work for small multiples of 0.05 *)
        if x.[spot + 2] = '4' && x.[spot + 3] = '9' && x.[spot + 4] = '9' then
          (* something like 0.94999999999999995559 *)
          incr f
        else
          f
    in
    both (decr f) closer_to_zero;
    both f at;
  ;;

  TEST_UNIT = test nan                            "nan  ";
  TEST_UNIT = test 0.0                              "0  ";
  TEST_UNIT = both min_positive_subnormal_value     "0  ";
  TEST_UNIT = both infinity                       "inf  ";

  TEST_UNIT = boundary                       0.05 ~closer_to_zero:  "0  " ~at:    "0.1";
  TEST_UNIT = boundary                       0.15 ~closer_to_zero:  "0.1" ~at:    "0.2";
  (* glibc printf resolves ties to even, cf.
     http://www.exploringbinary.com/inconsistent-rounding-of-printed-floating-point-numbers/ *)
  TEST_UNIT = boundary (* tie *)             0.25 ~closer_to_zero:  "0.2" ~at:    "0.2";
  TEST_UNIT = boundary                 (incr 0.25)~closer_to_zero:  "0.2" ~at:    "0.3";
  TEST_UNIT = boundary                       0.35 ~closer_to_zero:  "0.3" ~at:    "0.4";
  TEST_UNIT = boundary                       0.45 ~closer_to_zero:  "0.4" ~at:    "0.5";
  TEST_UNIT = both                           0.50                                 "0.5";
  TEST_UNIT = boundary                       0.55 ~closer_to_zero:  "0.5" ~at:    "0.6";
  TEST_UNIT = boundary                       0.65 ~closer_to_zero:  "0.6" ~at:    "0.7";
  (* this time tie-to-even means round away from 0 *)
  TEST_UNIT = boundary (* tie *)             0.75 ~closer_to_zero:  "0.7" ~at:    "0.8";
  TEST_UNIT = boundary                       0.85 ~closer_to_zero:  "0.8" ~at:    "0.9";
  TEST_UNIT = boundary                       0.95 ~closer_to_zero:  "0.9" ~at:    "1  ";
  TEST_UNIT = boundary                       1.05 ~closer_to_zero:  "1  " ~at:    "1.1";
  TEST_UNIT = boundary                       3.25 ~closer_to_zero:  "3.2" ~at:    "3.2";
  TEST_UNIT = boundary                 (incr 3.25)~closer_to_zero:  "3.2" ~at:    "3.3";
  TEST_UNIT = boundary                       3.75 ~closer_to_zero:  "3.7" ~at:    "3.8";
  TEST_UNIT = boundary                       9.95 ~closer_to_zero:  "9.9" ~at:   "10  ";
  TEST_UNIT = boundary                      10.05 ~closer_to_zero: "10  " ~at:   "10.1";
  TEST_UNIT = boundary                     100.05 ~closer_to_zero:"100  " ~at:  "100.1";
  TEST_UNIT = boundary (* tie *)           999.25 ~closer_to_zero:"999.2" ~at:  "999.2";
  TEST_UNIT = boundary               (incr 999.25)~closer_to_zero:"999.2" ~at:  "999.3";
  TEST_UNIT = boundary                     999.75 ~closer_to_zero:"999.7" ~at:  "999.8";
  TEST_UNIT = boundary                     999.95 ~closer_to_zero:"999.9" ~at:    "1k ";
  TEST_UNIT = both                        1000.                                   "1k ";

  (* some ties which we resolve manually in [iround_ratio_exn] *)
  TEST_UNIT = boundary                    1050.   ~closer_to_zero:  "1k " ~at:    "1k "
  TEST_UNIT = boundary              (incr 1050.)  ~closer_to_zero:  "1k " ~at:    "1k1"
  TEST_UNIT = boundary                    1950.   ~closer_to_zero:  "1k9" ~at:    "2k ";
  TEST_UNIT = boundary                    3250.   ~closer_to_zero:  "3k2" ~at:    "3k2";
  TEST_UNIT = boundary              (incr 3250.)  ~closer_to_zero:  "3k2" ~at:    "3k3";
  TEST_UNIT = boundary                    9950.   ~closer_to_zero:  "9k9" ~at:   "10k ";
  TEST_UNIT = boundary                  33_250.   ~closer_to_zero: "33k2" ~at:   "33k2";
  TEST_UNIT = boundary            (incr 33_250.)  ~closer_to_zero: "33k2" ~at:   "33k3";
  TEST_UNIT = boundary                  33_350.   ~closer_to_zero: "33k3" ~at:   "33k4";
  TEST_UNIT = boundary                  33_750.   ~closer_to_zero: "33k7" ~at:   "33k8";
  TEST_UNIT = boundary                 333_250.   ~closer_to_zero:"333k2" ~at:  "333k2";
  TEST_UNIT = boundary           (incr 333_250.)  ~closer_to_zero:"333k2" ~at:  "333k3";
  TEST_UNIT = boundary                 333_750.   ~closer_to_zero:"333k7" ~at:  "333k8";
  TEST_UNIT = boundary                 999_850.   ~closer_to_zero:"999k8" ~at:  "999k8";
  TEST_UNIT = boundary           (incr 999_850.)  ~closer_to_zero:"999k8" ~at:  "999k9";
  TEST_UNIT = boundary                 999_950.   ~closer_to_zero:"999k9" ~at:    "1m ";
  TEST_UNIT = boundary               1_050_000.   ~closer_to_zero:  "1m " ~at:    "1m ";
  TEST_UNIT = boundary         (incr 1_050_000.)  ~closer_to_zero:  "1m " ~at:    "1m1";

  TEST_UNIT = boundary             999_950_000.   ~closer_to_zero:"999m9" ~at:    "1g ";
  TEST_UNIT = boundary         999_950_000_000.   ~closer_to_zero:"999g9" ~at:    "1t ";
  TEST_UNIT = boundary     999_950_000_000_000.   ~closer_to_zero:"999t9" ~at:    "1p ";
  TEST_UNIT = boundary 999_950_000_000_000_000.   ~closer_to_zero:"999p9" ~at:"1.0e+18";

  (* Test the boundary between the subnormals and the normals. *)
  TEST_UNIT = boundary min_positive_normal_value ~closer_to_zero:"0  " ~at:"0  ";
end

module Replace_polymorphic_compare = struct
  let equal = equal
  let compare (x : t) y = compare x y
  let ascending = compare
  let descending x y = compare y x
  let min = min
  let max = max
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) (x : t) y = x = y
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x <> y
  let between t ~low ~high = low <= t && t <= high
  let _squelch_unused_module_warning_ = ()
end

include Replace_polymorphic_compare

let ( + ) = ( +. )
let ( - ) = ( -. )
let ( * ) = ( *. )
let ( / ) = ( /. )
let ( ~- ) = ( ~-. )

include Comparable.Map_and_set_binable (T)

module Sign = struct
  type t = Neg | Zero | Pos with sexp
end

let sign t =
  if t >. 0. then Sign.Pos
  else if t <. 0. then Sign.Neg
  else Sign.Zero

module Terse = struct
  type t = outer with bin_io
  let t_of_sexp = t_of_sexp

  let to_string x = Core_printf.sprintf "%.8G" x
  let sexp_of_t x = Sexp.Atom (to_string x)
  let of_string x = of_string x
end

let validate_ordinary t =
  Validate.of_error_opt (
    let module C = Class in
    match classify t with
    | C.Normal | C.Subnormal | C.Zero -> None
    | C.Infinite -> Some "value is infinite"
    | C.Nan -> Some "value is NaN")
;;

module V = struct
  module ZZ = Comparable.Validate (T)

  let validate_bound ~min ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_bound t ~min ~max)
  ;;

  let validate_lbound ~min t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_lbound t ~min)
  ;;

  let validate_ubound ~max t =
    Validate.first_failure (validate_ordinary t) (ZZ.validate_ubound t ~max)
  ;;
end

include V

include Comparable.With_zero (struct
  include T
  let zero = zero
  include V
end)

include Pretty_printer.Register(struct
  include T
  let module_name = "Core.Std.Float"
  let to_string = to_string
end)

module O = struct
  let ( +  ) = ( +  )
  let ( -  ) = ( -  )
  let ( *  ) = ( *  )
  let ( /  ) = ( /  )
  let ( ~- ) = ( ~- )
  include (Replace_polymorphic_compare : Polymorphic_compare_intf.Infix with type t := t)
  include Robustly_comparable
  let abs        = abs
  let neg        = neg
  let zero       = zero
  let of_int     = of_int
  let of_float x = x
end

TEST_MODULE = struct
  let check v expect =
    match Validate.result v, expect with
    | Ok (), `Ok | Error _, `Error -> ()
    | r, expect ->
      failwiths "mismatch" (r, expect)
        <:sexp_of< unit Or_error.t * [ `Ok | `Error ] >>
  ;;

  TEST_UNIT = check (validate_lbound ~min:(Incl 0.) nan)          `Error
  TEST_UNIT = check (validate_lbound ~min:(Incl 0.) infinity)     `Error
  TEST_UNIT = check (validate_lbound ~min:(Incl 0.) neg_infinity) `Error
  TEST_UNIT = check (validate_lbound ~min:(Incl 0.) (-1.))        `Error
  TEST_UNIT = check (validate_lbound ~min:(Incl 0.) 0.)           `Ok
  TEST_UNIT = check (validate_lbound ~min:(Incl 0.) 1.)           `Ok

  TEST_UNIT = check (validate_ubound ~max:(Incl 0.) nan)          `Error
  TEST_UNIT = check (validate_ubound ~max:(Incl 0.) infinity)     `Error
  TEST_UNIT = check (validate_ubound ~max:(Incl 0.) neg_infinity) `Error
  TEST_UNIT = check (validate_ubound ~max:(Incl 0.) (-1.))        `Ok
  TEST_UNIT = check (validate_ubound ~max:(Incl 0.) 0.)           `Ok
  TEST_UNIT = check (validate_ubound ~max:(Incl 0.) 1.)           `Error

  (* Some of the following tests used to live in lib_test/core_float_test.ml. *)

  let () = Random.init 137

  let (=) = Pervasives.(=)
  let (>=) = Pervasives.(>=)
  let (+) = Pervasives.(+)
  let (-) = Pervasives.(-)
  let ( * ) = Pervasives.( * )

  (* round:
     ...  <-)[-><-)[-><-)[-><-)[-><-)[-><-)[->   ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so round x -. x should be in (-0.5,0.5]
  *)
  let round_test x =
    let y = round x in
    -0.5 < y -. x && y -. x <= 0.5

  let iround_up_vs_down_test x =
    let expected_difference =
      if Parts.fractional (modf x) = 0. then
        0
      else
        1
    in
      ((iround_up_exn x) - (iround_down_exn x)) = expected_difference

  let test_all_four x ~specialized_iround ~specialized_iround_exn ~dir ~validate =
    let result1 = iround x ~dir in
    let result2 = try Some (iround_exn x ~dir) with _exn -> None in
    let result3 = specialized_iround x in
    let result4 = try Some (specialized_iround_exn x) with _exn -> None in
    let (=) = Pervasives.(=) in
    if result1 = result2 && result2 = result3 && result3 = result4 then
      validate result1
    else
      false

  (* iround ~dir:`Nearest built so this should always be true *)
  let iround_nearest_test x =
    test_all_four x
      ~specialized_iround:iround_nearest
      ~specialized_iround_exn:iround_nearest_exn
      ~dir:`Nearest
      ~validate:(function
        | None -> true
        | Some y ->
          let y = of_int y in
          -0.5 < y -. x && y -. x <= 0.5)

  (* iround_down:
     ... )[<---)[<---)[<---)[<---)[<---)[<---)[  ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so x -. iround_down x should be in [0,1)
  *)
  let iround_down_test x =
    test_all_four x
      ~specialized_iround:iround_down
      ~specialized_iround_exn:iround_down_exn
      ~dir:`Down
      ~validate:(function
        | None -> true
        | Some y ->
          let y = of_int y in
          0. <= x -. y && x -. y < 1.)

  (* iround_up:
     ...  ](--->](--->](--->](--->](--->](--->]( ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so iround_up x -. x should be in [0,1)
  *)
  let iround_up_test x =
    test_all_four x
      ~specialized_iround:iround_up
      ~specialized_iround_exn:iround_up_exn
      ~dir:`Up
      ~validate:(function
        | None -> true
        | Some y ->
          let y = of_int y in
          0. <= y -. x && y -. x < 1.)

  (* iround_towards_zero:
     ...  ](--->](--->](---><--->)[<---)[<---)[  ...
     ... -+-----+-----+-----+-----+-----+-----+- ...
     ... -3    -2    -1     0     1     2     3  ...
     so abs x -. abs (iround_towards_zero x) should be in [0,1)
  *)
  let iround_towards_zero_test x =
    test_all_four x
      ~specialized_iround:iround_towards_zero
      ~specialized_iround_exn:iround_towards_zero_exn
      ~dir:`Zero
      ~validate:(function
        | None -> true
        | Some y ->
          let x = abs x in
          let y = abs (of_int y) in
          0. <= x -. y && x -. y < 1. && (sign x = sign y || y = 0.0))

  (* Easy cases that used to live inline with the code above. *)
  TEST = iround_up (-3.4) = Some (-3)
  TEST = iround_up   0.0  = Some   0
  TEST = iround_up   3.4  = Some   4

  TEST = iround_up_exn (-3.4) = -3
  TEST = iround_up_exn   0.0  =  0
  TEST = iround_up_exn   3.4  =  4

  TEST = iround_down (-3.4) = Some (-4)
  TEST = iround_down   0.0  = Some   0
  TEST = iround_down   3.4  = Some   3

  TEST = iround_down_exn (-3.4) = -4
  TEST = iround_down_exn   0.0  =  0
  TEST = iround_down_exn   3.4  =  3

  TEST = iround_towards_zero (-3.4) = Some (-3)
  TEST = iround_towards_zero   0.0  = Some   0
  TEST = iround_towards_zero   3.4  = Some   3

  TEST = iround_towards_zero_exn (-3.4) = -3
  TEST = iround_towards_zero_exn   0.0  =  0
  TEST = iround_towards_zero_exn   3.4  =  3

  TEST = iround_nearest (-3.6) = Some (-4)
  TEST = iround_nearest (-3.5) = Some (-3)
  TEST = iround_nearest (-3.4) = Some (-3)
  TEST = iround_nearest   0.0  = Some   0
  TEST = iround_nearest   3.4  = Some   3
  TEST = iround_nearest   3.5  = Some   4
  TEST = iround_nearest   3.6  = Some   4

  TEST = iround_nearest_exn (-3.6) = -4
  TEST = iround_nearest_exn (-3.5) = -3
  TEST = iround_nearest_exn (-3.4) = -3
  TEST = iround_nearest_exn   0.0  =  0
  TEST = iround_nearest_exn   3.4  =  3
  TEST = iround_nearest_exn   3.5  =  4
  TEST = iround_nearest_exn   3.6  =  4

  let special_values_test () =
    round (-.1.50001) = -.2. &&
    round (-.1.5) = -.1. &&
    round (-.0.50001) = -.1. &&
    round (-.0.5) = 0. &&
    round 0.49999 = 0. &&
    round 0.5 = 1. &&
    round 1.49999 = 1. &&
    round 1.5 = 2. &&
    iround_exn ~dir:`Up (-.2.) = -2 &&
    iround_exn ~dir:`Up (-.1.9999) = -1 &&
    iround_exn ~dir:`Up (-.1.) = -1 &&
    iround_exn ~dir:`Up (-.0.9999) = 0 &&
    iround_exn ~dir:`Up 0. = 0 &&
    iround_exn ~dir:`Up 0.00001 = 1 &&
    iround_exn ~dir:`Up 1. = 1 &&
    iround_exn ~dir:`Up 1.00001 = 2 &&
    iround_up_exn (-.2.) = -2 &&
    iround_up_exn (-.1.9999) = -1 &&
    iround_up_exn (-.1.) = -1 &&
    iround_up_exn (-.0.9999) = 0 &&
    iround_up_exn 0. = 0 &&
    iround_up_exn 0.00001 = 1 &&
    iround_up_exn 1. = 1 &&
    iround_up_exn 1.00001 = 2 &&
    iround_exn ~dir:`Down (-.1.00001) = -2 &&
    iround_exn ~dir:`Down (-.1.) = -1 &&
    iround_exn ~dir:`Down (-.0.00001) = -1 &&
    iround_exn ~dir:`Down 0. = 0 &&
    iround_exn ~dir:`Down 0.99999 = 0 &&
    iround_exn ~dir:`Down 1. = 1 &&
    iround_exn ~dir:`Down 1.99999 = 1 &&
    iround_exn ~dir:`Down 2. = 2 &&
    iround_down_exn (-.1.00001) = -2 &&
    iround_down_exn (-.1.) = -1 &&
    iround_down_exn (-.0.00001) = -1 &&
    iround_down_exn 0. = 0 &&
    iround_down_exn 0.99999 = 0 &&
    iround_down_exn 1. = 1 &&
    iround_down_exn 1.99999 = 1 &&
    iround_down_exn 2. = 2 &&
    iround_exn ~dir:`Zero (-.2.) = -2 &&
    iround_exn ~dir:`Zero (-.1.99999) = -1 &&
    iround_exn ~dir:`Zero (-.1.) = -1 &&
    iround_exn ~dir:`Zero (-.0.99999) = 0 &&
    iround_exn ~dir:`Zero 0.99999 = 0 &&
    iround_exn ~dir:`Zero 1. = 1 &&
    iround_exn ~dir:`Zero 1.99999 = 1 &&
    iround_exn ~dir:`Zero 2. = 2

  let is_64_bit_platform = of_int max_int >= 2. ** 60.

  (* Tests for values close to [iround_lbound] and [iround_ubound]. *)
  let extremities_test ~round =
    if is_64_bit_platform then
      (* 64 bits *)
      round (2.0 ** 62. -. 512.) = Some (max_int - 511)
        && round (2.0 ** 62. -. 1024.) = Some (max_int - 1023)
        && round (-. (2.0 ** 62.)) = Some min_int
        && round (-. (2.0 ** 62. -. 512.)) = Some (min_int + 512)
        && round (2.0 ** 62.) = None
        && round (-. (2.0 ** 62. +. 1024.)) = None
    else
      (* 32 bits *)
      round (2.0 ** 30. -. 1.) = Some max_int
        && round (2.0 ** 30. -. 2.) = Some (max_int - 1)
        && round (-. (2.0 ** 30.)) = Some min_int
        && round (-. (2.0 ** 30. -. 1.)) = Some (min_int + 1)
        && round (2.0 ** 30.) = None
        && round (-. (2.0 ** 30. +. 1.)) = None

  TEST = extremities_test ~round:iround_down
  TEST = extremities_test ~round:iround_up
  TEST = extremities_test ~round:iround_nearest
  TEST = extremities_test ~round:iround_towards_zero

  (* test values beyond the integers range *)
  let large_value_test x =
    true

    && iround_down x = None
    && iround ~dir:`Down x = None
    && iround_up x = None
    && iround ~dir:`Up x = None
    && iround_towards_zero x = None
    && iround ~dir:`Zero x = None
    && iround_nearest x = None
    && iround ~dir:`Nearest x = None

    && (try ignore (iround_down_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Down x); false with _ -> true)
    && (try ignore (iround_up_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Up x); false with _ -> true)
    && (try ignore (iround_towards_zero_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Zero x); false with _ -> true)
    && (try ignore (iround_nearest_exn x); false with _ -> true)
    && (try ignore (iround_exn ~dir:`Nearest x); false with _ -> true)

    && round_down x = x
    && round ~dir:`Down x = x
    && round_up x = x
    && round ~dir:`Up x = x
    && round_towards_zero x = x
    && round ~dir:`Zero x = x
    && round_nearest x = x
    && round ~dir:`Nearest x = x

  let large_numbers =
    Core_list.concat (
      Core_list.init (1024 - 64) ~f:(fun x ->
        let x = float (x + 64) in
        let y =
          [2. ** x;
           2. ** x -. 2. ** (x -. 53.); (* one ulp down *)
           2. ** x +. 2. ** (x -. 52.)] (* one ulp up *)
        in
        y @ (List.map y ~f:neg)))
      @
      [infinity;
       neg_infinity]

  TEST = Core_list.for_all large_numbers ~f:large_value_test

  let numbers_near_powers_of_two =
    Core_list.concat (
      Core_list.init (if is_64_bit_platform then 62 else 30) ~f:(fun i ->
        let pow2 = 2. ** float i in
        let x =
          [ pow2;
            one_ulp `Down (pow2 +. 0.5);
            pow2 +. 0.5;
            one_ulp `Down (pow2 +. 1.0);
            pow2 +. 1.0;
            one_ulp `Down (pow2 +. 1.5);
            pow2 +. 1.5;
            one_ulp `Down (pow2 +. 2.0);
            pow2 +. 2.0;
            one_ulp `Down (pow2 *. 2.0 -. 1.0);
          ]
        in
        x @ (List.map x ~f:neg)
      ))

  TEST = Core_list.for_all numbers_near_powers_of_two ~f:iround_up_vs_down_test
  TEST = Core_list.for_all numbers_near_powers_of_two ~f:iround_nearest_test
  TEST = Core_list.for_all numbers_near_powers_of_two ~f:iround_down_test
  TEST = Core_list.for_all numbers_near_powers_of_two ~f:iround_up_test
  TEST = Core_list.for_all numbers_near_powers_of_two ~f:iround_towards_zero_test
  TEST = Core_list.for_all numbers_near_powers_of_two ~f:round_test

  (* code for generating random floats on which to test functions *)
  let rec absirand () =
    let rec aux acc cnt =
      if cnt = 0 then
        acc
      else
        let bit = if Random.bool () then 1 else 0 in
        aux (2 * acc + bit) (cnt - 1)
    in
    let result = aux 0 (if is_64_bit_platform then 62 else 30) in
    if result >= max_int - 255 then
      (* On a 64-bit box, [float x > max_int] when [x >= max_int - 255], so
         [iround (float x)] would be out of bounds.  So we try again.  This branch of code
         runs with probability 6e-17 :-)  As such, we have some fixed tests in
         [extremities_test] above, to ensure that we do always check some examples in
         that range. *)
      absirand ()
    else
      result

  (* -max_int <= frand () <= max_int *)
  let frand () =
    let x = (float (absirand ())) +. Random.float 1.0 in
    if Random.bool () then
      -1.0 *. x
    else
      x

  let randoms = Core_list.init ~f:(fun _ -> frand ()) 10_000

  TEST = Core_list.for_all randoms ~f:iround_up_vs_down_test
  TEST = Core_list.for_all randoms ~f:iround_nearest_test
  TEST = Core_list.for_all randoms ~f:iround_down_test
  TEST = Core_list.for_all randoms ~f:iround_up_test
  TEST = Core_list.for_all randoms ~f:iround_towards_zero_test
  TEST = Core_list.for_all randoms ~f:round_test
  TEST = special_values_test ()
  TEST = iround_nearest_test (of_int max_int)
  TEST = iround_nearest_test (of_int min_int)
end


BENCH_MODULE "Simple" = struct
  (* The [of_string] is so that won't get inlined. The values of [x] and [y] have no
     special significance. *)
  let x = of_string "1.0000000001000000111"
  let y = of_string "2.0000000001000000111"

  BENCH "add" = x +. y
  BENCH "mul" = x *. y
  BENCH "div" = x /. y
  BENCH "exp" = x ** y
  BENCH "log" = log x
  BENCH "sqrt"  = sqrt x
end

BENCH_MODULE "Rounding" = struct
  let x = of_string "1.0000000001000000111"

  BENCH "iround_down_exn"         = iround_down_exn         x
  BENCH "round_nearest_exn"       = iround_nearest_exn      x
  BENCH "iround_up_exn"           = iround_up_exn           x
  BENCH "iround_towards_zero_exn" = iround_towards_zero_exn x
  BENCH "Pervasives.int_of_float" = Pervasives.int_of_float x
end

(* These tests show the degenerate cases for the OCaml [**] operator.  The slowness of
   this primitive can be traced back to the implementation of [pow] in [glibc].  Also see
   our Perf notes for more about this issue. *)
BENCH_MODULE "pow (**)" = struct
  BENCH "very slow" = 1.0000000000000020 ** 0.5000000000000000
  BENCH "slow"      = 1.0000000000000020 ** 0.5000000000100000
  BENCH "slow"      = 1.0000000000000020 ** 0.4999999999000000
  BENCH "fast"      = 1.0000000000000020 ** 0.4999900000000000
end
