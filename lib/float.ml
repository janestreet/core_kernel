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
  type t = float with sexp, bin_io
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
type outer = t with sexp, bin_io (* alias for use by sub-modules *)

let to_float x = x
let of_float x = x

let of_string s =
  try Pervasives.float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

let to_string = Pervasives.string_of_float

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float
let min_positive_value = Pervasives.min_float
let zero = 0.

let frexp = Pervasives.frexp
let ldexp = Pervasives.ldexp

let is_nan x = (x : t) <> x
include
  (Float_robust_compare.Make(struct let epsilon = 1E-7 end) : Float_robust_compare.S)

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

let (+) t t' = t +. t'
let (-) t t' = t -. t'
let ( * ) t t' = t *. t'
let (/) t t' = t /. t'

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

  (* Not for real use.  Slow and does not handle 0. *)
  let one_ulp_less x =
    let rec aux x exp =
      let y = x -. (2. ** exp) in
      if x = y then
        aux x (exp +. 1.)
      else
        y
    in
    let start_exp = floor (log x /. log 2.) -. 55. in
    assert (x -. (2. ** start_exp) = x);
    aux x start_exp

  let numbers_near_powers_of_two =
    Core_list.concat (
      Core_list.init (if is_64_bit_platform then 62 else 30) ~f:(fun i ->
        let pow2 = 2. ** float i in
        let x =
          [ pow2;
            one_ulp_less (pow2 +. 0.5);
            pow2 +. 0.5;
            one_ulp_less (pow2 +. 1.0);
            pow2 +. 1.0;
            one_ulp_less (pow2 +. 1.5);
            pow2 +. 1.5;
            one_ulp_less (pow2 +. 2.0);
            pow2 +. 2.0;
            one_ulp_less (pow2 *. 2.0 -. 1.0);
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
