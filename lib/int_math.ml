open Interfaces
open Common

let negative_exponent () =
  Core_printf.invalid_argf "exponent can not be negative" ()

let overflow () =
  Core_printf.invalid_argf "integer overflow in pow" ()

(* To implement [int64_pow], we use C code rather than OCaml to eliminate allocation. *)
external int_math_int_pow   : int   -> int   -> int   = "int_math_int_pow_stub" "noalloc"
external int_math_int64_pow : int64 -> int64 -> int64 = "int_math_int64_pow_stub"

let int_pow base exponent =
  if exponent < 0 then negative_exponent ();

  if abs(base) > 1 &&
     (exponent > 63 ||
      abs(base) > Pow_overflow_bounds.int_positive_overflow_bounds.(exponent))
  then overflow ();

  int_math_int_pow base exponent
;;

(* we don't do [abs] in int64 case to avoid allocation *)
let int64_pow base exponent =
  if exponent < 0L then negative_exponent ();

  if (base > 1L || base < (-1L)) &&
     (exponent > 63L ||
      (base >= 0L &&
       base > Pow_overflow_bounds.int64_positive_overflow_bounds.(Int64.to_int exponent))
      ||
      (base < 0L &&
       base < Pow_overflow_bounds.int64_negative_overflow_bounds.(Int64.to_int exponent)))
  then overflow ();

  int_math_int64_pow base exponent
;;

BENCH_MODULE "int_math_pow" = struct
  BENCH "2 ^ 30"   = int_pow 2 30
  BENCH "2L ^ 30L" = int64_pow 2L 30L
  BENCH "2L ^ 60L" = int64_pow 2L 60L
end

module type T = sig
  type t
  include Floatable with type t := t
  include Stringable with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t
  include Polymorphic_compare_intf.Infix with type t := t

  val abs    : t -> t
  val neg    : t -> t
  val zero   : t
  val of_int_exn : int -> t
  val rem : t -> t -> t
end

module Make (X : T) = struct
  open X

  let ( % ) x y =
    if y <= zero then
      invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string x) (to_string y) ();
    let rval = X.rem x y in
    if rval < zero
    then rval + y
    else rval
  ;;

  let one = of_int_exn 1
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

  (** float division of integers *)
  let (//) x y = to_float x /. to_float y
  ;;

  let round_down i ~to_multiple_of:modulus = i - (i % modulus)
  ;;

  let round_up i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder = zero
    then i
    else i + modulus - remainder
  ;;

  let round_towards_zero i ~to_multiple_of =
    if i = zero then zero else
    if i > zero
    then round_down i ~to_multiple_of
    else round_up   i ~to_multiple_of
  ;;

  let round_nearest i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder * of_int_exn 2 < modulus
    then i - remainder
    else i - remainder + modulus
  ;;

  let round ?(dir=`Nearest) i ~to_multiple_of =
    match dir with
    | `Nearest -> round_nearest      i ~to_multiple_of
    | `Down    -> round_down         i ~to_multiple_of
    | `Up      -> round_up           i ~to_multiple_of
    | `Zero    -> round_towards_zero i ~to_multiple_of
  ;;

  TEST_MODULE "integer-rounding" = struct

    let check dir ~range:(lower, upper) ~modulus expected =
      let modulus = of_int_exn modulus in
      let expected = of_int_exn expected in
      for i = lower to upper do
        let observed = round ~dir ~to_multiple_of:modulus (of_int_exn i) in
        if observed <> expected then failwithf "invalid result for i = %d" i ()
      done
    ;;

    TEST_UNIT = check ~modulus:10 `Down    ~range:( 10,  19)   10
    TEST_UNIT = check ~modulus:10 `Down    ~range:(  0,   9)    0
    TEST_UNIT = check ~modulus:10 `Down    ~range:(-10,  -1) (-10)
    TEST_UNIT = check ~modulus:10 `Down    ~range:(-20, -11) (-20)

    TEST_UNIT = check ~modulus:10 `Up      ~range:( 11,  20)   20
    TEST_UNIT = check ~modulus:10 `Up      ~range:(  1,  10)   10
    TEST_UNIT = check ~modulus:10 `Up      ~range:( -9,   0)    0
    TEST_UNIT = check ~modulus:10 `Up      ~range:(-19, -10) (-10)

    TEST_UNIT = check ~modulus:10 `Zero    ~range:( 10,  19)   10
    TEST_UNIT = check ~modulus:10 `Zero    ~range:( -9,   9)    0
    TEST_UNIT = check ~modulus:10 `Zero    ~range:(-19, -10) (-10)

    TEST_UNIT = check ~modulus:10 `Nearest ~range:( 15,  24)   20
    TEST_UNIT = check ~modulus:10 `Nearest ~range:(  5,  14)   10
    TEST_UNIT = check ~modulus:10 `Nearest ~range:( -5,   4)    0
    TEST_UNIT = check ~modulus:10 `Nearest ~range:(-15,  -6) (-10)
    TEST_UNIT = check ~modulus:10 `Nearest ~range:(-25, -16) (-20)

    TEST_UNIT = check ~modulus:5 `Nearest ~range:(  8, 12)   10
    TEST_UNIT = check ~modulus:5 `Nearest ~range:(  3,  7)    5
    TEST_UNIT = check ~modulus:5 `Nearest ~range:( -2,  2)    0
    TEST_UNIT = check ~modulus:5 `Nearest ~range:( -7, -3)  (-5)
    TEST_UNIT = check ~modulus:5 `Nearest ~range:(-12, -8) (-10)
  end

  TEST_MODULE "remainder-and-modulus-random" = struct
    let check x y =
      let check_raises f =
        try begin
          ignore (f ());
          failwithf "failed for x = %s, y = %s" (to_string x) (to_string y) ()
        end with _ -> ()
      in
      let check cond =
        if not cond
        then failwithf "failed for x = %s, y = %s" (to_string x) (to_string y) ()
      in
      if x < zero
      then check (rem x y < zero)
      else check (rem x y >= zero);
      check (abs (rem x y) <= abs y - one);
      if y < zero then begin
        check_raises (fun () -> x % y);
        check_raises (fun () -> x /% y)
      end
      else begin
        check (x = (x /% y) * y + (x % y));
        check (x = (x /  y) * y + (rem x y));
        check (x % y >= zero);
        check (x % y <= y - one);
        if x > zero && y > zero
        then begin
          check (x /% y = x / y);
          check (x % y = rem x y)
        end;
      end

    ;;

    TEST_UNIT =
      Random.self_init ();
      for _i = 0 to 1000 do
        let max_value = 1000000000 in
        let x = of_int_exn (Random.int max_value) in
        let y = of_int_exn (Random.int max_value) in
        check x    y;
        check (-x) y;
        check x    (-y);
        check (-x) (-y);
      done
  end
end

TEST_MODULE "pow" = struct
  TEST = int_pow 0  0 = 1
  TEST = int_pow 0  1 = 0
  TEST = int_pow 10 1 = 10
  TEST = int_pow 10 2 = 100
  TEST = int_pow 10 3 = 1_000
  TEST = int_pow 10 4 = 10_000
  TEST = int_pow 10 5 = 100_000
  TEST = int_pow 2 10 = 1024

  TEST = int_pow 0 1_000_000 = 0
  TEST = int_pow 1 1_000_000 = 1
  TEST = int_pow (-1) 1_000_000 = 1
  TEST = int_pow (-1) 1_000_001 = -1

  TEST = int64_pow 0L 0L = 1L
  TEST = int64_pow 0L 1_000_000L = 0L
  TEST = int64_pow 1L 1_000_000L = 1L
  TEST = int64_pow (-1L) 1_000_000L = 1L
  TEST = int64_pow (-1L) 1_000_001L = -1L

  TEST = int64_pow 10L 1L  = 10L
  TEST = int64_pow 10L 2L  = 100L
  TEST = int64_pow 10L 3L  = 1_000L
  TEST = int64_pow 10L 4L  = 10_000L
  TEST = int64_pow 10L 5L  = 100_000L
  TEST = int64_pow 2L  10L = 1_024L
  TEST = int64_pow 5L  27L = 7450580596923828125L

  let exception_thrown pow b e = try let _ = pow b e in false with _ -> true;;

  TEST = exception_thrown int_pow 10 60
  TEST = exception_thrown int64_pow 10L 60L
  TEST = exception_thrown int_pow 10 (-1)
  TEST = exception_thrown int64_pow 10L (-1L)

  TEST = exception_thrown int64_pow 2L 63L
  TEST = not (exception_thrown int64_pow 2L 62L)

  TEST = exception_thrown int64_pow (-2L) 63L
  TEST = not (exception_thrown int64_pow (-2L) 62L)
end

TEST_MODULE "overflow_bounds" = struct
  TEST = Pow_overflow_bounds.overflow_bound_max_int_value = Pervasives.max_int
  TEST = Pow_overflow_bounds.overflow_bound_max_int64_value = Int64.max_int

  (* These tests are disabled because they create a dependency to bignum.

  module Big_int = struct
    include Big_int
    let (>)  = gt_big_int
    let (=)  = eq_big_int
    let (^)  = power_big_int_positive_int
    let (+)  = add_big_int
    let one  = unit_big_int
    let to_string = string_of_big_int
  end

  let test_overflow_table tbl conv max_val =
    assert (Array.length tbl = 64);
    let max_val = conv max_val in
    StdLabels.Array.iteri tbl ~f:(fun i max_base ->
      let max_base = conv max_base in
      let overflows b = Big_int.((b ^ i) > max_val) in
      let is_ok =
        if i = 0 then Big_int.(max_base = max_val)
        else
          not (overflows max_base) && overflows Big_int.(max_base + one)
      in
      if not is_ok then
        Core_printf.failwithf
          "overflow table check failed for %s (index %d)"
          (Big_int.to_string max_base) i ())
  ;;

  TEST_UNIT = test_overflow_table Pow_overflow_bounds.int_positive_overflow_bounds
                Big_int.big_int_of_int Pervasives.max_int

  TEST_UNIT = test_overflow_table Pow_overflow_bounds.int64_positive_overflow_bounds
                Big_int.big_int_of_int64 Int64.max_int
  *)
end
