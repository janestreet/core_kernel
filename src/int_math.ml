open Common
open Interfaces

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

let int63_pow_on_int64 base exponent =
  if exponent < 0L then negative_exponent ();

  if Int64.abs(base) > 1L &&
     (exponent > 63L ||
      Int64.abs(base) > Pow_overflow_bounds.int63_on_int64_positive_overflow_bounds.(Int64.to_int exponent))
  then overflow ();

  int_math_int64_pow base exponent
;;

let%test_unit _ =
  let x = match Word_size.word_size with W32 -> 9 | W64 -> 10 in
  for i = 0 to x do
    for j = 0 to x do
      assert (int_pow i j
              = Pervasives.(int_of_float ((float_of_int i) ** (float_of_int j))))
    done
  done

let%bench_module "int_math_pow" = (module struct
  let a = Array.init 10000 (fun _ -> Random.int 5)
  let%bench "random[ 5] x 10000" = Array.iter (fun x -> let _ = int_pow 2 x in ()) a
  let a = Array.init 10000 (fun _ -> Random.int 10)
  let%bench "random[10] x 10000" = Array.iter (fun x -> let _ = int_pow 2 x in ()) a
  let a = Array.init 10000 (fun _ -> Random.int 30)
  let%bench "random[30] x 10000" = Array.iter (fun x -> let _ = int_pow 2 x in ()) a
  let a = Array.init 10000 (fun _ -> Random.int 60)
  let%bench "random[60] x 10000" = Array.iter (fun x -> let _ = int_pow 2 x in ()) a
  let%bench "2 ^ 30"   = int_pow 2 30
  let%bench "2L ^ 30L" = int64_pow 2L 30L
  let%bench "2L ^ 60L" = int64_pow 2L 60L
end)

(* C stub for int popcount to use the POPCNT instruction where possible *)
external int_popcount : int -> int = "int_math_int_popcount" "noalloc"

(* To maintain javascript compatibility and enable unboxing, we implement popcount in
   OCaml rather than use C stubs. Implementation adapted from:
   https://en.wikipedia.org/wiki/Hamming_weight#Efficient_implementation *)
let int64_popcount =
  let open Int64 in
  let ( + ) = add in
  let ( - ) = sub in
  let ( * ) = mul in
  let ( lsr ) = shift_right_logical in
  let ( land ) = logand in
  let m1  = 0x5555555555555555L in (* 0b01010101... *)
  let m2  = 0x3333333333333333L in (* 0b00110011... *)
  let m4  = 0x0f0f0f0f0f0f0f0fL in (* 0b00001111... *)
  let h01 = 0x0101010101010101L in (* 1 bit set per byte *)
  fun x ->
    (* gather the bit count for every pair of bits *)
    let x = x - ((x lsr 1) land m1) in
    (* gather the bit count for every 4 bits *)
    let x = (x land m2) + ((x lsr 2) land m2) in
    (* gather the bit count for every byte *)
    let x = (x + (x lsr 4)) land m4 in
    (* sum the bit counts in the top byte and shift it down *)
    to_int ((x * h01) lsr 56)

let int32_popcount =
  (* On 64-bit systems, this is faster than implementing using [int32] arithmetic. *)
  let mask = 0xffff_ffffL in
  fun x -> int64_popcount (Int64.logand (Int64.of_int32 x) mask)

let nativeint_popcount =
  match Nativeint.size with
  | 32 -> (fun x -> int32_popcount (Nativeint.to_int32 x))
  | 64 -> (fun x -> int64_popcount (Int64.of_nativeint x))
  | _  -> assert false

(* Using [%bench_fun] to bind the input outside the benchmarked code actually has less
   overhead then using [%bench] naively. *)
let%bench_fun "popcount_bench_overhead" = let n = 0  in fun () -> Fn.id              n
let%bench_fun "int_popcount"            = let n = 0  in fun () -> int_popcount       n
let%bench_fun "int32_popcount"          = let n = 0l in fun () -> int32_popcount     n
let%bench_fun "int64_popcount"          = let n = 0L in fun () -> int64_popcount     n
let%bench_fun "nativeint_popcount"      = let n = 0n in fun () -> nativeint_popcount n

let%test_module "popcount" =
  (module struct
    open Sexplib.Std

    let test_int       n bits = [%test_result: int] (int_popcount       n) ~expect:bits
    let test_int32     n bits = [%test_result: int] (int32_popcount     n) ~expect:bits
    let test_int64     n bits = [%test_result: int] (int64_popcount     n) ~expect:bits
    let test_nativeint n bits = [%test_result: int] (nativeint_popcount n) ~expect:bits

    (* test simple constants and boundary conditions *)

    let%test_unit _ = test_int       0                  0
    let%test_unit _ = test_int       1                  1
    let%test_unit _ = test_int       (-1)               Int_conversions.num_bits_int
    let%test_unit _ = test_int       Pervasives.max_int (Int_conversions.num_bits_int - 1)
    let%test_unit _ = test_int       Pervasives.min_int 1

    let%test_unit _ = test_int32     0l                 0
    let%test_unit _ = test_int32     1l                 1
    let%test_unit _ = test_int32     (-1l)              32
    let%test_unit _ = test_int32     Int32.max_int      31
    let%test_unit _ = test_int32     Int32.min_int      1

    let%test_unit _ = test_int64     0L                 0
    let%test_unit _ = test_int64     1L                 1
    let%test_unit _ = test_int64     (-1L)              64
    let%test_unit _ = test_int64     Int64.max_int      63
    let%test_unit _ = test_int64     Int64.min_int      1

    let%test_unit _ = test_nativeint 0n                 0
    let%test_unit _ = test_nativeint 1n                 1
    let%test_unit _ = test_nativeint (-1n)              Nativeint.size
    let%test_unit _ = test_nativeint Nativeint.max_int  (Nativeint.size - 1)
    let%test_unit _ = test_nativeint Nativeint.min_int  1

    (* test that we can account for each bit individually *)

    let%test_unit _ =
      for i = 0 to Int_conversions.num_bits_int - 1 do
        let n = 1 lsl i in
        test_int n 1;
        test_int (lnot n) (Int_conversions.num_bits_int - 1)
      done

    let%test_unit _ =
      for i = 0 to 31 do
        let n = Int32.shift_left 1l i in
        test_int32 n 1;
        test_int32 (Int32.lognot n) 31
      done

    let%test_unit _ =
      for i = 0 to 63 do
        let n = Int64.shift_left 1L i in
        test_int64 n 1;
        test_int64 (Int64.lognot n) 63
      done

    let%test_unit _ =
      for i = 0 to Nativeint.size - 1 do
        let n = Nativeint.shift_left 1n i in
        test_nativeint n 1;
        test_nativeint (Nativeint.lognot n) (Nativeint.size - 1)
      done

    (* Make sure unboxing works as expected and so forth, which it wouldn't if we used C
       stubs with boxed values for [int64], [int32], and [nativeint].  Use random inputs
       to make sure the compiler can't inline and precompute results. *)

    let does_not_allocate f =
      let test () =
        let len = 100 in
        let inputs = ArrayLabels.init len ~f:(fun _ -> Random.bits ()) in
        let minor_before = Core_gc.minor_words () in
        for i = 0 to len-1 do
          ignore (f inputs.(i) : int)
        done;
        let minor_after = Core_gc.minor_words () in
        [%test_result: int]
          (minor_after - minor_before)
          ~expect:0
          ~message:"number of words allocated"
      in
      (* On 32-bit systems, int64 cannot be unboxed, so this test only makes sense on
         64-bit systems. *)
      match Nativeint.size with
      | 32 -> ()
      | 64 -> test ()
      | _  -> assert false

    let%test_unit _ = does_not_allocate (fun x -> int_popcount x)
    let%test_unit _ = does_not_allocate (fun x -> int32_popcount     (Int32.of_int     x))
    let%test_unit _ = does_not_allocate (fun x -> int64_popcount     (Int64.of_int     x))
    let%test_unit _ = does_not_allocate (fun x -> nativeint_popcount (Nativeint.of_int x))

  end)

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

  let%test_module "integer-rounding" = (module struct

    let check dir ~range:(lower, upper) ~modulus expected =
      let modulus = of_int_exn modulus in
      let expected = of_int_exn expected in
      for i = lower to upper do
        let observed = round ~dir ~to_multiple_of:modulus (of_int_exn i) in
        if observed <> expected then failwithf "invalid result for i = %d" i ()
      done
    ;;

    let%test_unit _ = check ~modulus:10 `Down    ~range:( 10,  19)   10
    let%test_unit _ = check ~modulus:10 `Down    ~range:(  0,   9)    0
    let%test_unit _ = check ~modulus:10 `Down    ~range:(-10,  -1) (-10)
    let%test_unit _ = check ~modulus:10 `Down    ~range:(-20, -11) (-20)

    let%test_unit _ = check ~modulus:10 `Up      ~range:( 11,  20)   20
    let%test_unit _ = check ~modulus:10 `Up      ~range:(  1,  10)   10
    let%test_unit _ = check ~modulus:10 `Up      ~range:( -9,   0)    0
    let%test_unit _ = check ~modulus:10 `Up      ~range:(-19, -10) (-10)

    let%test_unit _ = check ~modulus:10 `Zero    ~range:( 10,  19)   10
    let%test_unit _ = check ~modulus:10 `Zero    ~range:( -9,   9)    0
    let%test_unit _ = check ~modulus:10 `Zero    ~range:(-19, -10) (-10)

    let%test_unit _ = check ~modulus:10 `Nearest ~range:( 15,  24)   20
    let%test_unit _ = check ~modulus:10 `Nearest ~range:(  5,  14)   10
    let%test_unit _ = check ~modulus:10 `Nearest ~range:( -5,   4)    0
    let%test_unit _ = check ~modulus:10 `Nearest ~range:(-15,  -6) (-10)
    let%test_unit _ = check ~modulus:10 `Nearest ~range:(-25, -16) (-20)

    let%test_unit _ = check ~modulus:5 `Nearest ~range:(  8, 12)   10
    let%test_unit _ = check ~modulus:5 `Nearest ~range:(  3,  7)    5
    let%test_unit _ = check ~modulus:5 `Nearest ~range:( -2,  2)    0
    let%test_unit _ = check ~modulus:5 `Nearest ~range:( -7, -3)  (-5)
    let%test_unit _ = check ~modulus:5 `Nearest ~range:(-12, -8) (-10)
  end)

  let%test_module "remainder-and-modulus" = (module struct

    let check_integers x y =
      let check_raises f desc =
        match f () with
        | exception _ -> ()
        | z -> failwithf "%s: failed for x = %s, y = %s; produced %s rather than raising"
                 desc (to_string x) (to_string y) (to_string z) ()
      in
      let check_true cond desc =
        if not cond
        then failwithf "%s: failed for x = %s, y = %s" desc (to_string x) (to_string y) ()
      in
      if y = zero
      then
        begin
          check_raises (fun () -> x / y) "division by zero";
          check_raises (fun () -> rem x y) "rem _ zero";
          check_raises (fun () -> x % y) "_ % zero";
          check_raises (fun () -> x /% y) "_ /% zero";
        end
      else
        begin
          if x < zero
          then check_true (rem x y <= zero) "non-positive remainder"
          else check_true (rem x y >= zero) "non-negative remainder";
          check_true (abs (rem x y) <= abs y - one) "range of remainder";
          if y < zero then begin
            check_raises (fun () -> x % y) "_ % negative";
            check_raises (fun () -> x /% y) "_ /% negative"
          end
          else begin
            check_true (x = (x /% y) * y + (x % y)) "(/%) and (%) identity";
            check_true (x = (x /  y) * y + (rem x y)) "(/) and rem identity";
            check_true (x % y >= zero) "non-negative (%)";
            check_true (x % y <= y - one) "range of (%)";
            if x > zero && y > zero
            then begin
              check_true (x /% y = x / y) "(/%) and (/) identity";
              check_true (x % y = rem x y) "(%) and rem identity"
            end;
          end
        end
    ;;

    let check_natural_numbers x y =
      Core_list.iter [ x ; -x ; x+one ; -(x + one) ] ~f:(fun x ->
        Core_list.iter [ y ; -y ; y+one ; -(y + one) ] ~f:(fun y ->
          check_integers x y))

    let%test_unit "deterministic" =
      let big1 = of_int_exn 118_310_344 in
      let big2 = of_int_exn 828_172_408 in
      (* Important to test the case where one value is a multiple of the other.  Note that
         the [x + one] and [y + one] cases in [check_natural_numbers] ensure that we also
         test non-multiple cases. *)
      assert (big2 = big1 * of_int_exn 7);
      let values = [ zero ; one ; big1 ; big2 ] in
      Core_list.iter values ~f:(fun x ->
        Core_list.iter values ~f:(fun y ->
          check_natural_numbers x y))

    let%test_unit "random" =
      let rand = Core_random.State.make [| 8; 67; -5_309 |] in
      for _ = 0 to 1_000 do
        let max_value = 1_000_000_000 in
        let x = of_int_exn (Core_random.State.int rand max_value) in
        let y = of_int_exn (Core_random.State.int rand max_value) in
        check_natural_numbers x y
      done
  end)

end

let%test_module "pow" = (module struct
  let%test _ = int_pow 0  0 = 1
  let%test _ = int_pow 0  1 = 0
  let%test _ = int_pow 10 1 = 10
  let%test _ = int_pow 10 2 = 100
  let%test _ = int_pow 10 3 = 1_000
  let%test _ = int_pow 10 4 = 10_000
  let%test _ = int_pow 10 5 = 100_000
  let%test _ = int_pow 2 10 = 1024

  let%test _ = int_pow 0 1_000_000 = 0
  let%test _ = int_pow 1 1_000_000 = 1
  let%test _ = int_pow (-1) 1_000_000 = 1
  let%test _ = int_pow (-1) 1_000_001 = -1

  let%test _ = int64_pow 0L 0L = 1L
  let%test _ = int64_pow 0L 1_000_000L = 0L
  let%test _ = int64_pow 1L 1_000_000L = 1L
  let%test _ = int64_pow (-1L) 1_000_000L = 1L
  let%test _ = int64_pow (-1L) 1_000_001L = -1L

  let%test _ = int64_pow 10L 1L  = 10L
  let%test _ = int64_pow 10L 2L  = 100L
  let%test _ = int64_pow 10L 3L  = 1_000L
  let%test _ = int64_pow 10L 4L  = 10_000L
  let%test _ = int64_pow 10L 5L  = 100_000L
  let%test _ = int64_pow 2L  10L = 1_024L
  let%test _ = int64_pow 5L  27L = 7450580596923828125L

  let exception_thrown pow b e = try let _ = pow b e in false with _ -> true;;

  let%test _ = exception_thrown int_pow 10 60
  let%test _ = exception_thrown int64_pow 10L 60L
  let%test _ = exception_thrown int_pow 10 (-1)
  let%test _ = exception_thrown int64_pow 10L (-1L)

  let%test _ = exception_thrown int64_pow 2L 63L
  let%test _ = not (exception_thrown int64_pow 2L 62L)

  let%test _ = exception_thrown int64_pow (-2L) 63L
  let%test _ = not (exception_thrown int64_pow (-2L) 62L)
end)

let%test_module "overflow_bounds" = (module struct
  let%test _ = Pow_overflow_bounds.overflow_bound_max_int_value = Pervasives.max_int
  let%test _ = Pow_overflow_bounds.overflow_bound_max_int64_value = Int64.max_int

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

  let%test_unit _ = test_overflow_table Pow_overflow_bounds.int_positive_overflow_bounds
                Big_int.big_int_of_int Pervasives.max_int

  let%test_unit _ = test_overflow_table Pow_overflow_bounds.int64_positive_overflow_bounds
                Big_int.big_int_of_int64 Int64.max_int
end)
