open Interfaces
open Common

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
