open! Core_kernel
open! Import
open! Timing_wheel_ns

include Timing_wheel_unit_tests.Make (Timing_wheel_ns)

module Alarm_precision = struct
  include Alarm_precision

  let sexp_of_t t =
    [%message
      ""
        ~_:(t : t)
        ~_:(String.concat [ t
                            |> to_span
                            |> Time_ns.Span.to_int63_ns
                            |> Int63.to_string_hum
                          ; "ns" ])];
  ;;

  let print t = print_s [%sexp (t : t)]

  let%expect_test "constants" =
    print about_one_day;
    [%expect {|
      (19.546873382684446h 70_368_744_177_664ns) |}];
    print about_one_second;
    [%expect {|
      (1.073741824s 1_073_741_824ns) |}];
    print about_one_microsecond;
    [%expect {|
      (1.024us 1_024ns) |}];
    print about_one_millisecond;
    [%expect {|
      (1.048576ms 1_048_576ns) |}];
    print one_nanosecond;
    [%expect {|
     (1ns 1ns) |}];
  ;;

  let%expect_test "[div]" =
    for pow2 = -3 to 3 do
      print (div about_one_second ~pow2);
    done;
    [%expect {|
      (8.589934592s 8_589_934_592ns)
      (4.294967296s 4_294_967_296ns)
      (2.147483648s 2_147_483_648ns)
      (1.073741824s 1_073_741_824ns)
      (536.870912ms 536_870_912ns)
      (268.435456ms 268_435_456ns)
      (134.217728ms 134_217_728ns) |}];
  ;;

  let%expect_test "[mul]" =
    for pow2 = -3 to 3 do
      print (mul about_one_second ~pow2);
    done;
    [%expect {|
      (134.217728ms 134_217_728ns)
      (268.435456ms 268_435_456ns)
      (536.870912ms 536_870_912ns)
      (1.073741824s 1_073_741_824ns)
      (2.147483648s 2_147_483_648ns)
      (4.294967296s 4_294_967_296ns)
      (8.589934592s 8_589_934_592ns) |}];
  ;;

  let%expect_test "[of_span_floor_pow2_ns]" [@tags "64-bits-only"] =
    List.iter
      [ about_one_day
      ; about_one_second
      ; about_one_millisecond
      ; about_one_microsecond
      ; one_nanosecond ]
      ~f:(fun t ->
        require [%here] (equal t (t |> to_span |> of_span_floor_pow2_ns));
        if Time_ns.Span.( > ) (t |> to_span) Time_ns.Span.nanosecond
        then (
          require [%here] (equal t
                             (Time_ns.Span.( + ) (t |> to_span) Time_ns.Span.nanosecond
                              |> of_span_floor_pow2_ns))));
    List.iter
      [ 1.
      ; 1E-3
      ; 1E-6 ]
      ~f:(fun span ->
        let span = Time_ns.Span.of_sec span in
        print_s [%message
          ""
            (span : Time_ns.Span.Alternate_sexp.t)
            ~alarm_precision:(span |> of_span_floor_pow2_ns : t)]);
    [%expect {|
      ((span 1s) (alarm_precision (536.870912ms 536_870_912ns)))
      ((span 1ms) (alarm_precision (524.288us 524_288ns)))
      ((span 1us) (alarm_precision (512ns 512ns))) |}];
  ;;
end

let%expect_test "[Config.microsecond_precision]" =
  print_s [%sexp (Config.microsecond_precision () : Config.t)];
  [%expect {|
    ((alarm_precision 1.024us) (level_bits (10 10 6 6 5))) |}];
  print_s [%sexp (Config.durations (Config.microsecond_precision ())
                  : Time_ns.Span.Alternate_sexp.t list)];
  [%expect {|
    (1.048576ms
     1.073741824s
     1.1453246122666667m
     1.2216795864177779h
     1.6289061152237037d) |}];
;;
