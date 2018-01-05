open! Core_kernel
open  Expect_test_helpers_kernel
open  Core_kernel_private.Digit_string_helpers

let max_with ~digits =
  (Int.pow 10 digits) - 1

let test_write_int write_int ~digits =
  let max = max_with ~digits in
  print_endline "Expecting success:";
  (* show resulting strings at boundary values *)
  let show int =
    let bytes = Bytes.make digits '!' in
    write_int bytes ~pos:0 int;
    printf "%d -> %S\n" int (Bytes.to_string bytes)
  in
  show 0;
  show max;
  (* test success behavior for lots of correct values *)
  let expect_success_exn int =
    let bytes = Bytes.make (1 + digits + 1) '!' in
    write_int bytes ~pos:1 int;
    [%test_result: string]
      (Bytes.to_string bytes)
      ~expect:(sprintf ("!%0*d!") digits int)
  in
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      (Int.gen_log_uniform_incl 0 max)
      ~examples:[0; max]
      ~sexp_of:Int.sexp_of_t
      ~f:expect_success_exn);
  (* test failure cases *)
  print_endline "";
  print_endline "Expecting failure:";
  require_does_raise [%here] (fun () ->
    write_int (Bytes.make 0 '?') ~pos:0 0);
  require_does_raise [%here] (fun () ->
    write_int (Bytes.make digits '?') ~pos:(-1) 0);
  require_does_raise [%here] (fun () ->
    write_int (Bytes.make digits '?') ~pos:1 0);
  require_does_raise [%here] (fun () ->
    write_int (Bytes.make digits '?') ~pos:0 (-1));
  require_does_raise [%here] (fun () ->
    write_int (Bytes.make digits '?') ~pos:0 (max + 1));
;;

let%expect_test "write_1_digit_int" =
  test_write_int write_1_digit_int ~digits:1;
  [%expect {|
    Expecting success:
    0 -> "0"
    9 -> "9"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_1_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_1_digit_int: pos=-1 out of range for string of length 1")
    (Invalid_argument
     "Digit_string_helpers.write_1_digit_int: pos=1 out of range for string of length 1")
    (Invalid_argument
     "Digit_string_helpers.write_1_digit_int: -1 out of range [0, 9]")
    (Invalid_argument
     "Digit_string_helpers.write_1_digit_int: 10 out of range [0, 9]") |}];
;;

let%expect_test "write_2_digit_int" =
  test_write_int write_2_digit_int ~digits:2;
  [%expect {|
    Expecting success:
    0 -> "00"
    99 -> "99"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: pos=-1 out of range for string of length 2")
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: 2 digits do not fit at pos 1 in string of length 2")
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: -1 out of range [0, 99]")
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: 100 out of range [0, 99]") |}];
;;


let%expect_test "write_3_digit_int" =
  test_write_int write_3_digit_int ~digits:3;
  [%expect {|
    Expecting success:
    0 -> "000"
    999 -> "999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: pos=-1 out of range for string of length 3")
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: 3 digits do not fit at pos 1 in string of length 3")
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: -1 out of range [0, 999]")
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: 1000 out of range [0, 999]") |}];
;;


let%expect_test "write_4_digit_int" =
  test_write_int write_4_digit_int ~digits:4;
  [%expect {|
    Expecting success:
    0 -> "0000"
    9999 -> "9999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: pos=-1 out of range for string of length 4")
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: 4 digits do not fit at pos 1 in string of length 4")
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: -1 out of range [0, 9999]")
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: 10000 out of range [0, 9999]") |}];
;;

let%expect_test "write_5_digit_int" =
  test_write_int write_5_digit_int ~digits:5;
  [%expect {|
    Expecting success:
    0 -> "00000"
    99999 -> "99999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_5_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_5_digit_int: pos=-1 out of range for string of length 5")
    (Invalid_argument
     "Digit_string_helpers.write_5_digit_int: 5 digits do not fit at pos 1 in string of length 5")
    (Invalid_argument
     "Digit_string_helpers.write_5_digit_int: -1 out of range [0, 99999]")
    (Invalid_argument
     "Digit_string_helpers.write_5_digit_int: 100000 out of range [0, 99999]") |}];
;;

let%expect_test "write_6_digit_int" =
  test_write_int write_6_digit_int ~digits:6;
  [%expect {|
    Expecting success:
    0 -> "000000"
    999999 -> "999999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_6_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_6_digit_int: pos=-1 out of range for string of length 6")
    (Invalid_argument
     "Digit_string_helpers.write_6_digit_int: 6 digits do not fit at pos 1 in string of length 6")
    (Invalid_argument
     "Digit_string_helpers.write_6_digit_int: -1 out of range [0, 999999]")
    (Invalid_argument
     "Digit_string_helpers.write_6_digit_int: 1000000 out of range [0, 999999]") |}];
;;

let%expect_test "write_7_digit_int" =
  test_write_int write_7_digit_int ~digits:7;
  [%expect {|
    Expecting success:
    0 -> "0000000"
    9999999 -> "9999999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_7_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_7_digit_int: pos=-1 out of range for string of length 7")
    (Invalid_argument
     "Digit_string_helpers.write_7_digit_int: 7 digits do not fit at pos 1 in string of length 7")
    (Invalid_argument
     "Digit_string_helpers.write_7_digit_int: -1 out of range [0, 9999999]")
    (Invalid_argument
     "Digit_string_helpers.write_7_digit_int: 10000000 out of range [0, 9999999]") |}];
;;

let%expect_test "write_8_digit_int" =
  test_write_int write_8_digit_int ~digits:8;
  [%expect {|
    Expecting success:
    0 -> "00000000"
    99999999 -> "99999999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_8_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_8_digit_int: pos=-1 out of range for string of length 8")
    (Invalid_argument
     "Digit_string_helpers.write_8_digit_int: 8 digits do not fit at pos 1 in string of length 8")
    (Invalid_argument
     "Digit_string_helpers.write_8_digit_int: -1 out of range [0, 99999999]")
    (Invalid_argument
     "Digit_string_helpers.write_8_digit_int: 100000000 out of range [0, 99999999]") |}];
;;

let%expect_test "write_9_digit_int" =
  test_write_int write_9_digit_int ~digits:9;
  [%expect {|
    Expecting success:
    0 -> "000000000"
    999999999 -> "999999999"

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.write_9_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.write_9_digit_int: pos=-1 out of range for string of length 9")
    (Invalid_argument
     "Digit_string_helpers.write_9_digit_int: 9 digits do not fit at pos 1 in string of length 9")
    (Invalid_argument
     "Digit_string_helpers.write_9_digit_int: -1 out of range [0, 999999999]")
    (Invalid_argument
     "Digit_string_helpers.write_9_digit_int: 1000000000 out of range [0, 999999999]") |}];
;;

let test_read_int read_int ~digits =
  let max = max_with ~digits in
  print_endline "Expecting success:";
  (* show resulting strings at boundary values *)
  let show int =
    let string = sprintf "%0*d" digits int in
    let parsed = read_int string ~pos:0 in
    printf "%S -> %d\n" string parsed
  in
  show 0;
  show max;
  (* test success behavior for lots of correct values *)
  let expect_success_exn int =
    let string = sprintf "!%0*d!" digits int in
    let parsed = read_int string ~pos:1 in
    [%test_result: int] parsed ~expect:int
  in
  require_does_not_raise [%here] (fun () ->
    Quickcheck.test
      (Int.gen_log_uniform_incl 0 max)
      ~examples:[0; max]
      ~sexp_of:Int.sexp_of_t
      ~f:expect_success_exn);
  (* test failure cases *)
  print_endline "";
  print_endline "Expecting failure:";
  require_does_raise [%here] (fun () ->
    read_int "" ~pos:0);
  require_does_raise [%here] (fun () ->
    read_int (sprintf "%0*d" digits max) ~pos:(-1));
  require_does_raise [%here] (fun () ->
    read_int (sprintf "%0*d" digits max) ~pos:1);
  require_does_raise [%here] (fun () ->
    read_int (String.make digits '!') ~pos:0);
;;

let%expect_test "read_1_digit_int" =
  test_read_int read_1_digit_int ~digits:1;
  [%expect {|
    Expecting success:
    "0" -> 0
    "9" -> 9

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_1_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_1_digit_int: pos=-1 out of range for string of length 1")
    (Invalid_argument
     "Digit_string_helpers.read_1_digit_int: pos=1 out of range for string of length 1")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_2_digit_int" =
  test_read_int read_2_digit_int ~digits:2;
  [%expect {|
    Expecting success:
    "00" -> 0
    "99" -> 99

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_2_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_2_digit_int: pos=-1 out of range for string of length 2")
    (Invalid_argument
     "Digit_string_helpers.read_2_digit_int: 2 digits do not fit at pos 1 in string of length 2")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_3_digit_int" =
  test_read_int read_3_digit_int ~digits:3;
  [%expect {|
    Expecting success:
    "000" -> 0
    "999" -> 999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_3_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_3_digit_int: pos=-1 out of range for string of length 3")
    (Invalid_argument
     "Digit_string_helpers.read_3_digit_int: 3 digits do not fit at pos 1 in string of length 3")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_4_digit_int" =
  test_read_int read_4_digit_int ~digits:4;
  [%expect {|
    Expecting success:
    "0000" -> 0
    "9999" -> 9999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_4_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_4_digit_int: pos=-1 out of range for string of length 4")
    (Invalid_argument
     "Digit_string_helpers.read_4_digit_int: 4 digits do not fit at pos 1 in string of length 4")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_5_digit_int" =
  test_read_int read_5_digit_int ~digits:5;
  [%expect {|
    Expecting success:
    "00000" -> 0
    "99999" -> 99999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_5_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_5_digit_int: pos=-1 out of range for string of length 5")
    (Invalid_argument
     "Digit_string_helpers.read_5_digit_int: 5 digits do not fit at pos 1 in string of length 5")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_6_digit_int" =
  test_read_int read_6_digit_int ~digits:6;
  [%expect {|
    Expecting success:
    "000000" -> 0
    "999999" -> 999999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_6_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_6_digit_int: pos=-1 out of range for string of length 6")
    (Invalid_argument
     "Digit_string_helpers.read_6_digit_int: 6 digits do not fit at pos 1 in string of length 6")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_7_digit_int" =
  test_read_int read_7_digit_int ~digits:7;
  [%expect {|
    Expecting success:
    "0000000" -> 0
    "9999999" -> 9999999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_7_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_7_digit_int: pos=-1 out of range for string of length 7")
    (Invalid_argument
     "Digit_string_helpers.read_7_digit_int: 7 digits do not fit at pos 1 in string of length 7")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_8_digit_int" =
  test_read_int read_8_digit_int ~digits:8;
  [%expect {|
    Expecting success:
    "00000000" -> 0
    "99999999" -> 99999999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_8_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_8_digit_int: pos=-1 out of range for string of length 8")
    (Invalid_argument
     "Digit_string_helpers.read_8_digit_int: 8 digits do not fit at pos 1 in string of length 8")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_9_digit_int" =
  test_read_int read_9_digit_int ~digits:9;
  [%expect {|
    Expecting success:
    "000000000" -> 0
    "999999999" -> 999999999

    Expecting failure:
    (Invalid_argument
     "Digit_string_helpers.read_9_digit_int: pos=0 out of range for string of length 0")
    (Invalid_argument
     "Digit_string_helpers.read_9_digit_int: pos=-1 out of range for string of length 9")
    (Invalid_argument
     "Digit_string_helpers.read_9_digit_int: 9 digits do not fit at pos 1 in string of length 9")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;
