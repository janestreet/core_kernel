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

let%expect_test "write_2_digit_int" =
  test_write_int write_2_digit_int ~digits:2;
  [%expect {|
    Expecting success:
    0 -> "00"
    99 -> "99"

    Expecting failure:
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: argument must be (0, 99) -1")
    (Invalid_argument
     "Digit_string_helpers.write_2_digit_int: argument must be (0, 99) 100") |}];
;;

let%expect_test "write_3_digit_int" =
  test_write_int write_3_digit_int ~digits:3;
  [%expect {|
    Expecting success:
    0 -> "000"
    999 -> "999"

    Expecting failure:
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: argument must be (0, 999) -1")
    (Invalid_argument
     "Digit_string_helpers.write_3_digit_int: argument must be (0, 999) 1000") |}];
;;

let%expect_test "write_4_digit_int" =
  test_write_int write_4_digit_int ~digits:4;
  [%expect {|
    Expecting success:
    0 -> "0000"
    9999 -> "9999"

    Expecting failure:
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: argument must be (0, 9999) -1")
    (Invalid_argument
     "Digit_string_helpers.write_4_digit_int: argument must be (0, 9999) 10000") |}];
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
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_2_digit_int" =
  test_read_int read_2_digit_int ~digits:2;
  [%expect {|
    Expecting success:
    "00" -> 0
    "99" -> 99

    Expecting failure:
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;

let%expect_test "read_4_digit_int" =
  test_read_int read_4_digit_int ~digits:4;
  [%expect {|
    Expecting success:
    "0000" -> 0
    "9999" -> 9999

    Expecting failure:
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Invalid_argument "index out of bounds")
    (Failure "Char.get_digit_exn '!': not a digit") |}];
;;
