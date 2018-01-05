open! Import
open Std_internal

module Unsafe = struct
  let unsafe_char_of_digit n = Char.unsafe_of_int (Char.to_int '0' + n)

  let digit_of_char char = Char.get_digit_exn char

  let write_1_digit_int bytes ~pos int =
    Bytes.unsafe_set bytes pos (unsafe_char_of_digit int)

  let return_tens_and_write_ones bytes ~pos int =
    let tens = int / 10 in
    let ones = int - (tens * 10) in
    write_1_digit_int bytes ~pos ones;
    tens

  let write_2_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 1) int in
    write_1_digit_int bytes ~pos tens

  let write_3_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 2) int in
    write_2_digit_int bytes ~pos tens

  let write_4_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 3) int in
    write_3_digit_int bytes ~pos tens

  let write_5_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 4) int in
    write_4_digit_int bytes ~pos tens

  let write_6_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 5) int in
    write_5_digit_int bytes ~pos tens

  let write_7_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 6) int in
    write_6_digit_int bytes ~pos tens

  let write_8_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 7) int in
    write_7_digit_int bytes ~pos tens

  let write_9_digit_int bytes ~pos int =
    let tens = return_tens_and_write_ones bytes ~pos:(pos + 8) int in
    write_8_digit_int bytes ~pos tens

  let read_1_digit_int string ~pos =
    digit_of_char (String.unsafe_get string pos)

  let read_2_digit_int string ~pos =
    (read_1_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 1)

  let read_3_digit_int string ~pos =
    (read_2_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 2)

  let read_4_digit_int string ~pos =
    (read_3_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 3)

  let read_5_digit_int string ~pos =
    (read_4_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 4)

  let read_6_digit_int string ~pos =
    (read_5_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 5)

  let read_7_digit_int string ~pos =
    (read_6_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 6)

  let read_8_digit_int string ~pos =
    (read_7_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 7)

  let read_9_digit_int string ~pos =
    (read_8_digit_int string ~pos * 10) + read_1_digit_int string ~pos:(pos + 8)
end

let module_name = "Digit_string_helpers"

let raise_pos_out_of_bounds name ~len ~pos ~digits =
  if pos < 0 || pos >= len
  then
    invalid_argf "%s.%s: pos=%d out of range for string of length %d"
      module_name name pos len ()
  else
    invalid_argf "%s.%s: %d digits do not fit at pos %d in string of length %d"
      module_name name digits pos len ()

let raise_int_out_of_bounds name ~max int =
  invalid_argf "%s.%s: %d out of range [0, %d]"
    module_name
    name
    int
    max
    ()

let check_pos name ~len ~pos ~digits =
  if pos < 0 || pos + digits > len
  then raise_pos_out_of_bounds name ~len ~pos ~digits

let check_int name ~max int =
  if int < 0 || int > max
  then raise_int_out_of_bounds name ~max int

let check_write name ~bytes ~pos ~digits ~max int =
  let len = Bytes.length bytes in
  check_pos name ~digits ~len ~pos;
  check_int name ~max int
;;

let write_1_digit_int bytes ~pos int =
  check_write "write_1_digit_int" ~bytes ~pos ~digits:1 ~max:9 int;
  Unsafe.write_1_digit_int bytes ~pos int

let write_2_digit_int bytes ~pos int =
  check_write "write_2_digit_int" ~bytes ~pos ~digits:2 ~max:99 int;
  Unsafe.write_2_digit_int bytes ~pos int

let write_3_digit_int bytes ~pos int =
  check_write "write_3_digit_int" ~bytes ~pos ~digits:3 ~max:999 int;
  Unsafe.write_3_digit_int bytes ~pos int

let write_4_digit_int bytes ~pos int =
  check_write "write_4_digit_int" ~bytes ~pos ~digits:4 ~max:9_999 int;
  Unsafe.write_4_digit_int bytes ~pos int

let write_5_digit_int bytes ~pos int =
  check_write "write_5_digit_int" ~bytes ~pos ~digits:5 ~max:99_999 int;
  Unsafe.write_5_digit_int bytes ~pos int

let write_6_digit_int bytes ~pos int =
  check_write "write_6_digit_int" ~bytes ~pos ~digits:6 ~max:999_999 int;
  Unsafe.write_6_digit_int bytes ~pos int

let write_7_digit_int bytes ~pos int =
  check_write "write_7_digit_int" ~bytes ~pos ~digits:7 ~max:9_999_999 int;
  Unsafe.write_7_digit_int bytes ~pos int

let write_8_digit_int bytes ~pos int =
  check_write "write_8_digit_int" ~bytes ~pos ~digits:8 ~max:99_999_999 int;
  Unsafe.write_8_digit_int bytes ~pos int

let write_9_digit_int bytes ~pos int =
  check_write "write_9_digit_int" ~bytes ~pos ~digits:9 ~max:999_999_999 int;
  Unsafe.write_9_digit_int bytes ~pos int

let check_read name ~string ~pos ~digits =
  let len = String.length string in
  check_pos name ~digits ~len ~pos;
;;

let read_1_digit_int string ~pos =
  check_read "read_1_digit_int" ~string ~pos ~digits:1;
  Unsafe.read_1_digit_int string ~pos

let read_2_digit_int string ~pos =
  check_read "read_2_digit_int" ~string ~pos ~digits:2;
  Unsafe.read_2_digit_int string ~pos

let read_3_digit_int string ~pos =
  check_read "read_3_digit_int" ~string ~pos ~digits:3;
  Unsafe.read_3_digit_int string ~pos

let read_4_digit_int string ~pos =
  check_read "read_4_digit_int" ~string ~pos ~digits:4;
  Unsafe.read_4_digit_int string ~pos

let read_5_digit_int string ~pos =
  check_read "read_5_digit_int" ~string ~pos ~digits:5;
  Unsafe.read_5_digit_int string ~pos

let read_6_digit_int string ~pos =
  check_read "read_6_digit_int" ~string ~pos ~digits:6;
  Unsafe.read_6_digit_int string ~pos

let read_7_digit_int string ~pos =
  check_read "read_7_digit_int" ~string ~pos ~digits:7;
  Unsafe.read_7_digit_int string ~pos

let read_8_digit_int string ~pos =
  check_read "read_8_digit_int" ~string ~pos ~digits:8;
  Unsafe.read_8_digit_int string ~pos

let read_9_digit_int string ~pos =
  check_read "read_9_digit_int" ~string ~pos ~digits:9;
  Unsafe.read_9_digit_int string ~pos
