open! Import
open Std_internal

let unsafe_char_of_digit n = Char.unsafe_of_int (Char.to_int '0' + n)

let invalid_range ~digits ~max ~i =
  invalid_argf
    "Digit_string_helpers.write_%d_digit_int: argument must be (0, %d) %d"
    digits max i ()
;;

let write_4_digit_int s ~pos i =
  if i >= 10000 || i < 0 then invalid_range ~digits:4 ~max:9999 ~i;
  let j = i / 10 in
  Bytes.set s (pos + 3) (unsafe_char_of_digit (i - j * 10));
  let k = j / 10 in
  Bytes.set s (pos + 2) (unsafe_char_of_digit (j - k * 10));
  let l = k / 10 in
  Bytes.set s (pos + 1) (unsafe_char_of_digit (k - l * 10));
  Bytes.set s pos (unsafe_char_of_digit l);
;;

let write_2_digit_int s ~pos i =
  if i >= 100 || i < 0 then invalid_range ~digits:2 ~max:99 ~i;
  let j = i / 10 in
  Bytes.set s (pos + 1) (unsafe_char_of_digit (i - j * 10));
  Bytes.set s pos (unsafe_char_of_digit j);
;;

let write_3_digit_int s ~pos i =
  if i >= 1000 || i < 0 then invalid_range ~digits:3 ~max:999 ~i;
  let j = i / 10 in
  Bytes.set s (pos + 2) (unsafe_char_of_digit (i - j * 10));
  let k = j / 10 in
  Bytes.set s (pos + 1) (unsafe_char_of_digit (j - k * 10));
  Bytes.set s pos (unsafe_char_of_digit k);
;;

let read_1_digit_int str ~pos =
  Char.get_digit_exn str.[pos]
;;

let read_2_digit_int str ~pos =
  read_1_digit_int str ~pos * 10 + read_1_digit_int str ~pos:(pos + 1)
;;

let read_4_digit_int str ~pos =
  read_2_digit_int str ~pos * 100 + read_2_digit_int str ~pos:(pos + 2)
;;
