open! Import
open Std_internal

let char_of_digit n = Char.unsafe_of_int (Char.to_int '0' + n)

let invalid_range ~digits ~max ~i =
  invalid_argf
    "Time.string_of_int_%d_digits: argument must be (0, %d) %d"
    digits max i ()
;;

let blit_string_of_int_4_digits s ~pos i =
  if i >= 10000 || i < 0 then invalid_range ~digits:4 ~max:9999 ~i;
  let j = i / 10 in
  Bytes.set s (pos + 3) (char_of_digit (i - j * 10));
  let k = j / 10 in
  Bytes.set s (pos + 2) (char_of_digit (j - k * 10));
  let l = k / 10 in
  Bytes.set s (pos + 1) (char_of_digit (k - l * 10));
  Bytes.set s pos (char_of_digit l);
;;

let blit_string_of_int_2_digits s ~pos i =
  if i >= 100 || i < 0 then invalid_range ~digits:4 ~max:99 ~i;
  let j = i / 10 in
  Bytes.set s (pos + 1) (char_of_digit (i - j * 10));
  Bytes.set s pos (char_of_digit j);
;;

let blit_string_of_int_3_digits s ~pos i =
  if i >= 1000 || i < 0 then invalid_range ~digits:4 ~max:999 ~i;
  let j = i / 10 in
  Bytes.set s (pos + 2) (char_of_digit (i - j * 10));
  let k = j / 10 in
  Bytes.set s (pos + 1) (char_of_digit (j - k * 10));
  Bytes.set s pos (char_of_digit k);
;;

let parse_one_digit str pos =
  Char.get_digit_exn str.[pos]

let parse_two_digits str pos =
  parse_one_digit str pos * 10 + parse_one_digit str (pos + 1)
;;

let parse_four_digits str pos =
  parse_two_digits str pos * 100 + parse_two_digits str (pos + 2)
;;
