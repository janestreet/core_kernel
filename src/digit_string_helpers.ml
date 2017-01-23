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
  s.[pos + 3] <- char_of_digit (i - j * 10);
  let k = j / 10 in
  s.[pos + 2] <- char_of_digit (j - k * 10);
  let l = k / 10 in
  s.[pos + 1] <- char_of_digit (k - l * 10);
  s.[pos    ] <- char_of_digit l;
;;

let%test_unit _ =
  for i = 0 to 9999 do
    let s = String.make 4 ' ' in
    blit_string_of_int_4_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%04d" i) s
  done
;;

let blit_string_of_int_2_digits s ~pos i =
  if i >= 100 || i < 0 then invalid_range ~digits:4 ~max:99 ~i;
  let j = i / 10 in
  s.[pos + 1] <- char_of_digit (i - j * 10);
  s.[pos    ] <- char_of_digit j;
;;

let%test_unit _ =
  for i = 0 to 99 do
    let s = String.make 2 ' ' in
    blit_string_of_int_2_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%02d" i) s
  done
;;

let blit_string_of_int_3_digits s ~pos i =
  if i >= 1000 || i < 0 then invalid_range ~digits:4 ~max:999 ~i;
  let j = i / 10 in
  s.[pos + 2] <- char_of_digit (i - j * 10);
  let k = j / 10 in
  s.[pos + 1] <- char_of_digit (j - k * 10);
  s.[pos    ] <- char_of_digit k;
;;

let%test_unit _ =
  for i = 0 to 999 do
    let s = String.make 3 ' ' in
    blit_string_of_int_3_digits s ~pos:0 i;
    [%test_result: string] ~expect:(Printf.sprintf "%03d" i) s
  done
;;

let parse_two_digits str pos =
  let d1 = Char.get_digit_exn str.[pos] in
  let d2 = Char.get_digit_exn str.[pos + 1] in
  10 * d1 + d2
;;

let parse_four_digits str pos =
  parse_two_digits str pos * 100 + parse_two_digits str (pos + 2)
;;
