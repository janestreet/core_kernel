open! Core
include Iobuf_numeric_intf.Definitions
open Iobuf_type

let arch_sixtyfour = Sys.word_size_in_bits = 64

(* [Itoa] provides a range of functions for integer to ASCII conversion, used by [Poke],
   [Fill] and their [Unsafe] versions.

   The implementation here is done in terms of negative decimals due to the properties of
   [Int.min_value]. Since the result of [Int.(abs min_value)] is [Int.min_value], an
   attempt to utilize a positive decimal loop by writing the sign and calling [Int.abs x]
   fails. The converse, with [- Int.max_value] works for both cases. *)
module Itoa = struct
  (* [num_digits_neg x] returns the number of digits in [x] for non-positive integers
     ([num_digits_neg 0] is defined as 1).

     The below tends to perform better than a binary search or [/= 10 while <> 0], likely
     due to decimal values for our applications skewing towards smaller numbers. *)
  let num_digits_neg x =
    if x > -10
    then 1
    else if x > -100
    then 2
    else if x > -1000
    then 3
    else if x > -10000
    then 4
    else if x > -100000
    then 5
    else if x > -1000000
    then 6
    else if x > -10000000
    then 7
    else if x > -100000000
    then 8
    else if x > -1000000000
    then 9
    else if arch_sixtyfour
    then
      if x > -1000000000 * 10
      then 10
      else if x > -1000000000 * 100
      then 11
      else if x > -1000000000 * 1000
      then 12
      else if x > -1000000000 * 10000
      then 13
      else if x > -1000000000 * 100000
      then 14
      else if x > -1000000000 * 1000000
      then 15
      else if x > -1000000000 * 10000000
      then 16
      else if x > -1000000000 * 100000000
      then 17
      else if x > -1000000000 * 1000000000
      then 18
      else 19
    else 10
  ;;

  let num_digits x = if x < 0 then num_digits_neg x else num_digits_neg (-x)
  let min_len x = Bool.to_int (x < 0) + num_digits x
  let () = assert (String.length (Int.to_string Int.min_value) <= 19 + 1)

  (* Despite the div/mod by a constant optimizations, it's a slight savings to avoid a
     second div/mod. Note also that passing in an [int ref], rather than creating the ref
     locally here, results in allocation on the benchmarks. *)
  let unsafe_poke_negative_decimal_without_sign buf ~pos ~len int =
    let int = ref int in
    for pos = pos + len - 1 downto pos do
      let x = !int in
      int := !int / 10;
      Bigstring.unsafe_set buf pos (Char.unsafe_of_int (48 + (-x + (!int * 10))))
    done
  ;;

  let unsafe_poke_negative_decimal buf ~pos ~len int =
    Bigstring.unsafe_set buf pos '-';
    (* +1 and -1 to account for '-' *)
    unsafe_poke_negative_decimal_without_sign buf ~pos:(pos + 1) ~len:(len - 1) int
  ;;

  (* This function pokes a "trunc"ated decimal of length exactly [len]. If [int] is
     positive, then this will be the (at most) [len] least-significant digits, left-padded
     with '0', whereas if [int] is negative, it will be the (at most) [len - 1]
     least-significant digits, left-padded with '0', prefixed by the sign ('-').

     E.g. for [len = 3]:
     - 5 -> "005"
     - -5 -> "-05"
     - 50 -> "050"
     - -50 -> "-50"
     - 500 -> "500"
     - -500 -> "-00"

     The publicly-exposed functions compute the necessary [len] to prevent any digits from
     being truncated, but this function is used internally in cases where we are already
     confident the decimal will fit and can thus skip the extra work. *)
  let[@inline] gen_poke_padded_decimal_trunc ~buf_pos t ~pos ~len int =
    let pos = (buf_pos [@inlined hint]) t ~pos ~len in
    if int < 0
    then
      unsafe_poke_negative_decimal
        ([%template buf [@mode local]] t)
        ~pos
        ~len
        int [@nontail]
    else
      unsafe_poke_negative_decimal_without_sign
        ([%template buf [@mode local]] t)
        ~pos
        ~len
        (-int) [@nontail]
  ;;

  (* See [gen_poke_padded_decimal_trunc] re: truncation. *)
  let poke_padded_decimal_trunc t ~pos ~len int =
    (gen_poke_padded_decimal_trunc [@inlined hint]) ~buf_pos:buf_pos_exn t ~pos ~len int
  ;;

  (* See [gen_poke_padded_decimal_trunc] re: truncation. *)
  let unsafe_poke_padded_decimal_trunc t ~pos ~len int =
    (gen_poke_padded_decimal_trunc [@inlined hint])
      ~buf_pos:unsafe_buf_pos
      t
      ~pos
      ~len
      int
  ;;

  let[@inline] gen_poke_padded_decimal ~poke_padded_decimal_trunc t ~pos ~len int =
    let len = max len (min_len int) in
    (poke_padded_decimal_trunc [@inlined hint]) t ~pos ~len int;
    len
  ;;

  let poke_padded_decimal t ~pos ~len int =
    (gen_poke_padded_decimal [@inlined hint]) ~poke_padded_decimal_trunc t ~pos ~len int
  ;;

  let unsafe_poke_padded_decimal t ~pos ~len int =
    (gen_poke_padded_decimal [@inlined hint])
      ~poke_padded_decimal_trunc:unsafe_poke_padded_decimal_trunc
      t
      ~pos
      ~len
      int
  ;;

  let[@inline] gen_poke_decimal ~poke_padded_decimal_trunc t ~pos int =
    let len = min_len int in
    (poke_padded_decimal_trunc [@inlined hint]) t ~pos ~len int;
    len
  ;;

  let poke_decimal t ~pos int =
    (gen_poke_decimal [@inlined hint]) ~poke_padded_decimal_trunc t ~pos int
  ;;

  let unsafe_poke_decimal t ~pos int =
    (gen_poke_decimal [@inlined hint])
      ~poke_padded_decimal_trunc:unsafe_poke_padded_decimal_trunc
      t
      ~pos
      int
  ;;
end

module Date_string = struct
  let len_iso8601_extended = 10

  let[@inline] gen_poke_iso8601_extended ~buf_pos t ~pos date =
    let pos = (buf_pos [@inlined hint]) t ~pos ~len:len_iso8601_extended in
    Itoa.unsafe_poke_negative_decimal_without_sign
      ([%template buf [@mode local]] t)
      ~pos
      ~len:4
      (-Date.year date);
    let pos = pos + 4 in
    Itoa.unsafe_poke_negative_decimal
      ([%template buf [@mode local]] t)
      ~pos
      ~len:3
      (-Month.to_int (Date.month date));
    let pos = pos + 3 in
    Itoa.unsafe_poke_negative_decimal
      ([%template buf [@mode local]] t)
      ~pos
      ~len:3
      (-Date.day date) [@nontail]
  ;;

  let poke_iso8601_extended t ~pos date =
    (gen_poke_iso8601_extended [@inlined hint]) ~buf_pos:buf_pos_exn t ~pos date
  ;;

  let unsafe_poke_iso8601_extended t ~pos date =
    (gen_poke_iso8601_extended [@inlined hint]) ~buf_pos:unsafe_buf_pos t ~pos date
  ;;
end
