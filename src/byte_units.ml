(* Conversions between units of measure based on bytes. *)

open! Import
open Std_internal
module Repr = Int63

module T : sig
  type t [@@deriving compare, hash, sexp_of]

  val of_repr : Repr.t -> t
  val to_repr : t -> Repr.t
end = struct
  type t = Repr.t [@@deriving compare, hash]

  let of_repr = Fn.id
  let to_repr = Fn.id
  let sexp_of_t t = [%sexp Bytes (t : Repr.t)]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

module Infix = struct
  let ( - ) a b = of_repr (Repr.( - ) (to_repr a) (to_repr b))
  let ( + ) a b = of_repr (Repr.( + ) (to_repr a) (to_repr b))
  let ( // ) a b = Repr.( // ) (to_repr a) (to_repr b)

  let ( / ) t s = of_repr (Repr.of_float (Repr.to_float (to_repr t) /. s))
  let ( * ) t s = of_repr (Repr.of_float (Repr.to_float (to_repr t) *. s))
end

let scale = Infix.( * )
let iscale t s = of_repr (Repr.( * ) (to_repr t) (Repr.of_int s))
let bytes_int_exn t = Repr.to_int_exn (to_repr t)
let bytes_int63 = to_repr
let bytes_int64 t = Repr.to_int64 (to_repr t)
let bytes_float t = Repr.to_float (to_repr t)
let of_bytes_int b = of_repr (Repr.of_int b)
let of_bytes_int63 = of_repr
let of_bytes_int64_exn b = of_repr (Repr.of_int64_exn b)
let of_bytes_float_exn b = of_repr (Repr.of_float b)

let[@deprecated
  "[since 2019-01] Use [bytes_int_exn], [bytes_int63], [bytes_int64] or \
   [bytes_float] as appropriate."] bytes
  =
  bytes_float
;;

let[@deprecated
  "[since 2019-01] Use [of_bytes_int], [of_bytes_int63], [of_bytes_int64_exn] or \
   [of_bytes_float_exn] as appropriate."] of_bytes
  =
  of_bytes_float_exn
;;

let kilobyte : t = of_bytes_int 1024
let megabyte = iscale kilobyte 1024
let gigabyte = iscale megabyte 1024
let terabyte = iscale gigabyte 1024
let petabyte = iscale terabyte 1024
let exabyte = iscale petabyte 1024

let word =
  let module W = Word_size in
  match W.word_size with
  | W.W32 -> of_bytes_int 4
  | W.W64 -> of_bytes_int 8
;;

let kilobytes t : float = Infix.( // ) t kilobyte
let megabytes t = Infix.( // ) t megabyte
let gigabytes t = Infix.( // ) t gigabyte
let terabytes t = Infix.( // ) t terabyte
let petabytes t = Infix.( // ) t petabyte
let exabytes t = Infix.( // ) t exabyte
let words_int_exn t = Repr.to_int_exn (Repr.( / ) (to_repr t) (to_repr word))
let words_float t = Infix.( // ) t word
let of_kilobytes t : t = Infix.( * ) kilobyte t
let of_megabytes t = Infix.( * ) megabyte t
let of_gigabytes t = Infix.( * ) gigabyte t
let of_terabytes t = Infix.( * ) terabyte t
let of_petabytes t = Infix.( * ) petabyte t
let of_exabytes t = Infix.( * ) exabyte t
let of_words_int t = iscale word t
let of_words_float_exn t = Infix.( * ) word t

let[@deprecated "[since 2019-01] Use [words_int_exn] or [words_float]"] words =
  words_float
;;

let[@deprecated "[since 2019-01] Use [of_words_int] or [of_words_float_exn]"] of_words =
  of_words_float_exn
;;

let largest_measure t =
  if t >= exabyte
  then `Exabytes
  else if t >= petabyte
  then `Petabytes
  else if t >= terabyte
  then `Terabytes
  else if t >= gigabyte
  then `Gigabytes
  else if t >= megabyte
  then `Megabytes
  else if t >= kilobyte
  then `Kilobytes
  else `Bytes
;;

module Stable = struct
  module V1 = struct
    type nonrec t = t [@@deriving compare, hash]

    include Binable0.Of_binable
        (Float)
        (struct
          type nonrec t = t

          let to_binable = bytes_float
          let of_binable = of_bytes_float_exn
        end)

    include Sexpable.Of_sexpable (struct
        (* External.t - used just for custom sexp converters *)
        type t =
          [ `Bytes of float
          | `Kilobytes of float
          | `Megabytes of float
          | `Gigabytes of float
          | `Words of float ]
        [@@deriving sexp]
      end)
        (struct
          type nonrec t = t

          let to_sexpable t =
            match largest_measure t with
            | `Bytes -> `Bytes (bytes_float t)
            | `Kilobytes -> `Kilobytes (kilobytes t)
            | `Megabytes -> `Megabytes (megabytes t)
            | `Gigabytes | `Terabytes | `Petabytes | `Exabytes ->
              `Gigabytes (gigabytes t)
          ;;

          let of_sexpable = function
            | `Bytes n -> of_bytes_float_exn n
            | `Kilobytes n -> of_kilobytes n
            | `Megabytes n -> of_megabytes n
            | `Gigabytes n -> of_gigabytes n
            | `Words n -> of_words n
          ;;
        end)

    let to_string t =
      match largest_measure t with
      | `Bytes -> sprintf !"%db" (bytes_int_exn t)
      | `Kilobytes -> sprintf "%gk" (kilobytes t)
      | `Megabytes -> sprintf "%gm" (megabytes t)
      | `Gigabytes | `Terabytes | `Petabytes | `Exabytes -> sprintf "%gg" (gigabytes t)
    ;;

    (* This test documents the original to-string representation and fails under javascript
       due to differences in the rounding. *)
    let%expect_test (_[@tags "no-js"]) =
      printf !"%{}" (of_bytes_int 1000);
      [%expect {| 1000b |}];
      printf !"%{}" (of_bytes_int 1023);
      [%expect {| 1023b |}];
      printf !"%{}" (of_bytes_int 1024);
      [%expect {| 1k |}];
      printf !"%{}" (of_bytes_int 1025);
      [%expect {| 1.00098k |}];
      printf !"%{}" (of_bytes_int 1500);
      [%expect {| 1.46484k |}];
      printf !"%{}" (of_bytes_int 10000);
      [%expect {| 9.76562k |}];
      printf !"%{}" (of_bytes_int 100000);
      [%expect {| 97.6562k |}];
      printf !"%{}" (of_bytes_int 1000000);
      [%expect {| 976.562k |}];
      printf !"%{}" (of_bytes_int 10000000);
      [%expect {| 9.53674m |}]
    ;;

    let of_string s =
      let length = String.length s in
      if Int.( < ) length 2
      then invalid_argf "'%s' passed to Byte_units.of_string - too short" s ();
      let base_str = String.sub s ~pos:0 ~len:(length - 1) in
      let ext_char = Char.lowercase s.[length - 1] in
      let base =
        try Float.of_string base_str with
        | _ ->
          invalid_argf
            "'%s' passed to Byte_units.of_string - %s cannot be converted to float "
            s
            base_str
            ()
      in
      match ext_char with
      | 'b' -> of_bytes_float_exn base
      | 'k' -> of_kilobytes base
      | 'm' -> of_megabytes base
      | 'g' -> of_gigabytes base
      | 't' -> of_terabytes base
      | 'p' -> of_petabytes base
      | 'e' -> of_exabytes base
      | 'w' -> of_words base
      | ext ->
        invalid_argf
          "'%s' passed to Byte_units.of_string - illegal extension %c"
          s
          ext
          ()
    ;;

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom s ->
        (try of_string s with
         | Invalid_argument msg -> of_sexp_error msg sexp)
      | Sexp.List _ -> t_of_sexp sexp
    ;;
  end

  module V2 = struct
    type nonrec t = t [@@deriving compare, hash]

    include Binable0.Of_binable
        (Int63)
        (struct
          type nonrec t = t

          let to_binable = bytes_int63
          let of_binable = of_bytes_int63
        end)

    include Sexpable.Of_sexpable (struct
        (* External.t - used just for custom sexp converters *)
        type t = [`Bytes of Int63.Stable.V1.t] [@@deriving sexp]
      end)
        (struct
          type nonrec t = t

          let to_sexpable t = `Bytes (bytes_int63 t)
          let of_sexpable (`Bytes t) = of_bytes_int63 t
        end)
  end
end

let to_string_hum = Stable.V1.to_string
let to_string = Stable.V1.to_string
let of_string = Stable.V1.of_string

let to_string_short t =
  let f, ext =
    match largest_measure t with
    | `Bytes -> bytes_float t, 'b'
    | `Kilobytes -> kilobytes t, 'k'
    | `Megabytes -> megabytes t, 'm'
    | `Gigabytes -> gigabytes t, 'g'
    | `Terabytes -> terabytes t, 't'
    | `Petabytes -> petabytes t, 'p'
    | `Exabytes -> exabytes t, 'e'
  in
  if f >=. 100.
  then sprintf "%.0f%c" f ext
  else if f >=. 10.
  then sprintf "%.1f%c" f ext
  else sprintf "%.2f%c" f ext
;;

let%expect_test _ =
  printf !"%{#short}" (of_bytes_int 1000);
  [%expect {| 1000b |}];
  printf !"%{#short}" (of_bytes_int 1023);
  [%expect {| 1023b |}];
  printf !"%{#short}" (of_bytes_int 1024);
  [%expect {| 1.00k |}];
  printf !"%{#short}" (of_bytes_int 1025);
  [%expect {| 1.00k |}];
  printf !"%{#short}" (of_bytes_int 10000);
  [%expect {| 9.77k |}];
  printf !"%{#short}" (of_bytes_int 100000);
  [%expect {| 97.7k |}];
  printf !"%{#short}" (of_bytes_int 1000000);
  [%expect {| 977k |}];
  printf !"%{#short}" (of_bytes_int 10000000);
  [%expect {| 9.54m |}];
  printf !"%{#short}" (of_bytes 10000000000.);
  [%expect {| 9.31g |}];
  printf !"%{#short}" (of_bytes 1000000000000.);
  [%expect {| 931g |}];
  printf !"%{#short}" (of_bytes 100000000000000.);
  [%expect {| 90.9t |}];
  printf !"%{#short}" (of_bytes 100000000000000000.);
  [%expect {| 88.8p |}];
  printf !"%{#short}" (of_bytes 3000000000000000000.);
  [%expect {| 2.60e |}];
  ()
;;

let[@deprecated
  "[since 2019-01] Use [of_bytes], [of_kilobytes], [of_megabytes], etc as appropriate."]
     create
     units
     value
  =
  match units with
  | `Bytes -> of_bytes_float_exn value
  | `Kilobytes -> of_kilobytes value
  | `Megabytes -> of_megabytes value
  | `Gigabytes -> of_gigabytes value
  | `Words -> of_words_float_exn value
;;
