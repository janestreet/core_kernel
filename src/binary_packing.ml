open Std_internal

module Char = Caml.Char
module Int32 = Caml.Int32
module Int64 = Caml.Int64

#import "config.mlh"

#if JSC_ARCH_SIXTYFOUR
let   signed_max = Int32.to_int Int32.max_int
let unsigned_max = Int64.to_int 0xffff_ffffL
#endif

type endian = [ `Big_endian | `Little_endian ] [@@deriving compare, sexp]

(* Computes the offset based on the total number of bytes, the byte order, and the
   byte number. The byte number is ordered by decreasing significance starting at zero
   (big endian). So the most significant byte is 0, and the least significant byte is (len
   - 1). *)

exception Binary_packing_invalid_byte_number of int * int [@@deriving sexp]
let offset ~len ~byte_order byte_nr =
  if byte_nr >= len || byte_nr < 0 then
    raise (Binary_packing_invalid_byte_number (byte_nr, len));
  match byte_order with
  | `Little_endian -> len - 1 - byte_nr
  | `Big_endian -> byte_nr
;;

(* byte order added to the _8 functions to make testing easier (uniformity) *)
exception Pack_unsigned_8_argument_out_of_range of int [@@deriving sexp]
let pack_unsigned_8 ~buf ~pos n =
  if n > 0xFF || n < 0 then
    raise (Pack_unsigned_8_argument_out_of_range n)
  else buf.[pos] <- Char.unsafe_chr n;
;;

let unpack_unsigned_8 ~buf ~pos = Char.code buf.[pos]

exception Pack_signed_8_argument_out_of_range of int [@@deriving sexp]
let pack_signed_8 ~buf ~pos n =
  if n > 0x7F || n < -0x80 then
    raise (Pack_signed_8_argument_out_of_range n)
  else buf.[pos] <- Char.unsafe_chr n
;;

let unpack_signed_8 ~buf ~pos =
  let n = unpack_unsigned_8 ~buf ~pos in
  if n >= 0x80 then
    -(0x100 - n)
  else
    n
;;

exception Pack_unsigned_16_argument_out_of_range of int [@@deriving sexp]
let pack_unsigned_16 ~byte_order ~buf ~pos n =
  if n >= 0x10000 || n < 0 then
    raise (Pack_unsigned_16_argument_out_of_range n)
  else begin
    buf.[pos + offset ~len:2 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos + offset ~len:2 ~byte_order 1] <- Char.unsafe_chr (0xFF land n)
  end
;;

let pack_unsigned_16_big_endian ~buf ~pos n =
  if n >= 0x10000 || n < 0 then
    raise (Pack_unsigned_16_argument_out_of_range n)
  else begin
    buf.[pos    ] <- Char.unsafe_chr (0xFF land (n lsr 8));
    buf.[pos + 1] <- Char.unsafe_chr (0xFF land n)
  end
;;

let pack_unsigned_16_little_endian ~buf ~pos n =
  if n >= 0x10000 || n < 0 then
    raise (Pack_unsigned_16_argument_out_of_range n)
  else begin
    buf.[pos + 1] <- Char.unsafe_chr (0xFF land (n lsr 8));
    buf.[pos    ] <- Char.unsafe_chr (0xFF land n)
  end
;;

exception Pack_signed_16_argument_out_of_range of int [@@deriving sexp]
let pack_signed_16 ~byte_order ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000 then
    raise (Pack_signed_16_argument_out_of_range n)
  else begin
    buf.[pos + offset ~len:2 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos + offset ~len:2 ~byte_order 1] <- Char.unsafe_chr (0xFF land n)
  end
;;

let pack_signed_16_big_endian ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000 then
    raise (Pack_signed_16_argument_out_of_range n)
  else begin
    buf.[pos    ] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos + 1] <- Char.unsafe_chr (0xFF land n)
  end
;;

let pack_signed_16_little_endian ~buf ~pos n =
  if n > 0x7FFF || n < -0x8000 then
    raise (Pack_signed_16_argument_out_of_range n)
  else begin
    buf.[pos + 1] <- Char.unsafe_chr (0xFF land (n asr 8));
    buf.[pos    ] <- Char.unsafe_chr (0xFF land n)
  end
;;

let unpack_unsigned_16 ~byte_order ~buf ~pos =
  let b1 = Char.code buf.[pos + offset ~len:2 ~byte_order 0] lsl 8 in
  let b2 = Char.code buf.[pos + offset ~len:2 ~byte_order 1] in
  b1 lor b2
;;

let unpack_signed_16 ~byte_order ~buf ~pos =
  let n = unpack_unsigned_16 ~byte_order ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n)
  else n
;;

let unpack_unsigned_16_big_endian ~buf ~pos =
  let b1 = Char.code buf.[pos    ] lsl 8 in
  let b2 = Char.code buf.[pos + 1] in
  b1 lor b2
;;

let unpack_unsigned_16_little_endian ~buf ~pos =
  let b1 = Char.code buf.[pos + 1] lsl 8 in
  let b2 = Char.code buf.[pos    ] in
  b1 lor b2
;;

let unpack_signed_16_big_endian ~buf ~pos =
  let n = unpack_unsigned_16_big_endian ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n) else n
;;

let unpack_signed_16_little_endian ~buf ~pos =
  let n = unpack_unsigned_16_little_endian ~buf ~pos in
  if n >= 0x8000 then -(0x10000 - n) else n
;;

module Make_inline_tests (A: sig
  val num_bytes: int
  val signed: bool
  type t
  val ns: t list
  val of_int64: int64 -> t
  val to_int64: t -> int64
  val pack: byte_order:endian -> buf:string -> pos:int -> t -> unit
  val unpack: byte_order:endian -> buf:string -> pos:int -> t
  val pack_big_endian: buf:string -> pos:int -> t -> unit
  val unpack_big_endian: buf:string -> pos:int -> t
  val pack_little_endian: buf:string -> pos:int -> t -> unit
  val unpack_little_endian: buf:string -> pos:int -> t
end) = struct
  include A
  let pos = 3
  let buf_size = 13
  let ns_rev =
    List.map ns ~f:(fun t ->
      let t = to_int64 t in
      of_int64 (
        List.fold ~init:0L
          (List.init num_bytes ~f:Fn.id)
          ~f:(fun acc k ->
            Int64.logor acc (
              let w =
                Int64.shift_left
                  (Int64.logand 0xFFL (Int64.shift_right_logical t (k * 8)))
                  ((num_bytes - 1 - k) * 8)
              in
              if signed && num_bytes < 8 then
                let max_val = Int64.shift_left 1L (num_bytes * 8 - 1) in
                if w >= max_val then
                  Int64.sub w (Int64.shift_left max_val 1)
                else
                  w
              else
                w))))
  let padding = '.'
  let test_rest_of_buf buf =
    for k = 0 to buf_size - 1 do
      if k < pos || k > pos + num_bytes then
        assert (String.get buf k = padding)
    done
  ;;
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack ~byte_order:`Little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Little_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack ~byte_order:`Big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Big_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack_little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_little_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack_big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_big_endian ~buf ~pos)
  let%test _ =
    ns_rev = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack_big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_little_endian ~buf ~pos)
  let%test _ =
    ns_rev = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack_little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_big_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack ~byte_order:`Big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_big_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack ~byte_order:`Little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_little_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack_big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Big_endian ~buf ~pos)
  let%test _ =
    ns = List.map ns ~f:(fun n ->
      let buf = String.make buf_size padding in
      pack_little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Little_endian ~buf ~pos)
end

let%test_module "inline_unsigned_16" = (module Make_inline_tests (struct
  let ns = [0x3f20; 0x7f20; 0xef20; 0; 0x7fff; 0x8000; 0xffff]
  let num_bytes = 2
  let signed = false
  type t = int
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let pack = pack_unsigned_16
  let unpack = unpack_unsigned_16
  let pack_big_endian = pack_unsigned_16_big_endian
  let unpack_big_endian = unpack_unsigned_16_big_endian
  let pack_little_endian = pack_unsigned_16_little_endian
  let unpack_little_endian = unpack_unsigned_16_little_endian
end))

let%test_module "inline_signed_16" = (module Make_inline_tests (struct
  let ns = [0x3f20; 0x7f20; -0x7f20; -0x8000; 0; 1; 0x7fff]
  let num_bytes = 2
  let signed = true
  type t = int
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let pack = pack_signed_16
  let unpack = unpack_signed_16
  let pack_big_endian = pack_signed_16_big_endian
  let unpack_big_endian = unpack_signed_16_big_endian
  let pack_little_endian = pack_signed_16_little_endian
  let unpack_little_endian = unpack_signed_16_little_endian
end))

exception Pack_unsigned_32_argument_out_of_range of int [@@deriving sexp]
let check_unsigned_32_in_range n =
#if JSC_ARCH_SIXTYFOUR
    if n > unsigned_max || n < 0 then
      raise (Pack_unsigned_32_argument_out_of_range n)
#else
    if n < 0 then
      raise (Pack_unsigned_32_argument_out_of_range n)
#endif

let pack_unsigned_32_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  check_unsigned_32_in_range n;
  buf.[pos + offset ~len:4 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 24)); (* MSB *)
  buf.[pos + offset ~len:4 ~byte_order 1] <- Char.unsafe_chr (0xFF land (n asr 16));
  buf.[pos + offset ~len:4 ~byte_order 2] <- Char.unsafe_chr (0xFF land (n asr 8));
  buf.[pos + offset ~len:4 ~byte_order 3] <- Char.unsafe_chr (0xFF land n) (* LSB *)
;;

let pack_unsigned_32_int_big_endian ~buf ~pos n =
  check_unsigned_32_in_range n;
  buf.[pos] <- Char.unsafe_chr (0xFF land (n lsr 24)); (* MSB *)
  buf.[pos + 3] <- Char.unsafe_chr (0xFF land n); (* LSB *)
  Caml.Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n lsr 16)));
  Caml.Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n lsr 8)));
;;

let pack_unsigned_32_int_little_endian ~buf ~pos n =
  check_unsigned_32_in_range n;
  buf.[pos + 3] <- Char.unsafe_chr (0xFF land (n lsr 24)); (* MSB *)
  buf.[pos] <- Char.unsafe_chr (0xFF land n); (* LSB *)
  Caml.Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n lsr 16)));
  Caml.Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n lsr 8)));
;;

exception Pack_signed_32_argument_out_of_range of int [@@deriving sexp]
let check_signed_32_in_range n =
#if JSC_ARCH_SIXTYFOUR
    if n > signed_max || n < -(signed_max + 1) then
      raise (Pack_signed_32_argument_out_of_range n)
#else
    if false then
      raise (Pack_signed_32_argument_out_of_range n)
#endif

let pack_signed_32_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  check_signed_32_in_range n;
  buf.[pos + offset ~len:4 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 24)); (* MSB *)
  buf.[pos + offset ~len:4 ~byte_order 1] <- Char.unsafe_chr (0xFF land (n asr 16));
  buf.[pos + offset ~len:4 ~byte_order 2] <- Char.unsafe_chr (0xFF land (n asr 8));
  buf.[pos + offset ~len:4 ~byte_order 3] <- Char.unsafe_chr (0xFF land n) (* LSB *)
;;

let pack_signed_32_int_big_endian ~buf ~pos n =
  check_signed_32_in_range n;
  buf.[pos] <- Char.unsafe_chr (0xFF land (n asr 24)); (* MSB *)
  buf.[pos + 3] <- Char.unsafe_chr (0xFF land n); (* LSB *)
  Caml.Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n asr 16)));
  Caml.Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n asr 8)));
;;

let pack_signed_32_int_little_endian ~buf ~pos n =
  check_signed_32_in_range n;
  buf.[pos + 3] <- Char.unsafe_chr (0xFF land (n asr 24)); (* MSB *)
  buf.[pos] <- Char.unsafe_chr (0xFF land n); (* LSB *)
  Caml.Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (n asr 16)));
  Caml.Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (n asr 8)));
;;

let pack_signed_32 ~byte_order ~buf ~pos n =
  buf.[pos + offset ~len:4 ~byte_order 0] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 24));
  buf.[pos + offset ~len:4 ~byte_order 1] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 16));
  buf.[pos + offset ~len:4 ~byte_order 2] <-
    Char.unsafe_chr (0xFF land Int32.to_int (Int32.shift_right n 8));
  buf.[pos + offset ~len:4 ~byte_order 3] <- Char.unsafe_chr (0xFF land Int32.to_int n)
;;

let unpack_signed_32 ~byte_order ~buf ~pos =
  let b1 = (* MSB *)
    Int32.shift_left (Int32.of_int (Char.code buf.[pos + offset ~len:4 ~byte_order 0])) 24
  in
  let b2 = Char.code buf.[pos + offset ~len:4 ~byte_order 1] lsl 16 in
  let b3 = Char.code buf.[pos + offset ~len:4 ~byte_order 2] lsl 8 in
  let b4 = Char.code buf.[pos + offset ~len:4 ~byte_order 3] in (* LSB *)
  Int32.logor b1 (Int32.of_int (b2 lor b3 lor b4))
;;

let unpack_unsigned_32_int ~byte_order ~buf ~pos =
  assert (Sys.word_size = 64);
  let b1 = Char.code buf.[pos + offset ~len:4 ~byte_order 0] lsl 24 in (* msb *)
  let b2 = Char.code buf.[pos + offset ~len:4 ~byte_order 1] lsl 16 in
  let b3 = Char.code buf.[pos + offset ~len:4 ~byte_order 2] lsl 8 in
  let b4 = Char.code buf.[pos + offset ~len:4 ~byte_order 3] in (* lsb *)
  b1 lor b2 lor b3 lor b4
;;

let unpack_unsigned_32_int_big_endian ~buf ~pos =
  let b1 = Char.code buf.[pos] lsl 24 in (* msb *)
  let b4 = Char.code buf.[pos + 3] in (* lsb *)
  let b2 = Char.code (Caml.Bytes.unsafe_get buf (pos + 1)) lsl 16 in
  let b3 = Char.code (Caml.Bytes.unsafe_get buf (pos + 2)) lsl 8 in
  b1 lor b2 lor b3 lor b4
;;

let unpack_unsigned_32_int_little_endian ~buf ~pos =
  let b1 = Char.code buf.[pos + 3] lsl 24 in (* msb *)
  let b4 = Char.code buf.[pos] in (* lsb *)
  let b2 = Char.code (Caml.Bytes.unsafe_get buf (pos + 2)) lsl 16 in
  let b3 = Char.code (Caml.Bytes.unsafe_get buf (pos + 1)) lsl 8 in
  b1 lor b2 lor b3 lor b4
;;

#if JSC_ARCH_SIXTYFOUR

let unpack_signed_32_int ~byte_order ~buf ~pos =
  let n = unpack_unsigned_32_int ~byte_order ~buf ~pos in
  if n > signed_max then -(((signed_max + 1) lsl 1) - n)
  else n
;;

let unpack_signed_32_int_big_endian ~buf ~pos =
  let n = unpack_unsigned_32_int_big_endian ~buf ~pos in
  if n > signed_max then n - (unsigned_max + 1) else n
;;

let unpack_signed_32_int_little_endian ~buf ~pos =
  let n = unpack_unsigned_32_int_little_endian ~buf ~pos in
  if n > signed_max then n - (unsigned_max + 1) else n
;;

let%test_module "inline_unsigned_32_int" = (module Make_inline_tests (struct
  let ns = [0x3f20_3040; 0x7f20_3040;
            signed_max; signed_max + 1; unsigned_max; 0]
  let num_bytes = 4
  let signed = false
  type t = int
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let pack = pack_unsigned_32_int
  let unpack = unpack_unsigned_32_int
  let pack_big_endian = pack_unsigned_32_int_big_endian
  let unpack_big_endian = unpack_unsigned_32_int_big_endian
  let pack_little_endian = pack_unsigned_32_int_little_endian
  let unpack_little_endian = unpack_unsigned_32_int_little_endian
end))

let%test_module "inline_signed_32_int" = (module Make_inline_tests (struct
  let ns = [0x3f20_3040; 0x7f20_3040; -0x7f20_3040;
            signed_max; -(signed_max + 1); 0]
  let num_bytes = 4
  let signed = true
  type t = int
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let pack = pack_signed_32_int
  let unpack = unpack_signed_32_int
  let pack_big_endian = pack_signed_32_int_big_endian
  let unpack_big_endian = unpack_signed_32_int_big_endian
  let pack_little_endian = pack_signed_32_int_little_endian
  let unpack_little_endian = unpack_signed_32_int_little_endian
end))

#else

let unpack_signed_32_int               = unpack_unsigned_32_int
let unpack_signed_32_int_big_endian    = unpack_unsigned_32_int_big_endian
let unpack_signed_32_int_little_endian = unpack_unsigned_32_int_little_endian

#endif
 (* ARCH_SIXTYFOUR *)

let pack_signed_64 ~byte_order ~buf ~pos v =
  let top3 = Int64.to_int (Int64.shift_right v 40) in
  let mid3 = Int64.to_int (Int64.shift_right v 16) in
  let bot2 = Int64.to_int v in
  buf.[pos + offset ~len:8 ~byte_order 0] <- Char.unsafe_chr (0xFF land (top3 lsr 16));
  buf.[pos + offset ~len:8 ~byte_order 1] <- Char.unsafe_chr (0xFF land (top3 lsr 8));
  buf.[pos + offset ~len:8 ~byte_order 2] <- Char.unsafe_chr (0xFF land top3);
  buf.[pos + offset ~len:8 ~byte_order 3] <- Char.unsafe_chr (0xFF land (mid3 lsr 16));
  buf.[pos + offset ~len:8 ~byte_order 4] <- Char.unsafe_chr (0xFF land (mid3 lsr 8));
  buf.[pos + offset ~len:8 ~byte_order 5] <- Char.unsafe_chr (0xFF land mid3);
  buf.[pos + offset ~len:8 ~byte_order 6] <- Char.unsafe_chr (0xFF land (bot2 lsr 8));
  buf.[pos + offset ~len:8 ~byte_order 7] <- Char.unsafe_chr (0xFF land bot2)
;;

let pack_signed_64_big_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  buf.[pos] <-
    Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 56)));
  buf.[pos + 7] <- Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL v));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Caml.Bytes.unsafe_set buf (pos + 1)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 48))));
  Caml.Bytes.unsafe_set buf (pos + 2)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 40))));
  Caml.Bytes.unsafe_set buf (pos + 3)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 32))));
  Caml.Bytes.unsafe_set buf (pos + 4)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 24))));
  Caml.Bytes.unsafe_set buf (pos + 5)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 16))));
  Caml.Bytes.unsafe_set buf (pos + 6)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 8))))
;;

let pack_signed_64_little_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  buf.[pos] <- Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL v));
  buf.[pos + 7] <-
    Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 56)));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Caml.Bytes.unsafe_set buf (pos + 1)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 8))));
  Caml.Bytes.unsafe_set buf (pos + 2)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 16))));
  Caml.Bytes.unsafe_set buf (pos + 3)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 24))));
  Caml.Bytes.unsafe_set buf (pos + 4)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 32))));
  Caml.Bytes.unsafe_set buf (pos + 5)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 40))));
  Caml.Bytes.unsafe_set buf (pos + 6)
    (Char.unsafe_chr (Int64.to_int (Int64.logand 0xFFL (Int64.shift_right_logical v 48))))
;;

let unpack_signed_64 ~byte_order ~buf ~pos =
  Int64.logor
    (Int64.logor
       (Int64.shift_left
          (Int64.of_int (Char.code buf.[pos + offset ~len:8 ~byte_order 0] lsl 16
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 1] lsl 8
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 2]))
          40)
       (Int64.shift_left
          (Int64.of_int (Char.code buf.[pos + offset ~len:8 ~byte_order 3] lsl 16
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 4] lsl 8
                         lor Char.code buf.[pos + offset ~len:8 ~byte_order 5]))
          16))
    (Int64.of_int (Char.code buf.[pos + offset ~len:8 ~byte_order 6] lsl 8
                   lor Char.code buf.[pos + offset ~len:8 ~byte_order 7]))
;;

let unpack_signed_64_big_endian ~buf ~pos =
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code buf.[pos]
  and b8 = Char.code buf.[pos + 7] in

  let b2 = Char.code (Caml.Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Caml.Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Caml.Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Caml.Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Caml.Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Caml.Bytes.unsafe_get buf (pos + 6)) in

#if JSC_ARCH_SIXTYFOUR

  let i1 = Int64.of_int (                                b1)
  and i2 = Int64.of_int ((b2 lsl 48) lor (b3 lsl 40) lor
                         (b4 lsl 32) lor (b5 lsl 24) lor
                         (b6 lsl 16) lor (b7 lsl  8) lor b8) in
  Int64.(logor i2 (shift_left i1 56))

#else

  let i1 = Int64.of_int (                (b1 lsl 8) lor b2)
  and i2 = Int64.of_int ((b3 lsl 16) lor (b4 lsl 8) lor b5)
  and i3 = Int64.of_int ((b6 lsl 16) lor (b7 lsl 8) lor b8) in
  Int64.(logor i3 (logor (shift_left i2 24) (shift_left i1 48)))

#endif
;;

let unpack_signed_64_little_endian ~buf ~pos =
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code buf.[pos]
  and b8 = Char.code buf.[pos + 7] in

  let b2 = Char.code (Caml.Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Caml.Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Caml.Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Caml.Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Caml.Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Caml.Bytes.unsafe_get buf (pos + 6)) in

#if JSC_ARCH_SIXTYFOUR

  let i1 = Int64.of_int ( b1         lor (b2 lsl  8) lor
                         (b3 lsl 16) lor (b4 lsl 24) lor
                         (b5 lsl 32) lor (b6 lsl 40) lor (b7 lsl 48))
  and i2 = Int64.of_int   b8         in
  Int64.(logor i1 (shift_left i2 56))

#else

  let i1 = Int64.of_int (b1 lor (b2 lsl 8) lor (b3 lsl 16))
  and i2 = Int64.of_int (b4 lor (b5 lsl 8) lor (b6 lsl 16))
  and i3 = Int64.of_int (b7 lor (b8 lsl 8)) in
  Int64.(logor i1 (logor (shift_left i2 24) (shift_left i3 48)))

#endif
;;

let pack_signed_64_int ~byte_order ~buf ~pos n =
  assert (Sys.word_size = 64);
  buf.[pos + offset ~len:8 ~byte_order 0] <- Char.unsafe_chr (0xFF land (n asr 56));
  buf.[pos + offset ~len:8 ~byte_order 1] <- Char.unsafe_chr (0xFF land (n asr 48));
  buf.[pos + offset ~len:8 ~byte_order 2] <- Char.unsafe_chr (0xFF land (n asr 40));
  buf.[pos + offset ~len:8 ~byte_order 3] <- Char.unsafe_chr (0xFF land (n asr 32));
  buf.[pos + offset ~len:8 ~byte_order 4] <- Char.unsafe_chr (0xFF land (n asr 24));
  buf.[pos + offset ~len:8 ~byte_order 5] <- Char.unsafe_chr (0xFF land (n asr 16));
  buf.[pos + offset ~len:8 ~byte_order 6] <- Char.unsafe_chr (0xFF land (n asr 8));
  buf.[pos + offset ~len:8 ~byte_order 7] <- Char.unsafe_chr (0xFF land n)
;;

(* It's important to use [asr] not [lsr] in [pack_signed_64_int_big_endian] and
   [pack_signed_64_int_little_endian] so that the most significant byte is encoded
   correctly.  (It might be helpful to think about this as widening, i.e. sign
   extending, the number to 64 bits and then doing the right shift by 56.)
*)

let pack_signed_64_int_big_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  buf.[pos] <- Char.unsafe_chr (0xFF land (v asr 56));
  buf.[pos + 7] <- Char.unsafe_chr (0xFF land v);
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Caml.Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (v asr 48)));
  Caml.Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (v asr 40)));
  Caml.Bytes.unsafe_set buf (pos + 3) (Char.unsafe_chr (0xFF land (v asr 32)));
  Caml.Bytes.unsafe_set buf (pos + 4) (Char.unsafe_chr (0xFF land (v asr 24)));
  Caml.Bytes.unsafe_set buf (pos + 5) (Char.unsafe_chr (0xFF land (v asr 16)));
  Caml.Bytes.unsafe_set buf (pos + 6) (Char.unsafe_chr (0xFF land (v asr 8)))
;;

let pack_signed_64_int_little_endian ~buf ~pos v =
  (* Safely set the first and last bytes, so that we verify the string bounds. *)
  buf.[pos] <- Char.unsafe_chr (0xFF land v);
  buf.[pos + 7] <- Char.unsafe_chr (0xFF land (v asr 56));
  (* Now we can use [unsafe_set] for the intermediate bytes. *)
  Caml.Bytes.unsafe_set buf (pos + 1) (Char.unsafe_chr (0xFF land (v asr 8)));
  Caml.Bytes.unsafe_set buf (pos + 2) (Char.unsafe_chr (0xFF land (v asr 16)));
  Caml.Bytes.unsafe_set buf (pos + 3) (Char.unsafe_chr (0xFF land (v asr 24)));
  Caml.Bytes.unsafe_set buf (pos + 4) (Char.unsafe_chr (0xFF land (v asr 32)));
  Caml.Bytes.unsafe_set buf (pos + 5) (Char.unsafe_chr (0xFF land (v asr 40)));
  Caml.Bytes.unsafe_set buf (pos + 6) (Char.unsafe_chr (0xFF land (v asr 48)))
;;

let unpack_signed_64_int ~byte_order ~buf ~pos =
  assert (Sys.word_size = 64);
  (Char.code buf.[pos + offset ~len:8 ~byte_order 0] lsl 56)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 1] lsl 48)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 2] lsl 40)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 3] lsl 32)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 4] lsl 24)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 5] lsl 16)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 6] lsl 8)
  lor (Char.code buf.[pos + offset ~len:8 ~byte_order 7])
;;

exception Unpack_signed_64_int_most_significant_byte_too_large of int [@@deriving sexp]
let check_highest_order_byte_range byte =
  if byte < 64 || byte >= 192 then ()
  else raise (Unpack_signed_64_int_most_significant_byte_too_large byte)
;;

let unpack_signed_64_int_big_endian ~buf ~pos =
  assert (Sys.word_size = 64);
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code buf.[pos]
  and b8 = Char.code buf.[pos + 7] in

  let b2 = Char.code (Caml.Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Caml.Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Caml.Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Caml.Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Caml.Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Caml.Bytes.unsafe_get buf (pos + 6)) in

  check_highest_order_byte_range b1;

  (b1 lsl 56) lor
  (b2 lsl 48) lor (b3 lsl 40) lor
  (b4 lsl 32) lor (b5 lsl 24) lor
  (b6 lsl 16) lor (b7 lsl  8) lor b8
;;

let unpack_signed_64_int_little_endian ~buf ~pos =
  assert (Sys.word_size = 64);
  (* Do bounds checking only on the first and last bytes *)
  let b1 = Char.code buf.[pos]
  and b8 = Char.code buf.[pos + 7] in

  let b2 = Char.code (Caml.Bytes.unsafe_get buf (pos + 1))
  and b3 = Char.code (Caml.Bytes.unsafe_get buf (pos + 2))
  and b4 = Char.code (Caml.Bytes.unsafe_get buf (pos + 3))
  and b5 = Char.code (Caml.Bytes.unsafe_get buf (pos + 4))
  and b6 = Char.code (Caml.Bytes.unsafe_get buf (pos + 5))
  and b7 = Char.code (Caml.Bytes.unsafe_get buf (pos + 6)) in

  check_highest_order_byte_range b8;

  b1          lor (b2 lsl 8) lor
  (b3 lsl 16) lor (b4 lsl 24) lor
  (b5 lsl 32) lor (b6 lsl 40) lor
  (b7 lsl 48) lor (b8 lsl 56)
;;

#if JSC_ARCH_SIXTYFOUR
let%test_unit "63 bits overflow" =
  let buf = String.create 8 in
  let pos = 0 in
  List.iter
    [pack_signed_64_little_endian, unpack_signed_64_int_little_endian;
     pack_signed_64_big_endian, unpack_signed_64_int_big_endian]
    ~f:(fun (pack, unpack) ->
      List.iter [
        Int64.max_int, Some 127;
        Int64.min_int, Some 128;
        Int64.(add (of_int Int.max_value) 1L), Some 64;
        Int64.(add (of_int Int.min_value) (-1L)), Some 191;
        Int64.(of_int Int.max_value), None;
        Int64.(of_int Int.min_value), None;
      ] ~f:(fun (n, opt) ->
          pack ~buf ~pos n;
          try ignore (unpack ~buf ~pos : int); assert (opt = None)
          with Unpack_signed_64_int_most_significant_byte_too_large n when Some n = opt -> ()
       ))
;;
#endif

let%test_module "inline_signed_64" = (module Make_inline_tests (struct
  let ns = [0x3f20_3040_5060_7080L;
            0x7f20_3040_5060_7080L;
           -0x7f20_3040_5060_7080L;
            0x7fff_ffff_ffff_ffffL;
            0x8000_0000_0000_0000L;
            0L]
  let num_bytes = 8
  let signed = true
  type t = int64
  let of_int64 = Fn.id
  let to_int64 = Fn.id
  let pack = pack_signed_64
  let unpack = unpack_signed_64
  let pack_big_endian = pack_signed_64_big_endian
  let unpack_big_endian = unpack_signed_64_big_endian
  let pack_little_endian = pack_signed_64_little_endian
  let unpack_little_endian = unpack_signed_64_little_endian
end))

#if JSC_ARCH_SIXTYFOUR

let%test_module "inline_signed_64_int" =
  (module Make_inline_tests (struct
  (* These numbers are written with one endianness and read with the opposite endianness,
     so the smallest byte becomes the biggest byte. Because of this, the range restriction
     that applies to the biggest byte also applies to the smallest byte. *)
  let ns = [0x3f20_3040_5060_0708L;
            0x7f20_3040_5060_0708L;
           -0x7f20_3040_5060_0708L;
            0x7fff_ffff_ffff_0000L;
            0L] |> List.map ~f:Int64.to_int
  let num_bytes = 8
  let signed = true
  type t = int
  let of_int64 = Int64.to_int
  let to_int64 = Int64.of_int
  let pack = pack_signed_64_int
  let unpack = unpack_signed_64_int
  let pack_big_endian = pack_signed_64_int_big_endian
  let unpack_big_endian = unpack_signed_64_int_big_endian
  let pack_little_endian = pack_signed_64_int_little_endian
  let unpack_little_endian = unpack_signed_64_int_little_endian
end))

#endif

let pack_float ~byte_order ~buf ~pos f =
  pack_signed_64 ~byte_order ~buf ~pos (Int64.bits_of_float f)
;;

let unpack_float ~byte_order ~buf ~pos =
  Int64.float_of_bits (unpack_signed_64 ~byte_order ~buf ~pos)
;;

let rec last_nonmatch_plus_one ~buf ~min_pos ~pos ~char =
  let pos' = pos - 1 in
  if pos' >= min_pos && Core_char.(=) (String.get buf pos') char then
    last_nonmatch_plus_one ~buf ~min_pos ~pos:pos' ~char
  else
    pos
;;

let unpack_tail_padded_fixed_string ?(padding='\x00') ~buf ~pos ~len () =
  let data_end =
    last_nonmatch_plus_one ~buf ~min_pos:pos ~pos:(pos + len) ~char:padding
  in
  String.sub buf ~pos ~len:(data_end - pos)
;;

exception Pack_tail_padded_fixed_string_argument_too_long of
    [`s of string] * [`longer_than] * [`len of int] [@@deriving sexp]
;;

let pack_tail_padded_fixed_string ?(padding='\x00') ~buf ~pos ~len s =
  let slen = String.length s in
  if slen > len then
    raise (Pack_tail_padded_fixed_string_argument_too_long (`s s, `longer_than, `len len))
  else begin
    String.blit ~src:s ~dst:buf ~src_pos:0 ~dst_pos:pos ~len:slen;
    if slen < len then begin
      let diff = len - slen in
      String.fill buf ~pos:(pos + slen) ~len:diff padding
    end
  end
;;

let%test_module "inline_tail_padded_fixed_string" = (module struct
  let%test _ = last_nonmatch_plus_one ~buf:"222121212" ~min_pos:3 ~pos:9 ~char:'2' = 8
  let%test _ = last_nonmatch_plus_one ~buf:"111121212" ~min_pos:3 ~pos:9 ~char:'1' = 9
  let%test _ = last_nonmatch_plus_one ~buf:"222121222" ~min_pos:3 ~pos:9 ~char:'2' = 6
  let%test _ = last_nonmatch_plus_one ~buf:"222222222" ~min_pos:3 ~pos:9 ~char:'2' = 3
  let%test _ = last_nonmatch_plus_one ~buf:"221222222" ~min_pos:3 ~pos:9 ~char:'2' = 3
  let%test _ = last_nonmatch_plus_one ~buf:"222122222" ~min_pos:3 ~pos:9 ~char:'2' = 4
  let%test _ = last_nonmatch_plus_one ~buf:"222122222" ~min_pos:3 ~pos:9 ~char:'1' = 9
  let%test _ = last_nonmatch_plus_one ~buf:"222122222" ~min_pos:3 ~pos:8 ~char:'1' = 8
  let%test _ = last_nonmatch_plus_one ~buf:"222122221" ~min_pos:3 ~pos:8 ~char:'1' = 8
  let%test _ = last_nonmatch_plus_one ~buf:"222122221" ~min_pos:3 ~pos:8 ~char:'2' = 4
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c." ~pos:1 ~len:5 () = "b..c"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c." ~pos:1 ~len:4 () = "b..c"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c." ~pos:1 ~len:3 () = "b"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c." ~pos:1 ~len:2 () = "b"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c." ~pos:1 ~len:1 () = "b"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c" ~pos:2 ~len:3 () = "..c"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c" ~pos:2 ~len:2 () = ""
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..cd" ~pos:2 ~len:3 () = "..c"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..cd" ~pos:2 ~len:2 () = ""
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:"ab..c." ~pos:2 ~len:1 () = ""
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:".....x" ~pos:0 ~len:6 () = ".....x"
  let%test _ = unpack_tail_padded_fixed_string ~padding:'.' ~buf:".....x" ~pos:0 ~len:5 () = ""
  let%test _ =
    "1abcd.78" = (
      let buf = "12345678" in
      pack_tail_padded_fixed_string ~padding:'.' ~buf ~pos:1 ~len:5 "abcd";
      buf)
  let%test _ =
    "1abcde78" = (
      let buf = "12345678" in
      pack_tail_padded_fixed_string ~padding:'.' ~buf ~pos:1 ~len:5 "abcde";
      buf)
  let%test _ =
    "1.....78" = (
      let buf = "12345678" in
      pack_tail_padded_fixed_string ~padding:'.' ~buf ~pos:1 ~len:5 "";
      buf)
  let%test _ =
    "1.....78" = (
      let buf = "12345678" in
      pack_tail_padded_fixed_string ~padding:'.' ~buf ~pos:1 ~len:5 "...";
      buf)
end)
;;

let test byte_order =
  let buf = String.make 8 'a' in
  let test name to_string p u ns =
    List.iter ns ~f:(fun n ->
      p ~byte_order ~buf ~pos:0 n;
      let n' = u ~byte_order ~buf ~pos:0 in
      if n <> n' then
        failwith (sprintf "%s = unpack_%s (pack_%s %s)"
                    (to_string n') name name (to_string n)))
  in
  test "signed_8" string_of_int
    (fun ~byte_order:_ ~buf ~pos i -> pack_signed_8 ~buf ~pos i)
    (fun ~byte_order:_ ~buf ~pos -> unpack_signed_8 ~buf ~pos)
    [-0x80; -0x7F; -0xF; -1; 0; 1; 0xF; 0x7F];
  test "signed_16" string_of_int
    pack_signed_16 unpack_signed_16
    [-0x8000; -0x7ABC; -0xFF; -1; 0; 1; 0xFF; 0x7ABC; 0x7FFF];
  test "signed_32" Int32.to_string
    pack_signed_32 unpack_signed_32
    [-0x80000000l; -0x76543210l; -0xFFl; Int32.minus_one; Int32.zero; Int32.one; 0x76543210l; 0x7FFFFFFFl];
  test "signed_64" Int64.to_string
    pack_signed_64 unpack_signed_64
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]
;;

let test () = test `Big_endian; test `Little_endian
