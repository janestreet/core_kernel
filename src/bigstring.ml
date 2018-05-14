open! Import
open Std_internal
open Bigarray

module Binable = Binable0

module Z : sig
  type t = (char, int8_unsigned_elt, c_layout) Array1.t [@@deriving bin_io, sexp]
end = struct
  type t = bigstring [@@deriving bin_io, sexp]
end
include Z

external aux_create: max_mem_waiting_gc:int -> size:int -> t = "bigstring_alloc"
external unsafe_destroy_and_resize: t -> len:int -> t = "bigstring_realloc"

let arch_sixtyfour = Sys.word_size = 64
let arch_big_endian = Sys.big_endian

let create ?max_mem_waiting_gc size =
  let max_mem_waiting_gc =
    match max_mem_waiting_gc with
    | None -> ~-1
    | Some v -> Float.to_int (Byte_units.bytes v)
  in
  (* This check is important because [aux_create ~size:(-1)] raises [Out_of_memory], which
     could be confusing during debugging. *)
  if size < 0 then invalid_argf "create: size = %d < 0" size ();
  aux_create ~max_mem_waiting_gc ~size

let length = Array1.dim

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" [@@noalloc]

let init n ~f =
  let t = create n in
  for i = 0 to n - 1; do
    t.{i} <- f i;
  done;
  t

let check_args ~loc ~pos ~len (bstr : t) =
  if pos < 0 then invalid_arg (loc ^ ": pos < 0");
  if len < 0 then invalid_arg (loc ^ ": len < 0");
  let bstr_len = length bstr in
  if bstr_len < pos + len then
    invalid_arg (sprintf "Bigstring.%s: length(bstr) < pos + len" loc)

let get_opt_len bstr ~pos = function
  | Some len -> len
  | None -> length bstr - pos

let sub_shared ?(pos = 0) ?len (bstr : t) =
  let len = get_opt_len bstr ~pos len in
  Array1.sub bstr pos len

(* Blitting *)

external unsafe_blit
  : src : t -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_stub"

(* Exposing the external version of get/set supports better inlining *)
external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

module Bigstring_sequence = struct
  type nonrec t = t [@@deriving sexp_of]
  let create ~len = create len
  let get = get
  let set = set
  let length = length
end

module Bytes_sequence = struct
  type t = bytes [@@deriving sexp_of]
  let create ~len = Bytes.create len
  let get = Bytes.get
  let set = Bytes.set
  let length = Bytes.length
end

module Blit_elt = struct
  include Char
  let of_bool b = if b then 'a' else 'b'
end

include Test_blit.Make_and_test
    (Blit_elt)
    (struct
      include Bigstring_sequence
      let unsafe_blit = unsafe_blit
    end)

module From_bytes =
  Test_blit.Make_distinct_and_test
    (Blit_elt)
    (Bytes_sequence)
    (struct
      external unsafe_blit
        : src : bytes -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
        = "bigstring_blit_bytes_bigstring_stub" [@@noalloc]
      include Bigstring_sequence
    end)
;;

module To_bytes =
  Test_blit.Make_distinct_and_test
    (Blit_elt)
    (Bigstring_sequence)
    (struct
      external unsafe_blit
        : src : t -> src_pos : int -> dst : bytes -> dst_pos : int -> len : int -> unit
        = "bigstring_blit_bigstring_bytes_stub" [@@noalloc]
      include Bytes_sequence
    end)
;;

(* We don't use [Test_blit.Make_distinct_and_test] for [From_string] because it expects
   mutability of the source. *)
module From_string =
  Blit.Make_distinct
    (struct
      type t = string [@@deriving sexp_of]
      let length = String.length
    end)
    (struct
      external unsafe_blit
        : src : string -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
        = "bigstring_blit_string_bigstring_stub" [@@noalloc]
      include Bigstring_sequence
    end)
;;

module To_string = struct
  include To_bytes
  include Blit.Make_to_string (Z) (To_bytes)
end

let of_string = From_string.subo

let of_bytes = From_bytes.subo

let to_string = To_string.subo

let to_bytes = To_bytes.subo

let concat =
  let append ~src ~dst ~dst_pos_ref =
    let len = length src in
    let src_pos = 0 in
    let dst_pos = !dst_pos_ref in
    blit ~dst ~dst_pos ~src ~src_pos ~len;
    dst_pos_ref := dst_pos + len
  in
  fun ?sep list ->
    match list with
    | []           -> create 0
    | head :: tail ->
      let head_len = length head in
      let sep_len = Option.value_map sep ~f:length ~default:0 in
      let tail_count = List.length tail in
      let len =
        head_len
        + (sep_len * tail_count)
        + List.sum (module Int) tail ~f:length
      in
      let dst = create len in
      let dst_pos_ref = ref 0 in
      append ~src:head ~dst ~dst_pos_ref;
      List.iter tail ~f:(fun src ->
        begin
          match sep with
          | None     -> ()
          | Some sep -> append ~src:sep ~dst ~dst_pos_ref
        end;
        append ~src ~dst ~dst_pos_ref);
      assert (!dst_pos_ref = len);
      dst

(* Comparison *)

external unsafe_memcmp
  : t1 : t -> t1_pos : int -> t2 : t -> t2_pos : int -> len : int -> int
  = "bigstring_memcmp_stub" [@@noalloc]

let compare t1 t2 =
  if phys_equal t1 t2 then 0 else
    let len1 = length t1 in
    let len2 = length t2 in
    let len = Int.min len1 len2 in
    match unsafe_memcmp ~t1 ~t1_pos:0 ~t2 ~t2_pos:0 ~len with
    | 0 ->
      if len1 < len2 then -1 else
      if len1 > len2 then  1 else
        0
    | n -> n

external internalhash_fold_bigstring :
  Hash.state -> t -> Hash.state
  = "internalhash_fold_bigstring" [@@noalloc]

let _making_sure_the_C_binding_takes_an_int (x : Hash.state) = (x :> int)

let hash_fold_t = internalhash_fold_bigstring
let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t

type t_frozen = t [@@deriving bin_io, compare, hash, sexp]

let equal t1 t2 =
  if phys_equal t1 t2 then true else
    let len1 = length t1 in
    let len2 = length t2 in
    Int.equal len1 len2
    && Int.equal (unsafe_memcmp ~t1 ~t1_pos:0 ~t2 ~t2_pos:0 ~len:len1) 0

(* Reading / writing bin-prot *)

let read_bin_prot_verbose_errors t ?(pos=0) ?len reader =
  let len = get_opt_len t len ~pos in
  let limit = pos + len in
  check_args ~loc:"read_bin_prot_verbose_errors" t ~pos ~len;
  let invalid_data message a sexp_of_a =
    `Invalid_data (Error.create message a sexp_of_a)
  in
  let read bin_reader ~pos ~len =
    if len > limit - pos
    then `Not_enough_data
    else
      let pos_ref = ref pos in
      match
        (try `Ok (bin_reader t ~pos_ref)
         with exn -> `Invalid_data (Error.of_exn exn))
      with
      | `Invalid_data _ as x -> x
      | `Ok result ->
        let expected_pos = pos + len in
        if !pos_ref = expected_pos
        then `Ok (result, expected_pos)
        else invalid_data "pos_ref <> expected_pos" (!pos_ref, expected_pos)
               [%sexp_of: int * int]
  in
  match
    read
      Bin_prot.Utils.bin_read_size_header
      ~pos
      ~len:Bin_prot.Utils.size_header_length
  with
  | `Not_enough_data | `Invalid_data _ as x -> x
  | `Ok (element_length, pos) ->
    if element_length < 0
    then invalid_data "negative element length %d" element_length [%sexp_of: int]
    else read reader.Bin_prot.Type_class.read ~pos ~len:element_length
;;

let read_bin_prot t ?pos ?len reader =
  match read_bin_prot_verbose_errors t ?pos ?len reader with
  | `Ok x -> Ok x
  | `Invalid_data e -> Error (Error.tag e ~tag:"Invalid data")
  | `Not_enough_data -> Or_error.error_string "not enough data"

let write_bin_prot t ?(pos = 0) writer v =
  let data_len = writer.Bin_prot.Type_class.size v in
  let total_len = data_len + Bin_prot.Utils.size_header_length in
  if pos < 0 then
    failwiths "Bigstring.write_bin_prot: negative pos" pos [%sexp_of: int];
  if pos + total_len > length t then
    failwiths "Bigstring.write_bin_prot: not enough room"
      (`pos pos, `pos_after_writing (pos + total_len), `bigstring_length (length t))
      [%sexp_of: [`pos of int] * [`pos_after_writing of int] * [`bigstring_length of int]];
  let pos_after_size_header = Bin_prot.Utils.bin_write_size_header t ~pos data_len in
  let pos_after_data =
    writer.Bin_prot.Type_class.write t ~pos:pos_after_size_header v
  in
  if pos_after_data - pos <> total_len then begin
    failwiths "Bigstring.write_bin_prot bug!"
      (`pos_after_data pos_after_data,
       `start_pos pos,
       `bin_prot_size_header_length Bin_prot.Utils.size_header_length,
       `data_len data_len,
       `total_len total_len)
      [%sexp_of:
        [`pos_after_data of int] * [`start_pos of int]
        * [`bin_prot_size_header_length of int] * [`data_len of int] * [`total_len of int]
      ]
  end;
  pos_after_data

(* Search *)

external unsafe_find : t -> char -> pos:int -> len:int -> int = "bigstring_find" [@@noalloc]

let find ?(pos = 0) ?len chr bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"find" ~pos ~len bstr;
  let res = unsafe_find bstr chr ~pos ~len in
  if res < 0 then None else Some res

(* Destruction *)

external unsafe_destroy : t -> unit = "bigstring_destroy_stub"

(* Hex dump *)

include Hexdump.Of_indexable (struct
    type nonrec t = t
    let length = length
    let get    = get
  end)
;;

(* vim: set filetype=ocaml : *)

(* Binary-packing like accessors *)

external int32_of_int : int -> int32 = "%int32_of_int"
external int32_to_int : int32 -> int = "%int32_to_int"
external int64_of_int : int -> int64 = "%int64_of_int"
external int64_to_int : int64 -> int = "%int64_to_int"

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_get_16 : t -> int -> int = "%caml_bigstring_get16u"
external unsafe_get_32 : t -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_get_64 : t -> int -> int64 = "%caml_bigstring_get64u"
external unsafe_set_16 : t -> int -> int -> unit = "%caml_bigstring_set16u"
external unsafe_set_32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
external unsafe_set_64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"

let get_16 (t : t) (pos : int) : int =
  check_args ~loc:"get_16" ~pos ~len:2 t;
  unsafe_get_16 t pos
;;

let get_32 (t : t) (pos : int) : int32 =
  check_args ~loc:"get_32" ~pos ~len:4 t;
  unsafe_get_32 t pos
;;

let get_64 (t : t) (pos : int) : int64 =
  check_args ~loc:"get_64" ~pos ~len:8 t;
  unsafe_get_64 t pos
;;

(* Assumes [v] is a valid 16-bit integer, because all call sites check this before
   performing any operations on [t]. *)
let set_16 (t : t) (pos : int) (v : int) : unit =
  check_args ~loc:"set_16" ~pos ~len:2 t;
  unsafe_set_16 t pos v
;;

let set_32 (t : t) (pos : int) (v : int32) : unit =
  check_args ~loc:"set_32" ~pos ~len:4 t;
  unsafe_set_32 t pos v
;;

let set_64 (t : t) (pos : int) (v : int64) : unit =
  check_args ~loc:"set_64" ~pos ~len:8 t;
  unsafe_set_64 t pos v
;;

let sign_extend_16 u = (u lsl (Int.num_bits - 16)) asr (Int.num_bits - 16)

let check_valid_uint16 ~loc x =
  if x < 0 || x > 0xFFFF then
    invalid_arg (sprintf "Bigstring.%s: %d is not a valid unsigned 16-bit integer" loc x)
;;

let check_valid_int16 ~loc x =
  if x < -0x8000 || x > 0x7FFF then
    invalid_arg (sprintf "Bigstring.%s: %d is not a valid 16-bit integer" loc x)
;;

let check_valid_uint8 ~loc x =
  if x < 0 || x > 0xFF then
    invalid_arg (sprintf "Bigstring.%s: %d is not a valid unsigned 8-bit integer" loc x)
;;

let check_valid_int8 ~loc x =
  if x < -0x80 || x > 0x7F then
    invalid_arg (sprintf "Bigstring.%s: %d is not a valid 8-bit integer" loc x)
;;

let check_valid_int32 =
  if not arch_sixtyfour
  then fun _ ~loc:_ -> ()
  else fun x ~loc ->
    if x >= -1 lsl 31 && x < 1 lsl 31
    then ()
    else invalid_arg (sprintf "Bigstring.%s: %d is not a valid 32-bit integer" loc x)
;;

let check_valid_uint32 =
  if not arch_sixtyfour
  then fun x ~loc ->
    if x >= 0
    then ()
    else
      invalid_arg
        (sprintf "Bigstring.%s: %d is not a valid unsigned 32-bit integer" loc x)
  else fun x ~loc ->
    if x >= 0 && x < 1 lsl 32
    then ()
    else
      invalid_arg
        (sprintf "Bigstring.%s: %d is not a valid unsigned 32-bit integer" loc x)
;;

let check_valid_uint64 x ~loc =
  if x >= 0
  then ()
  else
    invalid_arg (sprintf "Bigstring.%s: %d is not a valid unsigned 64-bit integer" loc x)
;;

let unsafe_read_int16 t ~pos          = sign_extend_16 (unsafe_get_16 t pos)
let unsafe_read_int16_swap t ~pos     = sign_extend_16 (swap16 (unsafe_get_16 t pos))
let unsafe_write_int16 t ~pos x       = unsafe_set_16 t pos x
let unsafe_write_int16_swap t ~pos x  = unsafe_set_16 t pos (swap16 x)

let read_int16 t ~pos          = sign_extend_16 (get_16 t pos)
let read_int16_swap t ~pos     = sign_extend_16 (swap16 (get_16 t pos))
let write_int16 t ~pos x       =
  check_valid_int16 x ~loc:"write_int16";
  set_16 t pos x
;;
let write_int16_swap t ~pos x  =
  (* Omit "_swap" from the error message it's bi-endian. *)
  check_valid_int16 x ~loc:"write_int16";
  set_16 t pos (swap16 x)
;;

let unsafe_read_uint16 t ~pos          = unsafe_get_16 t pos
let unsafe_read_uint16_swap t ~pos     = swap16 (unsafe_get_16 t pos)
let unsafe_write_uint16 t ~pos x       = unsafe_set_16 t pos x
let unsafe_write_uint16_swap t ~pos x  = unsafe_set_16 t pos (swap16 x)

let read_uint16 t ~pos          = get_16 t pos
let read_uint16_swap t ~pos     = swap16 (get_16 t pos)
let write_uint16 t ~pos x       =
  check_valid_uint16 x ~loc:"write_uint16";
  set_16 t pos x
;;
let write_uint16_swap t ~pos x  =
  (* Omit "_swap" from the error message it's bi-endian. *)
  check_valid_uint16 x ~loc:"write_uint16";
  set_16 t pos (swap16 x)
;;

let unsafe_read_int32_int t ~pos       = int32_to_int (unsafe_get_32 t pos)
let unsafe_read_int32_int_swap t ~pos  = int32_to_int (swap32 (unsafe_get_32 t pos))
let unsafe_read_int32 t ~pos           = unsafe_get_32 t pos
let unsafe_read_int32_swap t ~pos      = swap32 (unsafe_get_32 t pos)
let unsafe_write_int32 t ~pos x        = unsafe_set_32 t pos x
let unsafe_write_int32_swap t ~pos x   = unsafe_set_32 t pos (swap32 x)
let unsafe_write_int32_int t ~pos x    = unsafe_set_32 t pos (int32_of_int x)
let unsafe_write_int32_int_swap t ~pos x = unsafe_set_32 t pos (swap32 (int32_of_int x))

let read_int32_int t ~pos       = int32_to_int (get_32 t pos)
let read_int32_int_swap t ~pos  = int32_to_int (swap32 (get_32 t pos))
let read_int32 t ~pos           = get_32 t pos
let read_int32_swap t ~pos      = swap32 (get_32 t pos)
let write_int32 t ~pos x        = set_32 t pos x
let write_int32_swap t ~pos x   = set_32 t pos (swap32 x)

let write_int32_int t ~pos x =
  check_valid_int32 x ~loc:"write_int32_int";
  set_32 t pos (int32_of_int x)
;;
let write_int32_int_swap t ~pos x =
  (* Omit "_swap" from the error message it's bi-endian. *)
  check_valid_int32 x ~loc:"write_int32_int";
  set_32 t pos (swap32 (int32_of_int x))
;;

let unsafe_read_int64_int t ~pos      = int64_to_int (unsafe_get_64 t pos)
let unsafe_read_int64_int_swap t ~pos = int64_to_int (swap64 (unsafe_get_64 t pos))
let unsafe_read_int64 t ~pos          = unsafe_get_64 t pos
let unsafe_read_int64_swap t ~pos     = swap64 (unsafe_get_64 t pos)
let unsafe_write_int64 t ~pos x       = unsafe_set_64 t pos x
let unsafe_write_int64_swap t ~pos x  = unsafe_set_64 t pos (swap64 x)
let unsafe_write_int64_int t ~pos x       = unsafe_set_64 t pos (int64_of_int x)
let unsafe_write_int64_int_swap t ~pos x  = unsafe_set_64 t pos (swap64 (int64_of_int x))

let read_int64_int t ~pos      = int64_to_int (get_64 t pos)
let read_int64_int_swap t ~pos = int64_to_int (swap64 (get_64 t pos))
let read_int64 t ~pos          = get_64 t pos
let read_int64_swap t ~pos     = swap64 (get_64 t pos)
let write_int64 t ~pos x       = set_64 t pos x
let write_int64_swap t ~pos x  = set_64 t pos (swap64 x)
let write_int64_int t ~pos x       = set_64 t pos (int64_of_int x)
let write_int64_int_swap t ~pos x  = set_64 t pos (swap64 (int64_of_int x))

let unsafe_get_int16_be  =
  if arch_big_endian
  then unsafe_read_int16
  else unsafe_read_int16_swap
let unsafe_get_int16_le  =
  if arch_big_endian
  then unsafe_read_int16_swap
  else unsafe_read_int16
let unsafe_get_uint16_be =
  if arch_big_endian
  then unsafe_read_uint16
  else unsafe_read_uint16_swap
let unsafe_get_uint16_le =
  if arch_big_endian
  then unsafe_read_uint16_swap
  else unsafe_read_uint16

let get_int16_be  =
  if arch_big_endian
  then read_int16
  else read_int16_swap
let get_int16_le  =
  if arch_big_endian
  then read_int16_swap
  else read_int16
let get_uint16_be =
  if arch_big_endian
  then read_uint16
  else read_uint16_swap
let get_uint16_le =
  if arch_big_endian
  then read_uint16_swap
  else read_uint16

let unsafe_set_int16_be  =
  if arch_big_endian
  then unsafe_write_int16
  else unsafe_write_int16_swap
let unsafe_set_int16_le  =
  if arch_big_endian
  then unsafe_write_int16_swap
  else unsafe_write_int16
let unsafe_set_uint16_be =
  if arch_big_endian
  then unsafe_write_uint16
  else unsafe_write_uint16_swap
let unsafe_set_uint16_le =
  if arch_big_endian
  then unsafe_write_uint16_swap
  else unsafe_write_uint16

let set_int16_be  =
  if arch_big_endian
  then write_int16
  else write_int16_swap
let set_int16_le  =
  if arch_big_endian
  then write_int16_swap
  else write_int16
let set_uint16_be =
  if arch_big_endian
  then write_uint16
  else write_uint16_swap
let set_uint16_le =
  if arch_big_endian
  then write_uint16_swap
  else write_uint16

let unsafe_get_int32_t_be  =
  if arch_big_endian
  then unsafe_read_int32
  else unsafe_read_int32_swap
let unsafe_get_int32_t_le  =
  if arch_big_endian
  then unsafe_read_int32_swap
  else unsafe_read_int32
let unsafe_set_int32_t_be  =
  if arch_big_endian
  then unsafe_write_int32
  else unsafe_write_int32_swap
let unsafe_set_int32_t_le  =
  if arch_big_endian
  then unsafe_write_int32_swap
  else unsafe_write_int32

let get_int32_t_be  =
  if arch_big_endian
  then read_int32
  else read_int32_swap
let get_int32_t_le  =
  if arch_big_endian
  then read_int32_swap
  else read_int32
let set_int32_t_be  =
  if arch_big_endian
  then write_int32
  else write_int32_swap
let set_int32_t_le  =
  if arch_big_endian
  then write_int32_swap
  else write_int32

let unsafe_get_int32_be  =
  if arch_big_endian
  then unsafe_read_int32_int
  else unsafe_read_int32_int_swap
let unsafe_get_int32_le  =
  if arch_big_endian
  then unsafe_read_int32_int_swap
  else unsafe_read_int32_int
let unsafe_set_int32_be  =
  if arch_big_endian
  then unsafe_write_int32_int
  else unsafe_write_int32_int_swap
let unsafe_set_int32_le  =
  if arch_big_endian
  then unsafe_write_int32_int_swap
  else unsafe_write_int32_int

let get_int32_be  =
  if arch_big_endian
  then read_int32_int
  else read_int32_int_swap
let get_int32_le  =
  if arch_big_endian
  then read_int32_int_swap
  else read_int32_int
let set_int32_be  =
  if arch_big_endian
  then write_int32_int
  else write_int32_int_swap
let set_int32_le  =
  if arch_big_endian
  then write_int32_int_swap
  else write_int32_int

let unsafe_get_int64_be_trunc =
  if arch_big_endian
  then unsafe_read_int64_int
  else unsafe_read_int64_int_swap
let unsafe_get_int64_le_trunc =
  if arch_big_endian
  then unsafe_read_int64_int_swap
  else unsafe_read_int64_int
let unsafe_set_int64_be       =
  if arch_big_endian
  then unsafe_write_int64_int
  else unsafe_write_int64_int_swap
let unsafe_set_int64_le       =
  if arch_big_endian
  then unsafe_write_int64_int_swap
  else unsafe_write_int64_int

let get_int64_be_trunc =
  if arch_big_endian
  then read_int64_int
  else read_int64_int_swap
let get_int64_le_trunc =
  if arch_big_endian
  then read_int64_int_swap
  else read_int64_int
let set_int64_be       =
  if arch_big_endian
  then write_int64_int
  else write_int64_int_swap
let set_int64_le       =
  if arch_big_endian
  then write_int64_int_swap
  else write_int64_int

let unsafe_get_int64_t_be  =
  if arch_big_endian
  then unsafe_read_int64
  else unsafe_read_int64_swap
let unsafe_get_int64_t_le  =
  if arch_big_endian
  then unsafe_read_int64_swap
  else unsafe_read_int64
let unsafe_set_int64_t_be  =
  if arch_big_endian
  then unsafe_write_int64
  else unsafe_write_int64_swap
let unsafe_set_int64_t_le  =
  if arch_big_endian
  then unsafe_write_int64_swap
  else unsafe_write_int64

let get_int64_t_be  =
  if arch_big_endian
  then read_int64
  else read_int64_swap
let get_int64_t_le  =
  if arch_big_endian
  then read_int64_swap
  else read_int64
let set_int64_t_be  =
  if arch_big_endian
  then write_int64
  else write_int64_swap
let set_int64_t_le  =
  if arch_big_endian
  then write_int64_swap
  else write_int64

let int64_conv_error () =
  failwith "unsafe_read_int64: value cannot be represented unboxed!"
;;

let uint64_conv_error () =
  failwith "unsafe_read_uint64: value cannot be represented unboxed!"
;;

let int64_to_int_exn n =
  if arch_sixtyfour
  then
    if n >= -0x4000_0000_0000_0000L && n < 0x4000_0000_0000_0000L then
      int64_to_int n
    else
      int64_conv_error ()
  else
  if n >= -0x0000_0000_4000_0000L && n < 0x0000_0000_4000_0000L then
    int64_to_int n
  else
    int64_conv_error ()
;;

let uint64_to_int_exn n =
  if arch_sixtyfour
  then
    if n >= 0L && n < 0x4000_0000_0000_0000L then
      int64_to_int n
    else
      uint64_conv_error ()
  else
  if n >= 0L && n < 0x0000_0000_4000_0000L then
    int64_to_int n
  else
    uint64_conv_error ()
;;

let unsafe_get_int64_be_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_be t ~pos)
let unsafe_get_int64_le_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_le t ~pos)

let get_int64_be_exn t ~pos = int64_to_int_exn (get_int64_t_be t ~pos)
let get_int64_le_exn t ~pos = int64_to_int_exn (get_int64_t_le t ~pos)

let unsafe_get_uint64_be_exn t ~pos = uint64_to_int_exn (unsafe_get_int64_t_be t ~pos)
let unsafe_get_uint64_le_exn t ~pos = uint64_to_int_exn (unsafe_get_int64_t_le t ~pos)

let get_uint64_be_exn t ~pos = uint64_to_int_exn (get_int64_t_be t ~pos)
let get_uint64_le_exn t ~pos = uint64_to_int_exn (get_int64_t_le t ~pos)

let unsafe_set_uint64_be = unsafe_set_int64_be
let unsafe_set_uint64_le = unsafe_set_int64_le

let set_uint64_be t ~pos n =
  check_valid_uint64 ~loc:"set_uint64_be" n;
  set_int64_be t ~pos n
;;

let set_uint64_le t ~pos n =
  check_valid_uint64 ~loc:"set_uint64_le" n;
  set_int64_le t ~pos n
;;

(* Type annotations on the [t]s are important here: in order for the compiler to generate
   optimized code, it needs to know the fully instantiated type of the bigarray. This is
   because the type of the bigarray encodes the element kind and the layout of the
   bigarray. Without the annotation the compiler generates a C call to the generic access
   functions. *)
let unsafe_set_uint8 (t : t) ~pos n =
  Array1.unsafe_set t pos (Char.unsafe_of_int n)
let unsafe_set_int8 (t : t) ~pos n =
  (* in all the set functions where there are these tests, it looks like the test could be
     removed, since they are only changing the values of the bytes that are not
     written. *)
  let n = if n < 0 then n + 256 else n in
  Array1.unsafe_set t pos (Char.unsafe_of_int n)
let unsafe_get_uint8 (t : t) ~pos =
  Char.to_int (Array1.unsafe_get t pos)
let unsafe_get_int8 (t : t) ~pos =
  let n = Char.to_int (Array1.unsafe_get t pos) in
  if n >= 128 then n - 256 else n

let set_uint8 (t : t) ~pos n =
  check_valid_uint8 ~loc:"set_uint8" n;
  Array1.set t pos (Char.unsafe_of_int n)
let set_int8 (t : t) ~pos n =
  check_valid_int8 ~loc:"set_int8" n;
  let n = if n < 0 then n + 256 else n in
  Array1.set t pos (Char.unsafe_of_int n)
let get_uint8 (t : t) ~pos =
  Char.to_int (Array1.get t pos)
let get_int8 (t : t) ~pos =
  let n = Char.to_int (Array1.get t pos) in
  if n >= 128 then n - 256 else n

let not_on_32bit = Sys.word_size > 32
let unsafe_set_uint32_le t ~pos n =
  let n = if not_on_32bit && n >= 1 lsl 31 then n - 1 lsl 32 else n in
  unsafe_set_int32_le t ~pos n
let unsafe_set_uint32_be t ~pos n =
  let n = if not_on_32bit && n >= 1 lsl 31 then n - 1 lsl 32 else n in
  unsafe_set_int32_be t ~pos n
let unsafe_get_uint32_le t ~pos =
  let n = unsafe_get_int32_le t ~pos in
  if not_on_32bit && n < 0 then n + 1 lsl 32 else n
let unsafe_get_uint32_be t ~pos =
  let n = unsafe_get_int32_be t ~pos in
  if not_on_32bit && n < 0 then n + 1 lsl 32 else n

let set_uint32_le t ~pos n =
  check_valid_uint32 ~loc:"set_uint32_le" n;
  let n = if not_on_32bit && n >= 1 lsl 31 then n - 1 lsl 32 else n in
  set_int32_le t ~pos n
let set_uint32_be t ~pos n =
  check_valid_uint32 ~loc:"set_uint32_be" n;
  let n = if not_on_32bit && n >= 1 lsl 31 then n - 1 lsl 32 else n in
  set_int32_be t ~pos n
let get_uint32_le t ~pos =
  let n = get_int32_le t ~pos in
  if not_on_32bit && n < 0 then n + 1 lsl 32 else n
let get_uint32_be t ~pos =
  let n = get_int32_be t ~pos in
  if not_on_32bit && n < 0 then n + 1 lsl 32 else n

let rec last_nonmatch_plus_one ~buf ~min_pos ~pos ~char =
  let pos' = pos - 1 in
  if pos' >= min_pos && Char.(=) (get buf pos') char then
    last_nonmatch_plus_one ~buf ~min_pos ~pos:pos' ~char
  else
    pos

let get_tail_padded_fixed_string ~padding t ~pos ~len () =
  let data_end = last_nonmatch_plus_one ~buf:t ~min_pos:pos ~pos:(pos + len) ~char:padding in
  to_string t ~pos ~len:(data_end - pos)

let set_tail_padded_fixed_string ~padding t ~pos ~len value =
  let slen = String.length value in
  if slen > len then
    failwithf "Bigstring.set_tail_padded_fixed_string: %S is longer than %d" value len ();
  From_string.blit ~src:value ~dst:t ~src_pos:0 ~dst_pos:pos ~len:slen;
  for i = pos + slen to pos + len - 1; do
    set t i padding
  done

let rec first_nonmatch ~buf ~pos ~max_pos ~char =
  if pos <= max_pos && Char.(=) (get buf pos) char then
    first_nonmatch ~buf ~pos:(succ pos) ~max_pos ~char
  else
    pos

let set_head_padded_fixed_string ~padding t ~pos ~len value =
  let slen = String.length value in
  if slen > len then
    failwithf "Bigstring.set_head_padded_fixed_string: %S is longer than %d" value len ();
  From_string.blit ~src:value ~dst:t ~src_pos:0 ~dst_pos:(pos + len - slen) ~len:slen;
  for i = pos to pos + len - slen - 1; do
    set t i padding
  done

let get_head_padded_fixed_string ~padding t ~pos ~len () =
  let data_begin = first_nonmatch ~buf:t ~pos ~max_pos:(pos + len - 1) ~char: padding in
  to_string t ~pos:data_begin ~len:(len - (data_begin - pos))

module Private = struct
  let sign_extend_16 = sign_extend_16
end
