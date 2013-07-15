INCLUDE "config.mlh"

open Std_internal
open Bigarray

module Binable = Binable0

module Z : sig
  type t = (char, int8_unsigned_elt, c_layout) Array1.t with bin_io, sexp
end = struct
  include Bin_prot.Std
  include Sexplib.Conv
  type t = bigstring with bin_io, sexp
end
include Z

external aux_create: max_mem_waiting_gc:int -> size:int -> t = "bigstring_alloc"

let create ?max_mem_waiting_gc size =
  let max_mem_waiting_gc =
    match max_mem_waiting_gc with
    | None -> ~-1
    | Some v -> Float.to_int (Byte_units.bytes v)
  in
  (* vgatien-baron: aux_create ~size:(-1) throws Out of memory, which could be quite
     confusing during debugging. *)
  if size < 0 then invalid_argf "create: size = %d < 0" size ();
  aux_create ~max_mem_waiting_gc ~size

TEST "create with different max_mem_waiting_gc" =
  Core_gc.full_major ();
  let count_gc_cycles mem_units =
    let cycles = ref 0 in
    let alarm = Core_gc.create_alarm (fun () -> incr cycles) in
    let large_int = 10_000 in
    let max_mem_waiting_gc = Byte_units.create mem_units 256. in
    for _i = 0 to large_int do
      let (_ : t) = create ~max_mem_waiting_gc large_int in
      ()
    done;
    Core_gc.delete_alarm alarm;
    !cycles
  in
  let large_max_mem = count_gc_cycles `Megabytes in
  let small_max_mem = count_gc_cycles `Bytes in
  (* We don't care if it's twice as many, we are only testing that there are less cycles
  involved *)
  (2 * large_max_mem) < small_max_mem


external length : t -> int = "bigstring_length" "noalloc"

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" "noalloc"

let init n ~f =
  let t = create n in
  for i = 0 to n - 1; do
    t.{i} <- f i;
  done;
  t
;;

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

let get bstr pos = Array1.get bstr pos

let set bstr pos c = Array1.set bstr pos c

module Bigstring_sequence = struct
  type nonrec t = t with sexp_of
  let create ~len = create len
  let get = get
  let set = set
  let length = length
end

module String_sequence = struct
  type t = string with sexp_of
  let create ~len = String.create len
  let get = String.get
  let set = String.set
  let length = String.length
end

module Blit_elt = struct
  include Char
  let of_bool b = if b then 'a' else 'b'
end

include Blit.Make
          (Blit_elt)
          (struct
            include Bigstring_sequence
            let unsafe_blit = unsafe_blit
          end)

module From_string =
  Blit.Make_distinct
    (Blit_elt)
    (String_sequence)
    (struct
      external unsafe_blit
        : src : string -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
        = "bigstring_blit_string_bigstring_stub" "noalloc"
      include Bigstring_sequence
    end)
;;

module To_string =
  Blit.Make_distinct
    (Blit_elt)
    (Bigstring_sequence)
    (struct
      external unsafe_blit
        : src : t -> src_pos : int -> dst : string -> dst_pos : int -> len : int -> unit
        = "bigstring_blit_bigstring_string_stub" "noalloc"
      include String_sequence
    end)
;;

let of_string = From_string.subo

let to_string = To_string.subo

(* Memory mapping *)

let map_file ~shared fd n = Array1.map_file fd Bigarray.char c_layout shared n

(* Search *)

external unsafe_find : t -> char -> pos:int -> len:int -> int = "bigstring_find"
let find ?(pos = 0) ?len chr bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"find" ~pos ~len bstr;
  let res = unsafe_find bstr chr ~pos ~len in
  if res < 0 then None else Some res

(* Destruction *)

external unsafe_destroy : t -> unit = "bigstring_destroy_stub"

(* vim: set filetype=ocaml : *)

(* Binary-packing like accessors *)

external unsafe_read_int16            : t -> pos:int -> int
  = "unsafe_read_int16_t"       "noalloc"
external unsafe_read_int16_swap       : t -> pos:int -> int
  = "unsafe_read_int16_t_swap"  "noalloc"
external unsafe_read_uint16           : t -> pos:int -> int
  = "unsafe_read_uint16_t"      "noalloc"
external unsafe_read_uint16_swap      : t -> pos:int -> int
  = "unsafe_read_uint16_t_swap" "noalloc"

external unsafe_write_int16           : t -> pos:int -> int -> unit
  = "unsafe_write_int16_t"       "noalloc"
external unsafe_write_int16_swap      : t -> pos:int -> int -> unit
  = "unsafe_write_int16_t_swap"  "noalloc"
external unsafe_write_uint16          : t -> pos:int -> int -> unit
  = "unsafe_write_uint16_t"      "noalloc"
external unsafe_write_uint16_swap     : t -> pos:int -> int -> unit
  = "unsafe_write_uint16_t_swap" "noalloc"

external unsafe_read_int32_int        : t -> pos:int -> int
  = "unsafe_read_int32_t"         "noalloc"
external unsafe_read_int32_int_swap   : t -> pos:int -> int
  = "unsafe_read_int32_t_swap"    "noalloc"

external unsafe_write_int32_int       : t -> pos:int -> int -> unit
  = "unsafe_write_int32_t"        "noalloc"
external unsafe_write_int32_int_swap  : t -> pos:int -> int -> unit
  = "unsafe_write_int32_t_swap"   "noalloc"

external unsafe_read_int32            : t -> pos:int -> Int32.t
  = "unsafe_read_int32"
external unsafe_read_int32_swap       : t -> pos:int -> Int32.t
  = "unsafe_read_int32_swap"
external unsafe_write_int32           : t -> pos:int -> Int32.t -> unit
  = "unsafe_write_int32"       "noalloc"
external unsafe_write_int32_swap      : t -> pos:int -> Int32.t -> unit
  = "unsafe_write_int32_swap"  "noalloc"

(* [unsafe_read_int64_int] and [unsafe_read_int64_int_swap] may raise exceptions on
   both 32-bit and 64-bit platforms.  As such, they cannot be marked [noalloc].
*)
external unsafe_read_int64_int        : t -> pos:int -> int
  = "unsafe_read_int64_t"
external unsafe_read_int64_int_swap   : t -> pos:int -> int
  = "unsafe_read_int64_t_swap"

external unsafe_write_int64_int       : t -> pos:int -> int -> unit
  = "unsafe_write_int64_t"      "noalloc"
external unsafe_write_int64_int_swap  : t -> pos:int -> int -> unit
  = "unsafe_write_int64_t_swap" "noalloc"

external unsafe_read_int64            : t -> pos:int -> Int64.t
  = "unsafe_read_int64"
external unsafe_read_int64_swap       : t -> pos:int -> Int64.t
  = "unsafe_read_int64_swap"
external unsafe_write_int64           : t -> pos:int -> Int64.t -> unit
  = "unsafe_write_int64"
external unsafe_write_int64_swap      : t -> pos:int -> Int64.t -> unit
  = "unsafe_write_int64_swap"


IFDEF ARCH_BIG_ENDIAN THEN
let unsafe_get_int16_be  = unsafe_read_int16
let unsafe_get_int16_le  = unsafe_read_int16_swap
let unsafe_get_uint16_be = unsafe_read_uint16
let unsafe_get_uint16_le = unsafe_read_uint16_swap

let unsafe_set_int16_be  = unsafe_write_int16
let unsafe_set_int16_le  = unsafe_write_int16_swap
let unsafe_set_uint16_be = unsafe_write_uint16
let unsafe_set_uint16_le = unsafe_write_uint16_swap

let unsafe_get_int32_t_be  = unsafe_read_int32
let unsafe_get_int32_t_le  = unsafe_read_int32_swap
let unsafe_set_int32_t_be  = unsafe_write_int32
let unsafe_set_int32_t_le  = unsafe_write_int32_swap

let unsafe_get_int32_be  = unsafe_read_int32_int
let unsafe_get_int32_le  = unsafe_read_int32_int_swap
let unsafe_set_int32_be  = unsafe_write_int32_int
let unsafe_set_int32_le  = unsafe_write_int32_int_swap

let unsafe_get_int64_be_exn = unsafe_read_int64_int
let unsafe_get_int64_le_exn = unsafe_read_int64_int_swap
let unsafe_set_int64_be  = unsafe_write_int64_int
let unsafe_set_int64_le  = unsafe_write_int64_int_swap

let unsafe_get_int64_t_be  = unsafe_read_int64
let unsafe_get_int64_t_le  = unsafe_read_int64_swap
let unsafe_set_int64_t_be  = unsafe_write_int64
let unsafe_set_int64_t_le  = unsafe_write_int64_swap
ELSE
let unsafe_get_int16_be  = unsafe_read_int16_swap
let unsafe_get_int16_le  = unsafe_read_int16
let unsafe_get_uint16_be = unsafe_read_uint16_swap
let unsafe_get_uint16_le = unsafe_read_uint16

let unsafe_set_int16_be  = unsafe_write_int16_swap
let unsafe_set_int16_le  = unsafe_write_int16
let unsafe_set_uint16_be = unsafe_write_uint16_swap
let unsafe_set_uint16_le = unsafe_write_uint16

let unsafe_get_int32_be  = unsafe_read_int32_int_swap
let unsafe_get_int32_le  = unsafe_read_int32_int
let unsafe_set_int32_be  = unsafe_write_int32_int_swap
let unsafe_set_int32_le  = unsafe_write_int32_int

let unsafe_get_int32_t_be  = unsafe_read_int32_swap
let unsafe_get_int32_t_le  = unsafe_read_int32
let unsafe_set_int32_t_be  = unsafe_write_int32_swap
let unsafe_set_int32_t_le  = unsafe_write_int32

let unsafe_get_int64_be_exn  = unsafe_read_int64_int_swap
let unsafe_get_int64_le_exn  = unsafe_read_int64_int
let unsafe_set_int64_be  = unsafe_write_int64_int_swap
let unsafe_set_int64_le  = unsafe_write_int64_int

let unsafe_get_int64_t_be  = unsafe_read_int64_swap
let unsafe_get_int64_t_le  = unsafe_read_int64
let unsafe_set_int64_t_be  = unsafe_write_int64_swap
let unsafe_set_int64_t_le  = unsafe_write_int64
ENDIF

let unsafe_set_uint8 t ~pos n =
  Array1.unsafe_set t pos (Char.unsafe_of_int n)
let unsafe_set_int8 t ~pos n =
  (* in all the set functions where there are these tests, it looks like the test could be
     removed, since they are only changing the values of the bytes that are not written. *)
  let n = if n < 0 then n + 256 else n in
  Array1.unsafe_set t pos (Char.unsafe_of_int n)
let unsafe_get_uint8 t ~pos =
  Char.to_int (Array1.unsafe_get t pos)
let unsafe_get_int8 t ~pos =
  let n = Char.to_int (Array1.unsafe_get t pos) in
  if n >= 128 then n - 256 else n

let unsafe_set_uint32_le t ~pos n =
  let n = if n >= 1 lsl 31 then n - 1 lsl 32 else n in
  unsafe_set_int32_le t ~pos n
let unsafe_set_uint32_be t ~pos n =
  let n = if n >= 1 lsl 31 then n - 1 lsl 32 else n in
  unsafe_set_int32_be t ~pos n
let unsafe_get_uint32_le t ~pos =
  let n = unsafe_get_int32_le t ~pos in
  if n < 0 then n + 1 lsl 32 else n
let unsafe_get_uint32_be t ~pos =
  let n = unsafe_get_int32_be t ~pos in
  if n < 0 then n + 1 lsl 32 else n

TEST_MODULE "binary accessors" = struct

  let buf = create 256

  let test_accessor ~buf ~fget ~fset vals =
    Core_list.for_all vals ~f:(fun x -> fset buf ~pos:0 x; x = fget buf ~pos:0)
  ;;

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int16_le
    ~fset:unsafe_set_int16_le
    [-32768; -1; 0; 1; 32767]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_uint16_le
    ~fset:unsafe_set_uint16_le
    [0; 1; 65535]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int16_be
    ~fset:unsafe_set_int16_be
    [-32768; -1; 0; 1; 32767]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_uint16_be
    ~fset:unsafe_set_uint16_be
    [0; 1; 65535]


IFDEF ARCH_SIXTYFOUR THEN

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int32_le
    ~fset:unsafe_set_int32_le
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int32_be
    ~fset:unsafe_set_int32_be
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_le_exn
    ~fset:unsafe_set_int64_le
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_be_exn
    ~fset:unsafe_set_int64_be
    [Int64.to_int_exn (-0x4000_0000_0000_0000L);
     Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L;
     Int64.to_int_exn 0x3fff_ffff_ffff_ffffL]

ENDIF (* ARCH_SIXTYFOUR *)

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_t_le
    ~fset:unsafe_set_int64_t_le
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_t_be
    ~fset:unsafe_set_int64_t_be
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_t_be
    ~fset:unsafe_set_int64_t_be
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]

  (* Test 63/64-bit precision boundary.

     Seen on a data stream the constant 0x4000_0000_0000_0000 is supposed to
     represent a 64-bit positive integer (2^62).

     Whilst this bit pattern does fit inside an OCaml value of type [int] on a
     64-bit machine, it is the representation of a negative number (the most negative
     number representable in type [int]), and in particular is not the representation
     of 2^62.  It is thus suitable for this test.
  *)
  TEST = let too_big = 0x4000_0000_0000_0000L in
    unsafe_set_int64_t_le buf ~pos:0 too_big;
    try
      let _too_small = unsafe_get_int64_le_exn buf ~pos:0 in false
    with _ -> true
end

let rec last_nonmatch_plus_one ~buf ~min_pos ~pos ~char =
  let pos' = pos - 1 in
  if pos' >= min_pos && Char.(=) (get buf pos') char then
    last_nonmatch_plus_one ~buf ~min_pos ~pos:pos' ~char
  else
    pos

let get_padded_fixed_string ~padding t ~pos ~len () =
  let data_end = last_nonmatch_plus_one ~buf:t ~min_pos:pos ~pos:(pos + len) ~char:padding in
  to_string t ~pos ~len:(data_end - pos)

let set_padded_fixed_string ~padding t ~pos ~len value =
  let slen = String.length value in
  if slen > len then
    failwithf "Bigstring.set_padded_fixed_string: %S is longer than %d" value len ();
  From_string.blit ~src:value ~dst:t ~src_pos:0 ~dst_pos:pos ~len:slen;
  for i = pos + slen to pos + len - 1; do
    set t i padding
  done
