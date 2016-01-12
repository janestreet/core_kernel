#import "config.mlh"

open Std_internal
open Bigarray

module Binable = Binable0

module Z : sig
  type t = (char, int8_unsigned_elt, c_layout) Array1.t [@@deriving bin_io, sexp]
end = struct
  include Bin_prot.Std
  include Sexplib.Conv
  type t = bigstring [@@deriving bin_io, sexp]
end
include Z

external aux_create: max_mem_waiting_gc:int -> size:int -> t = "bigstring_alloc"

let create ?max_mem_waiting_gc size =
  let max_mem_waiting_gc =
    match max_mem_waiting_gc with
    | None -> ~-1
    | Some v -> Float.to_int (Byte_units.bytes v)
  in
  if size < 0 then invalid_argf "create: size = %d < 0" size ();
  aux_create ~max_mem_waiting_gc ~size

let%test "create with different max_mem_waiting_gc" =
  Core_gc.full_major ();
  let module Alarm = Core_gc.Expert.Alarm in
  let count_gc_cycles mem_units =
    let cycles = ref 0 in
    let alarm = Alarm.create (fun () -> incr cycles) in
    let large_int = 10_000 in
    let max_mem_waiting_gc = Byte_units.create mem_units 256. in
    for _ = 0 to large_int do
      let (_ : t) = create ~max_mem_waiting_gc large_int in
      ()
    done;
    Alarm.delete alarm;
    !cycles
  in
  let large_max_mem = count_gc_cycles `Megabytes in
  let small_max_mem = count_gc_cycles `Bytes in
  (* We don't care if it's twice as many, we are only testing that there are less cycles
  involved *)
  (2 * large_max_mem) < small_max_mem

let length = Array1.dim

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

module String_sequence = struct
  type t = string [@@deriving sexp_of]
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

let%test_module "concat" =
  (module struct

    let test ?sep list =
      [%test_result: t]
        (concat ?sep:(Option.map sep ~f:of_string)
           (List.map list ~f:of_string))
        ~expect:(of_string (String.concat ?sep list))

    let%test_unit _ = test []
    let%test_unit _ = test [""]
    let%test_unit _ = test ["foo"]
    let%test_unit _ = test ["foo"; "bar"]
    let%test_unit _ = test ["foo"; "bar"; "baz"]
    let%test_unit _ = test ~sep:"," []
    let%test_unit _ = test ~sep:"," [""]
    let%test_unit _ = test ~sep:"," ["foo"]
    let%test_unit _ = test ~sep:"," ["foo"; "bar"]
    let%test_unit _ = test ~sep:"," ["foo"; "bar"; "baz"]
    let%test_unit _ = test ~sep:",.?" ["Strings"; "of"; "different"; "lengths."]

  end)

(* Comparison *)

external unsafe_memcmp
  : t1 : t -> t1_pos : int -> t2 : t -> t2_pos : int -> len : int -> int
  = "bigstring_memcmp_stub" "noalloc"

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

let equal t1 t2 =
  if phys_equal t1 t2 then true else
    let len1 = length t1 in
    let len2 = length t2 in
    Int.equal len1 len2
    && Int.equal (unsafe_memcmp ~t1 ~t1_pos:0 ~t2 ~t2_pos:0 ~len:len1) 0

let%test_module "comparison" = (module struct
  let sign n =
    if n < 0 then ~-1 else
    if n > 0 then   1 else
      0

  let check t1 t2 int =
    let bool = match int with 0 -> true | _ -> false in
    [%test_result: int]  (sign (compare t1 t2)) ~expect:int;
    [%test_result: bool] (equal t1 t2)          ~expect:bool

  let%test_unit _ = let t = of_string "cat" in check t t 0
  let%test_unit _ = check (of_string "cat") (of_string "cat")   0
  let%test_unit _ = check (of_string "cat") (of_string "cab")   1
  let%test_unit _ = check (of_string "cat") (of_string "caz") ~-1
  let%test_unit _ = check (of_string "cat") (of_string "c")     1
  let%test_unit _ = check (of_string "c")   (of_string "cat") ~-1
  let%test_unit _ = check (of_string "cat") (of_string "dog") ~-1
  let%test_unit _ = check (of_string "dog") (of_string "cat")   1
end)

let%bench_module "comparison" = (module struct
  let t1 = of_string "microsoft"
  let t2 = of_string "microsoff"

  let%bench "equal" = equal t1 t2
end)

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

let%test_module _ = (module struct
  let make_t ~size input =
    (* We hardcode the size here to catch problems if [Bin_prot.Utils.size_header_length]
       ever changes. *)
    let t = create (String.length input + 8) in
    ignore (Bin_prot.Write.bin_write_int_64bit t ~pos:0 size : int);
    List.iteri (String.to_list input) ~f:(fun i c -> set t (i+8) c);
    t

  let test (type a) ~size input ?pos ?len reader sexp_of_a compare_a ~expect =
    let result =
      match read_bin_prot_verbose_errors (make_t ~size input) ?pos ?len reader with
      | `Ok (x, _bytes_read) -> `Ok x
      | `Not_enough_data -> `Not_enough_data
      | `Invalid_data _ -> `Invalid_data
    in
    [%test_result: [ `Ok of a | `Not_enough_data | `Invalid_data ]] result ~expect

  let test_int ?pos ?len ~size input ~expect =
    test ~size input ?pos ?len Int.bin_reader_t Int.sexp_of_t Int.compare ~expect
  let test_string ?pos ?len ~size input ~expect =
    test ~size input ?pos ?len String.bin_reader_t String.sexp_of_t String.compare ~expect

  (* Keep in mind that the string bin-prot representation is itself prefixed with a
     length, so strings under the length-prefixed bin-prot protocol end up with two
     lengths at the front. *)
  let%test_unit _ = test_int    ~size:1 "\042"            ~expect:(`Ok 42)
  let%test_unit _ = test_int    ~size:1 "\042suffix"      ~expect:(`Ok 42)
  let%test_unit _ = test_string ~size:4 "\003foo"         ~expect:(`Ok "foo")
  let%test_unit _ = test_string ~size:4 "\003foo" ~len:12 ~expect:(`Ok "foo")

  let%test "pos <> 0" =
    let t =
       ("prefix" ^ to_string (make_t ~size:4 "\003foo") ^ "suffix")
       |> of_string
    in
    read_bin_prot_verbose_errors t ~pos:6 String.bin_reader_t = `Ok ("foo", 18)

  let%test_unit "negative size" = test_string ~size:(-1) "\003foo" ~expect:`Invalid_data
  let%test_unit "wrong size"    = test_string ~size:3    "\003foo" ~expect:`Invalid_data
  let%test_unit "bad bin-prot"  = test_string ~size:4    "\007foo" ~expect:`Invalid_data

  let%test_unit "len too short" = test_string ~size:4 "\003foo" ~len:3 ~expect:`Not_enough_data

  let%test "no header" =
    let t = of_string "\003foo" in
    read_bin_prot_verbose_errors t String.bin_reader_t = `Not_enough_data
end)

let read_bin_prot t ?pos ?len reader =
  match read_bin_prot_verbose_errors t ?pos ?len reader with
  | `Ok x -> Ok x
  | `Invalid_data e -> Error (Error.tag e "Invalid data")
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

let%test_module _ = (module struct
  let test ?pos writer v ~expect =
    let size = writer.Bin_prot.Type_class.size v + 8 in
    let t = create size in
    ignore (write_bin_prot t ?pos writer v : int);
    [%test_result: string] (to_string t) ~expect

  let%test_unit _ =
    test String.bin_writer_t "foo" ~expect:"\004\000\000\000\000\000\000\000\003foo"
  let%test_unit _ =
    test Int.bin_writer_t    123   ~expect:"\001\000\000\000\000\000\000\000\123"
  let%test_unit _ =
    test
      (Or_error.bin_writer_t Unit.bin_writer_t)
      (Or_error.error_string "test")
      ~expect:"\007\000\000\000\000\000\000\000\001\001\004test"
  ;;
end)

(* Memory mapping *)

let map_file ~shared fd n = Array1.map_file fd Bigarray.char c_layout shared n

(* Search *)

external unsafe_find : t -> char -> pos:int -> len:int -> int = "bigstring_find" "noalloc"

let find ?(pos = 0) ?len chr bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"find" ~pos ~len bstr;
  let res = unsafe_find bstr chr ~pos ~len in
  if res < 0 then None else Some res

(* Destruction *)

external unsafe_destroy : t -> unit = "bigstring_destroy_stub"

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

let sign_extend_16 u = (u lsl (Core_int.num_bits - 16)) asr (Core_int.num_bits - 16)

let%test_unit _ =
  List.iter [ 0,0
            ; 1,1
            ; 0x7fff,  32767
            ; 0xffff, -1
            ; 0x8000, -32768]
    ~f:(fun (i,expect) ->
      assert (i >= 0);
      [%test_result: int] ~expect (sign_extend_16 i)
    )

let unsafe_read_int16 t ~pos          = sign_extend_16 (unsafe_get_16 t pos)
let unsafe_read_int16_swap t ~pos     = sign_extend_16 (swap16 (unsafe_get_16 t pos))
let unsafe_write_int16 t ~pos x       = unsafe_set_16 t pos x
let unsafe_write_int16_swap t ~pos x  = unsafe_set_16 t pos (swap16 x)

let unsafe_read_uint16 t ~pos          = unsafe_get_16 t pos
let unsafe_read_uint16_swap t ~pos     = swap16 (unsafe_get_16 t pos)
let unsafe_write_uint16 t ~pos x       = unsafe_set_16 t pos x
let unsafe_write_uint16_swap t ~pos x  = unsafe_set_16 t pos (swap16 x)

let unsafe_read_int32_int t ~pos       = int32_to_int (unsafe_get_32 t pos)
let unsafe_read_int32_int_swap t ~pos  = int32_to_int (swap32 (unsafe_get_32 t pos))
let unsafe_read_int32 t ~pos           = unsafe_get_32 t pos
let unsafe_read_int32_swap t ~pos      = swap32 (unsafe_get_32 t pos)
let unsafe_write_int32 t ~pos x        = unsafe_set_32 t pos x
let unsafe_write_int32_swap t ~pos x   = unsafe_set_32 t pos (swap32 x)
let unsafe_write_int32_int t ~pos x    = unsafe_set_32 t pos (int32_of_int x)
let unsafe_write_int32_int_swap t ~pos x = unsafe_set_32 t pos (swap32 (int32_of_int x))

let unsafe_read_int64_int t ~pos      = int64_to_int (unsafe_get_64 t pos)
let unsafe_read_int64_int_swap t ~pos = int64_to_int (swap64 (unsafe_get_64 t pos))
let unsafe_read_int64 t ~pos          = unsafe_get_64 t pos
let unsafe_read_int64_swap t ~pos     = swap64 (unsafe_get_64 t pos)
let unsafe_write_int64 t ~pos x       = unsafe_set_64 t pos x
let unsafe_write_int64_swap t ~pos x  = unsafe_set_64 t pos (swap64 x)
let unsafe_write_int64_int t ~pos x       = unsafe_set_64 t pos (int64_of_int x)
let unsafe_write_int64_int_swap t ~pos x  = unsafe_set_64 t pos (swap64 (int64_of_int x))

#if JSC_ARCH_BIG_ENDIAN

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

let unsafe_get_int64_be_trunc = unsafe_read_int64_int
let unsafe_get_int64_le_trunc = unsafe_read_int64_int_swap
let unsafe_set_int64_be       = unsafe_write_int64_int
let unsafe_set_int64_le       = unsafe_write_int64_int_swap

let unsafe_get_int64_t_be  = unsafe_read_int64
let unsafe_get_int64_t_le  = unsafe_read_int64_swap
let unsafe_set_int64_t_be  = unsafe_write_int64
let unsafe_set_int64_t_le  = unsafe_write_int64_swap

#else

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

let unsafe_get_int64_be_trunc = unsafe_read_int64_int_swap
let unsafe_get_int64_le_trunc = unsafe_read_int64_int
let unsafe_set_int64_be       = unsafe_write_int64_int_swap
let unsafe_set_int64_le       = unsafe_write_int64_int

let unsafe_get_int64_t_be  = unsafe_read_int64_swap
let unsafe_get_int64_t_le  = unsafe_read_int64
let unsafe_set_int64_t_be  = unsafe_write_int64_swap
let unsafe_set_int64_t_le  = unsafe_write_int64

#endif

let int64_conv_error () =
  failwith "unsafe_read_int64: value cannot be represented unboxed!"
;;

let uint64_conv_error () =
  failwith "unsafe_read_uint64: value cannot be represented unboxed!"
;;

#if JSC_ARCH_SIXTYFOUR

let int64_to_int_exn n =
  if n >= -0x4000_0000_0000_0000L && n < 0x4000_0000_0000_0000L then
    int64_to_int n
  else
    int64_conv_error ()
;;

let uint64_to_int_exn n =
  if n >= 0L && n < 0x4000_0000_0000_0000L then
    int64_to_int n
  else
    uint64_conv_error ()
;;

#else

let int64_to_int_exn n =
  if n >= -0x0000_0000_4000_0000L && n < 0x0000_0000_4000_0000L then
    int64_to_int n
  else
    int64_conv_error ()
;;

let uint64_to_int_exn n =
  if n >= 0L && n < 0x0000_0000_4000_0000L then
    int64_to_int n
  else
    uint64_conv_error ()
;;

#endif

let unsafe_get_int64_be_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_be t ~pos)
let unsafe_get_int64_le_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_le t ~pos)

let unsafe_get_uint64_be_exn t ~pos = uint64_to_int_exn (unsafe_get_int64_t_be t ~pos)
let unsafe_get_uint64_le_exn t ~pos = uint64_to_int_exn (unsafe_get_int64_t_le t ~pos)

let unsafe_set_uint64_be = unsafe_set_int64_be
let unsafe_set_uint64_le = unsafe_set_int64_le

let%bench_module "unsafe_get_*int64_* don't allocate intermediate boxes" =
  (module struct
    let t = init 8 ~f:Char.of_int_exn
    let%bench "int64_be_exn"    = unsafe_get_int64_be_exn  t ~pos:0
    let%bench "int64_le_exn"    = unsafe_get_int64_le_exn  t ~pos:0
    let%bench "uint64_be_exn"   = unsafe_get_uint64_be_exn t ~pos:0
    let%bench "uint64_le_exn"   = unsafe_get_uint64_le_exn t ~pos:0
  end)
;;

(* Type annotations on the [t]s are important here: in order for the compiler to generate
   optimized code, it needs to know the fully instantiated type of the bigarray. This is
   because the type of the bigarray encodes the element kind and the layout of the
   bigarray. Without the annotation the compiler generates a C call to the generic access
   functions.
*)
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

let%test_module "binary accessors" = (module struct

  let buf = create 256

  let test_accessor ~buf to_str ~fget ~fset vals =
    Core_list.foldi ~init:true vals ~f:(fun i passing x ->
      fset buf ~pos:0 x;
      let y = fget buf ~pos:0 in
      if x <> y then eprintf "Value %d: expected %s, got %s\n" i (to_str x) (to_str y);
      x = y && passing)
  ;;

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_int16_le
    ~fset:unsafe_set_int16_le
    [-32768; -1; 0; 1; 32767]

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_uint16_le
    ~fset:unsafe_set_uint16_le
    [0; 1; 65535]

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_int16_be
    ~fset:unsafe_set_int16_be
    [-32768; -1; 0; 1; 32767]

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_uint16_be
    ~fset:unsafe_set_uint16_be
    [0; 1; 65535]


#if JSC_ARCH_SIXTYFOUR

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_int32_le
    ~fset:unsafe_set_int32_le
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_int32_be
    ~fset:unsafe_set_int32_be
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_int64_le_exn
    ~fset:unsafe_set_int64_le
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  let%test _ = test_accessor ~buf Int.to_string
    ~fget:unsafe_get_int64_be_exn
    ~fset:unsafe_set_int64_be
    [Int64.to_int_exn (-0x4000_0000_0000_0000L);
     Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L;
     Int64.to_int_exn 0x3fff_ffff_ffff_ffffL]

  let%test _ =
    List.for_all
      [ unsafe_get_uint64_be_exn, unsafe_set_uint64_be
      ; unsafe_get_uint64_le_exn, unsafe_set_uint64_le
      ]
      ~f:(fun (fget, fset) ->
        test_accessor ~buf Int.to_string
          ~fget ~fset
          ([ 0L
           ; 1L
           ; 0xffff_ffffL
           ; 0x3fff_ffff_ffff_ffffL ]
          |> List.map ~f:Int64.to_int_exn))

  let%test_unit _ =
    List.iter
      [ "\x40\x00\x00\x00\x00\x00\x00\x00"
      ; "\x80\x00\x00\x00\x00\x00\x00\x00"
      ; "\xA0\x00\x00\x00\x00\x00\x00\x00"
      ; "\xF0\x00\x00\x00\x00\x00\x00\x00"
      ; "\x4F\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
      ; "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
      ] ~f:(fun string ->
    assert (Exn.does_raise (fun () -> unsafe_get_uint64_be_exn ~pos:0 (of_string string)));
    assert (Exn.does_raise (fun () -> unsafe_get_uint64_le_exn ~pos:0
                                        (of_string (String.rev string)))))

#endif (* ARCH_SIXTYFOUR *)

  let%test _ = test_accessor ~buf Int64.to_string
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

  let%test _ = test_accessor ~buf Int64.to_string
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

  let%test _ = test_accessor ~buf Int64.to_string
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

     Seen on a data stream the constant 0x4000_0000_0000_0000 is supposed to represent a
     64-bit positive integer (2^62).

     Whilst this bit pattern does fit in an OCaml [int] on a 64-bit machine, it is the
     representation of a negative number ([Int.min_value]), and in particular is not the
     representation of 2^62.  It is thus suitable for this test. *)
  let test_int64 get_exn get_trunc set_t double_check_set =
    List.iter
      [ 0x4000_0000_0000_0000L
      ; Int64.succ (Int64.of_int Int.max_value)
      ; Int64.pred (Int64.of_int Int.min_value)
      ; Int64.min_value
      ; Int64.max_value
      ; Int64.succ Int64.min_value
      ; Int64.pred Int64.max_value
      ]
      ~f:(fun too_big ->
        let trunc = int64_to_int too_big in
        try
          set_t buf ~pos:0 too_big;
          [%test_result: int64] ~expect:too_big (double_check_set buf ~pos:0);
          let test_get name got =
            [%test_pred: string Or_error.t] is_error ~message:name
              (Or_error.map ~f:(fun i -> sprintf "%d = 0x%x" i i) got)
          in
          let got_exn = Or_error.try_with (fun () -> get_exn buf ~pos:0) in
          test_get "get_exn" got_exn;
          [%test_result: int] ~message:"get_trunc" ~expect:trunc
            (get_trunc buf ~pos:0)
        with e ->
          failwiths "test_int64"
            ( sprintf "too_big = %LdL = 0x%LxL" too_big too_big
            , sprintf "trunc = %d = 0x%x" trunc trunc
            , e
            )
            [%sexp_of: string * string * exn])
let%test_unit "unsafe_get_int64_le" =
    test_int64
      unsafe_get_int64_le_exn
      unsafe_get_int64_le_trunc
      unsafe_set_int64_t_le
      unsafe_get_int64_t_le
  let%test_unit "unsafe_get_int64_be" =
    test_int64
      unsafe_get_int64_be_exn
      unsafe_get_int64_be_trunc
      unsafe_set_int64_t_be
      unsafe_get_int64_t_be
end)

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

