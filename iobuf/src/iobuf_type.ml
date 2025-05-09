open! Core
include Iobuf_type_intf.Definitions

[%%import "include.mlh"]
[%%if UNSAFE_IS_SAFE]

let unsafe_is_safe = true

[%%else]

let unsafe_is_safe = false

[%%endif]

module Repr = struct
  (* WHEN YOU CHANGE THIS, CHANGE iobuf_fields IN iobuf.h AS WELL!!! *)
  type t =
    { mutable buf :
        (Bigstring.t
        [@sexp.opaque] (* The data in [buf] is at indices [lo], [lo+1], ... [hi-1]. **))
    ; mutable lo_min : int
    ; mutable lo : int
    ; mutable hi : int
    ; mutable hi_max : int
    }
  [@@deriving
    fields ~getters ~direct_iterators:(iter, set_all_mutable_fields), globalize, sexp_of]
end

type repr = Repr.t =
  { mutable buf : Bigstring.t
  ; mutable lo_min : int
  ; mutable lo : int
  ; mutable hi : int
  ; mutable hi_max : int
  }

type (-'read_write, +'seek) t = Repr.t [@@deriving globalize, sexp_of]

module With_shallow_sexp = struct
  type (_, _) t = Repr.t [@@deriving globalize, sexp_of]
end

let globalize0 = Repr.globalize

let[@cold] fail t message a sexp_of_a =
  (* Immediately convert the iobuf to sexp.  Otherwise, the iobuf could be modified before
     conversion and printing.  Since we plan to use iobufs for pooled network buffers in
     practice, this could be very confusing when debugging production systems. *)
  Error.raise
    (Error.create
       message
       (a, [%sexp_of: (_, _) t] (globalize0 t))
       (Tuple.T2.sexp_of_t sexp_of_a Fn.id))
;;

let length t = t.hi - t.lo

(* We want [check_range] inlined, so we don't want a string constant in there. *)
let[@cold] bad_range ~pos ~len t =
  fail
    t
    "Iobuf got invalid range"
    (`pos pos, `len len)
    [%sexp_of: [ `pos of int ] * [ `len of int ]]
;;

let[@cold] bad_range_bstr ~pos ~len ~str_len =
  raise_s
    [%message "bad range relative to bigstring" (str_len : int) (pos : int) (len : int)]
;;

let check_range t ~pos ~len =
  if pos < 0 || len < 0 || len > length t - pos then bad_range ~pos ~len t
[@@inline always]
;;

let[@inline always] unsafe_bigstring_view ~pos ~len buf = exclave_
  let lo = pos in
  let hi = pos + len in
  { buf; lo_min = lo; lo; hi; hi_max = hi }
;;

let[@inline always] check_bigstring ~bstr ~pos ~len =
  let str_len = Bigstring.length bstr in
  if pos < 0
     || pos > str_len
     ||
     let max_len = str_len - pos in
     len < 0 || len > max_len
  then bad_range_bstr ~str_len ~pos ~len
;;

let bigstring_view ~pos ~len bstr = exclave_
  check_bigstring ~bstr ~pos ~len;
  unsafe_bigstring_view ~pos ~len bstr
;;

let of_bigstring__local ?pos ?len buf = exclave_
  let str_len = Bigstring.length buf in
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
      if pos < 0 || pos > str_len
      then
        raise_s
          [%sexp "Iobuf.of_bigstring got invalid pos", (pos : int), ~~(str_len : int)];
      pos
  in
  let len =
    match len with
    | None -> str_len - pos
    | Some len ->
      let max_len = str_len - pos in
      if len < 0 || len > max_len
      then
        raise_s
          [%sexp "Iobuf.of_bigstring got invalid pos", (len : int), ~~(max_len : int)];
      len
  in
  unsafe_bigstring_view ~pos ~len buf
;;

let unsafe_bigstring_view =
  if unsafe_is_safe then bigstring_view else unsafe_bigstring_view
;;

let of_bigstring ?pos ?len buf = globalize0 (of_bigstring__local ?pos ?len buf) [@nontail]

let set_bounds_and_buffer_sub ~pos ~len ~src ~dst =
  check_range src ~pos ~len;
  let lo = src.lo + pos in
  let hi = lo + len in
  dst.lo_min <- lo;
  dst.lo <- lo;
  dst.hi <- hi;
  dst.hi_max <- hi;
  if not (phys_equal dst.buf src.buf) then dst.buf <- src.buf
[@@inline]
;;

let set_bounds_and_buffer ~src ~dst =
  dst.lo_min <- src.lo_min;
  dst.lo <- src.lo;
  dst.hi <- src.hi;
  dst.hi_max <- src.hi_max;
  if not (phys_equal dst.buf src.buf) then dst.buf <- src.buf
;;

let create ~len =
  if len < 0 then raise_s [%sexp "Iobuf.create got negative len", (len : int)];
  of_bigstring (Bigstring.create len)
;;

(* We used to do it like {v

let unsafe_with_range t ~pos f =
  f t.buf ~pos:(t.lo + pos);
;;

let with_range t ~pos ~len f =
  check_range t ~pos ~len;
  unsafe_with_range t ~pos f;
;;

let inc_lo t amount = t.lo <- t.lo + amount

(** [unsafe_with_advance] and [unsafe_with_range] forego range checks for code that does
    macro range checks, like we want to do in [Parachute_fix.Std.Protocol].
    Esp. [Consume.Unsafe.int32_le] for unrolled character scanning. *)
let unsafe_with_advance t ~len f =
  let result = unsafe_with_range t ~pos:0 f in
  inc_lo t len;
  result;
;;

let with_advance t ~len f =
  check_range t ~pos:0 ~len;
  unsafe_with_advance t ~len f;
;;

(* pulled out and type-constrained for inlining *)
let ignore_range (_ : Bigstring.t) ~pos:(_ : int) = ()

let advance t len = with_advance t ~len ignore_range

   v} but higher order functions don't get inlined, even in simple uses like advance.
   Therefor, we stick to first order. *)

let[@inline always] unsafe_buf_pos t ~pos ~len:_ = t.lo + pos

let[@inline] buf_pos_exn t ~pos ~len =
  check_range t ~pos ~len;
  unsafe_buf_pos t ~pos ~len
;;

let unsafe_buf_pos = if unsafe_is_safe then buf_pos_exn else unsafe_buf_pos
let unsafe_advance t n = t.lo <- t.lo + n

let advance t len =
  check_range t ~len ~pos:0;
  unsafe_advance t len
[@@inline always]
;;

let unsafe_advance = if unsafe_is_safe then advance else unsafe_advance

module Char_elt = struct
  include Char

  let of_bool = function
    | true -> '0'
    | false -> '1'
  ;;
end

let[@inline] get_char t pos = Bigstring.unsafe_get t.buf (buf_pos_exn t ~len:1 ~pos)
let[@inline] set_char t pos c = Bigstring.unsafe_set t.buf (buf_pos_exn t ~len:1 ~pos) c

module T_src = struct
  type t = Repr.t [@@deriving sexp_of]

  let create = create
  let length = length
  let get t pos = get_char t pos
  let set t pos c = set_char t pos c
end

module Bytes_dst = struct
  include Bytes

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    let blit =
      if unsafe_is_safe then Bigstring.To_bytes.blit else Bigstring.To_bytes.unsafe_blit
    in
    blit ~src:src.buf ~src_pos:(unsafe_buf_pos src ~pos:src_pos ~len) ~dst ~dst_pos ~len
  ;;

  let create ~len = create len
  let get t i = get t i
end

module String_dst = struct
  let sub src ~pos ~len =
    Bigstring.To_string.sub src.buf ~pos:(buf_pos_exn src ~pos ~len) ~len
  ;;

  let subo ?(pos = 0) ?len src =
    let len =
      match len with
      | None -> length src - pos
      | Some len -> len
    in
    Bigstring.To_string.subo src.buf ~pos:(buf_pos_exn src ~pos ~len) ~len
  ;;
end

module Bigstring_dst = struct
  include Bigstring

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    let blit = if unsafe_is_safe then Bigstring.blit else Bigstring.unsafe_blit in
    blit ~src:src.buf ~src_pos:(unsafe_buf_pos src ~pos:src_pos ~len) ~dst ~dst_pos ~len
  ;;

  let create ~len = create len
end
