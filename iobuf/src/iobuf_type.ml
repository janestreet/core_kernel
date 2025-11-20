[@@@ocaml.flambda_o3]

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
  type 'loc t =
    { mutable buf : ((Bigstring.t, 'loc) Modes.At_locality.t[@sexp.opaque]) @@ local
        (* The data in [buf] is at indices [lo], [lo+1], ... [hi-1]. **)
    ; mutable lo_min : int
    ; mutable lo : int
    ; mutable hi : int
    ; mutable hi_max : int
    }
  [@@deriving fields ~getters ~direct_iterators:(iter, set_all_mutable_fields), sexp_of]

  let globalize_shared ({ buf; lo_min; lo; hi; hi_max } @ local) =
    let buf = Modes.At_locality.globalize_global buf in
    { buf; lo_min; lo; hi; hi_max }
  ;;

  let globalize_copied ({ buf; lo_min; lo; hi; hi_max } @ local) =
    let buf =
      Modes.At_locality.wrap (Bigstring.globalize (Modes.At_locality.unwrap_local buf))
    in
    { buf; lo_min; lo; hi; hi_max }
  ;;
end

type 'loc repr = 'loc Repr.t =
  { mutable buf : (Bigstring.t, 'loc) Modes.At_locality.t @@ local
  ; mutable lo_min : int
  ; mutable lo : int
  ; mutable hi : int
  ; mutable hi_max : int
  }

type (-'read_write, +'seek, 'loc) t = 'loc Repr.t [@@deriving sexp_of]

module With_shallow_sexp = struct
  type (_, _, 'loc) t = 'loc Repr.t [@@deriving sexp_of]

  let globalize = `deprecated
end

let globalize = `deprecated
let globalize_shared = Repr.globalize_shared
let globalize_copied = Repr.globalize_copied
let buf t = Modes.At_locality.unwrap_global t.buf
let%template[@mode local] buf t = exclave_ Modes.At_locality.unwrap_local t.buf

let[@cold] fail t message a sexp_of_a =
  (* Immediately convert the iobuf to sexp. Otherwise, the iobuf could be modified before
     conversion and printing. Since we plan to use iobufs for pooled network buffers in
     practice, this could be very confusing when debugging production systems. *)
  Error.raise
    (Error.create
       message
       (a, [%sexp_of: (_, _, _) t] (globalize_copied t))
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

let[@inline always] check_bigstring ~bstr ~pos ~len =
  let str_len = Bigstring.length bstr in
  if pos < 0
     || pos > str_len
     ||
     let max_len = str_len - pos in
     len < 0 || len > max_len
  then bad_range_bstr ~str_len ~pos ~len
;;

[%%template
let wrap = Modes.At_locality.wrap
let[@mode local] wrap = Modes.At_locality.wrap_local

[@@@alloc.default a @ m = (stack_local, heap_global)]
[@@@mode.default l = (m, global)]

let[@inline always] unsafe_of_bigstring_sub ~pos ~len (buf @ l) =
  (let lo = pos in
   let hi = pos + len in
   { buf = (wrap [@mode l]) buf; lo_min = lo; lo; hi; hi_max = hi })
  [@exclave_if_stack a]
;;

let of_bigstring_sub ~pos ~len (bstr @ l) =
  check_bigstring ~bstr ~pos ~len;
  (unsafe_of_bigstring_sub [@alloc a] [@mode l]) ~pos ~len bstr [@exclave_if_stack a]
;;

let of_bigstring ?pos ?len (buf @ l) =
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
          [%sexp
            "Iobuf.of_bigstring got invalid len"
            , (len : int)
            , ~~(max_len : int)
            , ~~(str_len : int)
            , ~~(pos : int)];
      len
  in
  (unsafe_of_bigstring_sub [@alloc a] [@mode l]) ~pos ~len buf [@exclave_if_stack a]
;;

let unsafe_of_bigstring_sub =
  if unsafe_is_safe
  then of_bigstring_sub [@alloc a] [@mode l]
  else unsafe_of_bigstring_sub [@alloc a] [@mode l]
;;]

let set_bounds_and_buffer_sub ~pos ~len ~src ~dst =
  check_range src ~pos ~len;
  let lo = src.lo + pos in
  let hi = lo + len in
  dst.lo_min <- lo;
  dst.lo <- lo;
  dst.hi <- hi;
  dst.hi_max <- hi;
  if not
       (phys_equal
          ([%template buf [@mode local]] dst)
          ([%template buf [@mode local]] src))
  then dst.buf <- Modes.At_locality.wrap (buf src)
[@@inline]
;;

let set_bounds_and_buffer ~src ~dst =
  dst.lo_min <- src.lo_min;
  dst.lo <- src.lo;
  dst.hi <- src.hi;
  dst.hi_max <- src.hi_max;
  if not
       (phys_equal
          ([%template buf [@mode local]] dst)
          ([%template buf [@mode local]] src))
  then dst.buf <- Modes.At_locality.wrap (buf src)
;;

let create ~len =
  if len < 0 then raise_s [%sexp "Iobuf.create got negative len", (len : int)];
  of_bigstring (Bigstring.create len)
;;

(* We used to do it like
   {v
let unsafe_with_range t ~pos f =
  f ([%template buf [@mode local]] t) ~pos:(t.lo + pos);
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
   v}
   but higher order functions don't get inlined, even in simple uses like advance.
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
  type t = char [@@deriving equal ~localize]

  let of_bool = function
    | true -> '0'
    | false -> '1'
  ;;
end

let[@inline] get_char t pos =
  Bigstring.unsafe_get ([%template buf [@mode local]] t) (buf_pos_exn t ~len:1 ~pos)
;;

let[@inline] set_char t pos c =
  Bigstring.unsafe_set ([%template buf [@mode local]] t) (buf_pos_exn t ~len:1 ~pos) c
;;

module T_src = struct
  type 'loc t = 'loc Repr.t [@@deriving sexp_of]

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
    blit
      ~src:([%template buf [@mode local]] src)
      ~src_pos:(unsafe_buf_pos src ~pos:src_pos ~len)
      ~dst
      ~dst_pos
      ~len [@nontail]
  ;;

  let create ~len = create len
  let get t i = get t i
end

module String_dst = struct
  let sub src ~pos ~len =
    Bigstring.To_string.sub
      ([%template buf [@mode local]] src)
      ~pos:(buf_pos_exn src ~pos ~len)
      ~len [@nontail]
  ;;

  let subo ?(pos = 0) ?len src =
    let len =
      match len with
      | None -> length src - pos
      | Some len -> len
    in
    Bigstring.To_string.subo
      ([%template buf [@mode local]] src)
      ~pos:(buf_pos_exn src ~pos ~len)
      ~len [@nontail]
  ;;
end

module Bigstring_dst = struct
  include Bigstring

  let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    let blit = if unsafe_is_safe then Bigstring.blit else Bigstring.unsafe_blit in
    blit
      ~src:([%template buf [@mode local]] src)
      ~src_pos:(unsafe_buf_pos src ~pos:src_pos ~len)
      ~dst
      ~dst_pos
      ~len [@nontail]
  ;;

  let create ~len = create len
end
