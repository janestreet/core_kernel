open! Core
open Iobuf_blit
open Iobuf_hexdump
include Iobuf_basic_intf.Definitions
include Iobuf_type

module type Bound = Bound with type ('d, 'w, 'l) iobuf := ('d, 'w, 'l) t

let read_only t = t
let read_only__local t = t
let no_seek t = t
let no_seek__local t = t

module Lo_bound = struct
  let[@cold] stale t iobuf =
    fail iobuf "Iobuf.Lo_bound.restore got stale snapshot" t [%sexp_of: int]
  ;;

  type t = int [@@deriving compare ~localize, sexp_of] (* lo *)

  let window t = t.lo

  let restore t iobuf =
    if t < iobuf.lo_min || t > iobuf.hi then stale t iobuf;
    iobuf.lo <- t
  ;;

  let limit t = t.lo_min
end

module Hi_bound = struct
  let[@cold] stale t iobuf =
    fail iobuf "Iobuf.Hi_bound.restore got stale snapshot" t [%sexp_of: int]
  ;;

  type t = int [@@deriving compare ~localize, sexp_of] (* hi *)

  let window t = t.hi

  let restore t iobuf =
    if t > iobuf.hi_max || t < iobuf.lo then stale t iobuf;
    iobuf.hi <- t
  ;;

  let limit t = t.hi_max
end

let length_lo t = t.lo - t.lo_min
let length_hi t = t.hi_max - t.hi
let is_empty t = t.lo = t.hi
let rewind t = t.lo <- t.lo_min

let reset t =
  t.lo <- t.lo_min;
  t.hi <- t.hi_max
;;

let flip_lo t =
  t.hi <- t.lo;
  t.lo <- t.lo_min
;;

let[@cold] bounded_flip_lo_stale t lo_min =
  fail t "Iobuf.bounded_flip_lo got stale snapshot" lo_min [%sexp_of: Lo_bound.t]
;;

let bounded_flip_lo t lo_min =
  if lo_min < t.lo_min || lo_min > t.lo
  then bounded_flip_lo_stale t lo_min
  else (
    t.hi <- t.lo;
    t.lo <- lo_min)
;;

let flip_hi t =
  t.lo <- t.hi;
  t.hi <- t.hi_max
;;

let[@cold] bounded_flip_hi_stale t hi_max =
  fail t "Iobuf.bounded_flip_hi got stale snapshot" hi_max [%sexp_of: Hi_bound.t]
;;

let bounded_flip_hi t hi_max =
  if hi_max > t.hi_max || hi_max < t.hi
  then bounded_flip_hi_stale t hi_max
  else (
    t.lo <- t.hi;
    t.hi <- hi_max)
;;

let capacity t = t.hi_max - t.lo_min

let invariant t =
  try
    Repr.Fields.Direct.iter
      t
      ~buf:(fun _ _ _ -> ())
      ~lo_min:(fun _ _ lo_min ->
        assert (lo_min >= 0);
        assert (lo_min = t.hi_max - capacity t))
      ~hi_max:(fun _ _ hi_max ->
        assert (hi_max >= t.lo);
        assert (hi_max = t.lo_min + capacity t))
      ~lo:(fun _ _ lo ->
        assert (lo >= t.lo_min);
        assert (lo <= t.hi))
      ~hi:(fun _ _ hi ->
        assert (hi >= t.lo);
        assert (hi <= t.hi_max))
  with
  | e -> fail t "Iobuf.invariant failed" e [%sexp_of: exn]
;;

let[@inline] unsafe_sub_shared ~pos ~len t = exclave_
  let lo = t.lo + pos in
  let hi = lo + len in
  { buf = t.buf; lo_min = lo; lo; hi; hi_max = hi }
;;

let sub_shared__local ?(pos = 0) ?len t = exclave_
  let len =
    match len with
    | None -> length t - pos
    | Some len -> len
  in
  check_range t ~pos ~len;
  unsafe_sub_shared ~pos ~len t
;;

let unsafe_sub_shared ~pos ~len t = exclave_
  if unsafe_is_safe then sub_shared__local ~pos ~len t else unsafe_sub_shared ~pos ~len t
;;

let sub_shared ?pos ?len t = globalize_shared (sub_shared__local ?pos ?len t) [@nontail]

let copy t =
  of_bigstring (Bigstring.sub ([%template buf [@mode local]] t) ~pos:t.lo ~len:(length t))
;;

let clone { buf; lo_min; lo; hi; hi_max } =
  let buf =
    Modes.At_locality.wrap (Bigstring.copy (Modes.At_locality.unwrap_local buf))
  in
  { buf; lo_min; lo; hi; hi_max }
;;

let narrow_lo t = t.lo_min <- t.lo
let narrow_hi t = t.hi_max <- t.hi

let narrow t =
  narrow_lo t;
  narrow_hi t
;;

let unsafe_resize t ~len = t.hi <- t.lo + len

let resize t ~len =
  if len < 0 then bad_range t ~len ~pos:0;
  let hi = t.lo + len in
  if hi > t.hi_max then bad_range t ~len ~pos:0;
  t.hi <- hi
[@@inline always]
;;

let unsafe_resize = if unsafe_is_safe then resize else unsafe_resize

let protect_window_bounds_and_buffer t ~f =
  let lo = t.lo in
  let hi = t.hi in
  let lo_min = t.lo_min in
  let hi_max = t.hi_max in
  let bstr = buf t in
  (* also mutable *)
  try
    t.lo_min <- lo;
    t.hi_max <- hi;
    let result = f t in
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    raise exn
;;

let protect_window_bounds_and_buffer__local t ~f = exclave_
  let lo = t.lo in
  let hi = t.hi in
  let lo_min = t.lo_min in
  let hi_max = t.hi_max in
  let bstr = buf t in
  (* also mutable *)
  try
    t.lo_min <- lo;
    t.hi_max <- hi;
    let result = f t in
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    raise exn
;;

let protect_window_bounds_and_buffer_1 t x ~f =
  let lo = t.lo in
  let hi = t.hi in
  let lo_min = t.lo_min in
  let hi_max = t.hi_max in
  let bstr = buf t in
  (* also mutable *)
  try
    t.lo_min <- lo;
    t.hi_max <- hi;
    let result = f t x in
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    raise exn
;;

let protect_window_bounds_and_buffer_2 t x y ~f =
  let lo = t.lo in
  let hi = t.hi in
  let lo_min = t.lo_min in
  let hi_max = t.hi_max in
  let bstr = buf t in
  (* also mutable *)
  try
    t.lo_min <- lo;
    t.hi_max <- hi;
    let result = f t x y in
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    raise exn
;;

let protect_window_bounds_and_buffer_3 t x y z ~f =
  let lo = t.lo in
  let hi = t.hi in
  let lo_min = t.lo_min in
  let hi_max = t.hi_max in
  let bstr = buf t in
  (* also mutable *)
  try
    t.lo_min <- lo;
    t.hi_max <- hi;
    let result = f t x y z in
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    t.lo_min <- lo_min;
    t.hi_max <- hi_max;
    if not (phys_equal bstr (buf t)) then t.buf <- Modes.At_locality.wrap bstr;
    raise exn
;;

let empty = create ~len:0

let%template of_string s =
  (of_bigstring [@alloc a]) (Bigstring.of_string s) [@exclave_if_stack a]
[@@alloc a = (stack, heap)]
;;

let of_bytes s = of_bigstring (Bigstring.of_bytes s)

let to_stringlike ~(convert : ?pos:int -> ?len:int -> Bigstring.t @ local -> 'a) ?len t
  : 'a
  =
  let len =
    match len with
    | Some len ->
      check_range t ~pos:0 ~len;
      len
    | None -> length t
  in
  convert ([%template buf [@mode local]] t) ~pos:t.lo ~len [@nontail]
;;

let to_string ?len t = to_stringlike ~convert:Bigstring.to_string ?len t
let to_bytes ?len t = to_stringlike ~convert:Bigstring.to_bytes ?len t

let compact t =
  let len = t.hi - t.lo in
  Bigstring.blit
    ~src:([%template buf [@mode local]] t)
    ~src_pos:t.lo
    ~len
    ~dst:([%template buf [@mode local]] t)
    ~dst_pos:t.lo_min;
  t.lo <- t.lo_min + len;
  t.hi <- t.hi_max
;;

let[@cold] bounded_compact_stale t lo_min hi_max =
  fail
    t
    "Iobuf.bounded_compact got stale snapshot"
    (lo_min, hi_max)
    [%sexp_of: Lo_bound.t * Hi_bound.t]
;;

let bounded_compact t lo_min hi_max =
  let len = t.hi - t.lo in
  if hi_max > t.hi_max || hi_max < lo_min + len || lo_min < t.lo_min
  then bounded_compact_stale t lo_min hi_max
  else (
    Bigstring.blit
      ~src:([%template buf [@mode local]] t)
      ~src_pos:t.lo
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:lo_min;
    t.lo <- lo_min + len;
    t.hi <- hi_max)
;;

let transfer ~src ~dst =
  reset dst;
  Blit_fill.blito ~src ~dst ();
  flip_lo dst
;;

let to_string_hum = Hexdump.to_string_hum

let memcmp a b =
  let len = length a in
  let c = Int.compare len (length b) in
  if c <> 0
  then c
  else
    Bigstring.memcmp
      ~pos1:a.lo
      ([%template buf [@mode local]] a)
      ~pos2:b.lo
      ([%template buf [@mode local]] b)
      ~len [@nontail]
;;

let memset t ~pos ~len c =
  Bigstring.memset
    ~pos:(buf_pos_exn t ~pos ~len)
    ~len
    ([%template buf [@mode local]] t)
    c [@nontail]
;;

let unsafe_memset t ~pos ~len c =
  Bigstring.unsafe_memset
    ~pos:(buf_pos_exn t ~pos ~len)
    ~len
    ([%template buf [@mode local]] t)
    c [@nontail]
;;

let zero t = memset t ~pos:0 ~len:(length t) '\000'

let concat bufs =
  let total_length = ref 0 in
  let n = Array.length bufs in
  for i = 0 to n - 1 do
    (* This can overflow in 32 bit and javascript, so safe blits below. *)
    total_length := !total_length + length (Array.unsafe_get bufs i)
  done;
  let t = create ~len:!total_length in
  let pos = ref 0 in
  for i = 0 to n - 1 do
    let src = Array.unsafe_get bufs i in
    let len = length src in
    Blit.blit ~src ~dst:t ~src_pos:0 ~dst_pos:!pos ~len;
    pos := !pos + len
  done;
  t
;;

let contains t ~substring =
  Bigstring.unsafe_memmem
    ~haystack:([%template buf [@mode local]] t)
    ~haystack_pos:t.lo
    ~haystack_len:(length t)
    ~needle:substring
    ~needle_pos:0
    ~needle_len:(Bigstring.length substring)
  >= 0
;;
