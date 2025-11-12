[@@@ocaml.flambda_o3]

open! Core
open Iobuf_type
include Iobuf_expert_intf.Definitions

let buf = buf
let%template[@mode local] buf = (buf [@mode local])
let hi_max t = t.hi_max
let hi t = t.hi
let lo t = t.lo
let lo_min t = t.lo_min
let set_buf t buf = t.buf <- Modes.At_locality.wrap buf
let set_hi_max t hi_max = t.hi_max <- hi_max
let set_hi t hi = t.hi <- hi
let set_lo t lo = t.lo <- lo
let set_lo_min t lo_min = t.lo_min <- lo_min
let buf_pos_exn = buf_pos_exn
let unsafe_buf_pos = unsafe_buf_pos

let to_bigstring_shared ?pos ?len t =
  let pos, len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
  in
  Bigstring.sub_shared (buf t) ~pos:(t.lo + pos) ~len
;;

let unsafe_reinitialize t ~lo_min ~lo ~hi ~hi_max bstr =
  (* avoid [caml_modify], if possible *)
  if not (phys_equal ([%template buf [@mode local]] t) bstr)
  then t.buf <- Modes.At_locality.wrap bstr;
  t.lo_min <- lo_min;
  t.lo <- lo;
  t.hi <- hi;
  t.hi_max <- hi_max
;;

let reinitialize t ~lo_min ~lo ~hi ~hi_max buf =
  if not
       (0 <= lo_min
        && lo_min <= lo
        && lo <= hi
        && hi <= hi_max
        && hi_max <= Bigstring.length buf)
  then
    raise_s
      [%message
        "Expert.reinitialize got invalid bounds"
          (lo_min : int)
          (lo : int)
          (hi : int)
          (hi_max : int)
          (Bigstring.length buf : int)];
  unsafe_reinitialize t ~lo_min ~lo ~hi ~hi_max buf
;;

let unsafe_reinitialize = if unsafe_is_safe then reinitialize else unsafe_reinitialize

let _remember_to_update_unsafe_reinitialize
  :  local_ (_, _, _) t -> buf:(Bigstring.t, global) Modes.At_locality.t -> lo_min:int
  -> lo:int -> hi:int -> hi_max:int -> unit
  =
  Repr.Fields.Direct.set_all_mutable_fields
;;

let reinitialize_of_bigstring t ~pos ~len buf =
  let str_len = Bigstring.length buf in
  if pos < 0 || pos > str_len
  then
    raise_s
      [%message
        "Expert.reinitialize_of_bigstring got invalid pos" (pos : int) (str_len : int)];
  let max_len = str_len - pos in
  if len < 0 || len > max_len
  then
    raise_s
      [%message
        "Expert.reinitialize_of_bigstring got invalid len" (len : int) (max_len : int)];
  let lo = pos in
  let hi = pos + len in
  unsafe_reinitialize t ~lo_min:lo ~lo ~hi ~hi_max:hi buf
;;

let set_bounds_and_buffer = set_bounds_and_buffer
let set_bounds_and_buffer_sub = set_bounds_and_buffer_sub

let protect_window t ~f =
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;

let protect_window_global_deprecated t ~f =
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;

let protect_window_1 t x ~f =
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t x in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;

let protect_window_2 t x y ~f =
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t x y in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;

let protect_window_1_global_deprecated t x ~f =
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t x in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;

let protect_window_2_global_deprecated t x y ~f =
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t x y in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;

let protect_window__local t ~f = exclave_
  let lo = t.lo in
  let hi = t.hi in
  try
    let result = f t in
    t.lo <- lo;
    t.hi <- hi;
    result
  with
  | exn ->
    t.lo <- lo;
    t.hi <- hi;
    raise exn
;;
