open! Core
open Iobuf_safe
open Iobuf_type
include Iobuf_bin_io_intf.Definitions

let read_bin_prot read t ~pos =
  let buf_pos = unsafe_buf_pos t ~pos ~len:0 in
  let pos_ref = ref buf_pos in
  let a = read (buf t) ~pos_ref in
  let len = !pos_ref - buf_pos in
  check_range t ~pos ~len;
  a, len
;;

let consume (reader : 'a Bin_prot.Type_class.reader) t =
  let a, len = read_bin_prot reader.read t ~pos:0 in
  unsafe_advance t len;
  a
;;

let poke_size writer t ~pos a =
  let len = writer.Bin_prot.Type_class.size a in
  let buf_pos = buf_pos_exn t ~pos ~len in
  let stop_pos = writer.Bin_prot.Type_class.write (buf t) ~pos:buf_pos a in
  if stop_pos - buf_pos = len
  then len
  else
    fail
      t
      "Iobuf.write_bin_prot got unexpected number of bytes written (Bin_prot bug: \
       Type_class.write disagrees with .size)"
      (`size_len len, `buf_pos buf_pos, `write_stop_pos stop_pos)
      [%sexp_of: [ `size_len of int ] * [ `buf_pos of int ] * [ `write_stop_pos of int ]]
;;

let poke_size__local sizer writer t ~pos (local_ a) =
  let len = sizer a in
  let buf_pos = buf_pos_exn t ~pos ~len in
  let stop_pos = writer (buf t) ~pos:buf_pos a in
  if stop_pos - buf_pos = len
  then len
  else
    fail
      t
      "Iobuf.write_bin_prot_local got unexpected number of bytes written (Bin_prot bug: \
       writer disagrees with sizer)"
      (`size_len len, `buf_pos buf_pos, `write_stop_pos stop_pos)
      [%sexp_of: [ `size_len of int ] * [ `buf_pos of int ] * [ `write_stop_pos of int ]]
;;

let fill writer t a = poke_size writer t ~pos:0 a |> unsafe_advance t

let fill__local sizer writer t (local_ a) =
  poke_size__local sizer writer t ~pos:0 a |> unsafe_advance t
;;

let peek read t ~pos = read_bin_prot read t ~pos |> fst
let poke writer t ~pos a = ignore (poke_size writer t ~pos a : int)
let header_bytes = 4

let consume_with_header t bin_prot_reader =
  let result =
    if length t < header_bytes
    then
      error
        "Iobuf.Bin_io.consume_with_header not enough data to read length"
        (globalize_shared t)
        [%sexp_of: (_, _, _) With_shallow_sexp.t]
    else (
      let mark = t.lo in
      let v_len = Consume.int32_be t in
      if v_len > length t
      then (
        t.lo <- mark;
        error
          "Iobuf.Bin_io.consume_with_header not enough data to read value"
          (v_len, globalize_shared t)
          [%sexp_of: int * (_, _, _) With_shallow_sexp.t])
      else Ok (consume bin_prot_reader t))
  in
  result
;;

let fill_with_header t writer v =
  let v_len = writer.Bin_prot.Type_class.size v in
  let need = v_len + header_bytes in
  let result =
    if need > length t
    then
      error
        "Iobuf.Bin_io.fill_with_header not enough space"
        (need, globalize_shared t)
        [%sexp_of: int * (_, _, _) With_shallow_sexp.t]
    else (
      Fill.int32_be_trunc t v_len;
      fill writer t v;
      Ok ())
  in
  result
;;

let fill_with_header__local t sizer writer (local_ v) =
  let v_len = sizer v in
  let need = v_len + header_bytes in
  let result =
    if need > length t
    then
      error
        "Iobuf.Bin_io.fill_with_header__local not enough space"
        (need, globalize_shared t)
        [%sexp_of: int * (_, _, _) With_shallow_sexp.t]
    else (
      Fill.int32_be_trunc t v_len;
      fill__local sizer writer t v;
      Ok ())
  in
  result
;;

let%template[@mode m = (global, local)] unsafe_poke_with_known_size write t ~pos ~size a =
  ignore (write (buf t) ~pos:(unsafe_buf_pos t ~pos ~len:size) a : int)
;;

let unsafe_consume = consume
let unsafe_fill = fill
let unsafe_fill__local = fill__local
let unsafe_peek = peek
let unsafe_poke = poke
let unsafe_poke_size = poke_size
