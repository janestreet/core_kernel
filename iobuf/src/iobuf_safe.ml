open! Core
open Iobuf_numeric
open Iobuf_type
module IR = Int_repr
include Iobuf_safe_intf.Definitions

module Consume = struct
  type src = (read, seek) t

  module To (Dst : sig
      type t [@@deriving sexp_of]

      val create : len:int -> t
      val length : local_ t -> int
      val get : t -> int -> char
      val set : t -> int -> char -> unit
      val unsafe_blit : (Repr.t, t) Core.Blit.blit
    end) =
  struct
    include Base_for_tests.Test_blit.Make_distinct_and_test (Char_elt) (T_src) (Dst)

    let unsafe_blit ~src ~dst ~dst_pos ~len =
      let blit = if unsafe_is_safe then blit else unsafe_blit in
      blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
      unsafe_advance src len
    ;;

    let blit ~src ~dst ~dst_pos ~len =
      blit ~src ~src_pos:0 ~dst ~dst_pos ~len;
      unsafe_advance src len
    ;;

    let blito ~src ?(src_len = length src) ~dst ?dst_pos () =
      blito ~src ~src_pos:0 ~src_len ~dst ?dst_pos ();
      unsafe_advance src src_len
    ;;

    let sub src ~len =
      let dst = sub src ~pos:0 ~len in
      unsafe_advance src len;
      dst
    ;;

    let subo ?len src =
      let len =
        match len with
        | None -> length src
        | Some len -> len
      in
      let dst = subo ~pos:0 ~len src in
      unsafe_advance src len;
      dst
    ;;
  end

  module To_bytes = To (Bytes_dst)
  module To_bigstring = To (Bigstring_dst)

  module To_string = struct
    let sub src ~len =
      let dst = String_dst.sub src ~len ~pos:0 in
      unsafe_advance src len;
      dst
    ;;

    let subo ?len src =
      let len =
        match len with
        | None -> length src
        | Some len -> len
      in
      let dst = String_dst.subo ~pos:0 ~len src in
      unsafe_advance src len;
      dst
    ;;
  end

  type nonrec ('a, 'd, 'w) t__local = local_ ('d, seek) t -> local_ 'a
    constraint 'd = [> read ]

  type nonrec ('a, 'd, 'w) t = local_ ('d, seek) t -> 'a constraint 'd = [> read ]

  let uadv t n x =
    unsafe_advance t n;
    x
  [@@inline always]
  ;;

  let uadv_local t n x =
    unsafe_advance t n;
    x
  [@@inline always]
  ;;

  let pos t len = buf_pos_exn t ~pos:0 ~len

  let tail_padded_fixed_string ~padding ~len t =
    uadv
      t
      len
      (Bigstring.get_tail_padded_fixed_string t.buf ~pos:(pos t len) ~padding ~len ())
  ;;

  let head_padded_fixed_string ~padding ~len t =
    uadv
      t
      len
      (Bigstring.get_head_padded_fixed_string t.buf ~pos:(pos t len) ~padding ~len ())
  ;;

  let bytes ~str_pos ~len t =
    let dst = Bytes.create (len + str_pos) in
    To_bytes.blit ~src:t ~dst ~len ~dst_pos:str_pos;
    dst
  ;;

  let string ~str_pos ~len t =
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t)
  ;;

  let bigstring ~str_pos ~len t =
    let dst = Bigstring.create (len + str_pos) in
    To_bigstring.blit ~src:t ~dst ~len ~dst_pos:str_pos;
    dst
  ;;

  let byteso ?(str_pos = 0) ?len t =
    bytes
      t
      ~str_pos
      ~len:
        (match len with
         | None -> length t
         | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t =
    string
      t
      ~str_pos
      ~len:
        (match len with
         | None -> length t
         | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t =
    bigstring
      t
      ~str_pos
      ~len:
        (match len with
         | None -> length t
         | Some len -> len)
  ;;

  module Local = struct
    let tail_padded_fixed_string ~padding ~len t = exclave_
      uadv_local
        t
        len
        (Bigstring.get_tail_padded_fixed_string_local
           t.buf
           ~pos:(pos t len)
           ~padding
           ~len
           ())
    ;;

    let head_padded_fixed_string ~padding ~len t = exclave_
      uadv_local
        t
        len
        (Bigstring.get_head_padded_fixed_string_local
           t.buf
           ~pos:(pos t len)
           ~padding
           ~len
           ())
    ;;

    let bytes ~str_pos ~len t = exclave_
      let dst = Bytes.create_local (len + str_pos) in
      To_bytes.blit ~src:t ~dst ~len ~dst_pos:str_pos;
      dst
    ;;

    let string ~str_pos ~len t = exclave_
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t)
    ;;

    let byteso ?(str_pos = 0) ?len t = exclave_
      bytes
        t
        ~str_pos
        ~len:
          (match len with
           | None -> length t
           | Some len -> len)
    ;;

    let stringo ?(str_pos = 0) ?len t = exclave_
      string
        t
        ~str_pos
        ~len:
          (match len with
           | None -> length t
           | Some len -> len)
    ;;

    open Bigstring

    let len = 8

    let[@inline always] int64_t_be t = exclave_
      uadv_local t len (Local.unsafe_get_int64_t_be t.buf ~pos:(pos t len)) [@nontail]
    ;;

    let[@inline always] int64_t_le t = exclave_
      uadv_local t len (Local.unsafe_get_int64_t_le t.buf ~pos:(pos t len)) [@nontail]
    ;;
  end

  open Bigstring

  let len = 1
  let[@inline always] char t = uadv t len (Bigstring.unsafe_get t.buf (pos t len))
  let[@inline always] uint8 t = uadv t len (unsafe_get_uint8 t.buf ~pos:(pos t len))
  let[@inline always] int8 t = uadv t len (unsafe_get_int8 t.buf ~pos:(pos t len))
  let len = 2
  let[@inline always] int16_be t = uadv t len (unsafe_get_int16_be t.buf ~pos:(pos t len))
  let[@inline always] int16_le t = uadv t len (unsafe_get_int16_le t.buf ~pos:(pos t len))

  let[@inline always] uint16_be t =
    uadv t len (unsafe_get_uint16_be t.buf ~pos:(pos t len))
  ;;

  let[@inline always] uint16_le t =
    uadv t len (unsafe_get_uint16_le t.buf ~pos:(pos t len))
  ;;

  let len = 4
  let[@inline always] int32_be t = uadv t len (unsafe_get_int32_be t.buf ~pos:(pos t len))

  let[@inline always] int32_t_be t =
    uadv t len (unsafe_get_int32_t_be t.buf ~pos:(pos t len))
  ;;

  let[@inline always] int32_le t = uadv t len (unsafe_get_int32_le t.buf ~pos:(pos t len))

  let[@inline always] int32_t_le t =
    uadv t len (unsafe_get_int32_t_le t.buf ~pos:(pos t len))
  ;;

  let[@inline always] uint32_be t =
    uadv t len (unsafe_get_uint32_be t.buf ~pos:(pos t len))
  ;;

  let[@inline always] uint32_le t =
    uadv t len (unsafe_get_uint32_le t.buf ~pos:(pos t len))
  ;;

  let len = 8

  let[@inline always] int64_be_exn t =
    uadv t len (unsafe_get_int64_be_exn t.buf ~pos:(pos t len))
  ;;

  let[@inline always] int64_le_exn t =
    uadv t len (unsafe_get_int64_le_exn t.buf ~pos:(pos t len))
  ;;

  let[@inline always] uint64_be_exn t =
    uadv t len (unsafe_get_uint64_be_exn t.buf ~pos:(pos t len))
  ;;

  let[@inline always] uint64_le_exn t =
    uadv t len (unsafe_get_uint64_le_exn t.buf ~pos:(pos t len))
  ;;

  let[@inline always] int64_t_be t =
    uadv t len (unsafe_get_int64_t_be t.buf ~pos:(pos t len))
  ;;

  let[@inline always] int64_t_le t =
    uadv t len (unsafe_get_int64_t_le t.buf ~pos:(pos t len))
  ;;

  let[@inline always] int64_be_trunc t =
    uadv t len (unsafe_get_int64_be_trunc t.buf ~pos:(pos t len))
  ;;

  let[@inline always] int64_le_trunc t =
    uadv t len (unsafe_get_int64_le_trunc t.buf ~pos:(pos t len))
  ;;

  module Int_repr = struct
    let[@inline always] uint8 t = IR.Uint8.of_base_int_trunc (uint8 t)
    let[@inline always] uint16_be t = IR.Uint16.of_base_int_trunc (uint16_be t)
    let[@inline always] uint16_le t = IR.Uint16.of_base_int_trunc (uint16_le t)
    let[@inline always] uint32_be t = IR.Uint32.of_base_int32_trunc (int32_t_be t)
    let[@inline always] uint32_le t = IR.Uint32.of_base_int32_trunc (int32_t_le t)
    let[@inline always] uint64_be t = IR.Uint64.of_base_int64_trunc (int64_t_be t)
    let[@inline always] uint64_le t = IR.Uint64.of_base_int64_trunc (int64_t_le t)
    let[@inline always] int8 t = IR.Int8.of_base_int_trunc (int8 t)
    let[@inline always] int16_be t = IR.Int16.of_base_int_trunc (int16_be t)
    let[@inline always] int16_le t = IR.Int16.of_base_int_trunc (int16_le t)
    let[@inline always] int32_be t = IR.Int32.of_base_int32 (int32_t_be t)
    let[@inline always] int32_le t = IR.Int32.of_base_int32 (int32_t_le t)
    let[@inline always] int64_be t = int64_t_be t
    let[@inline always] int64_le t = int64_t_le t
  end
end

module Fill = struct
  type nonrec ('a, 'd, 'w) t__local = local_ (read_write, seek) t -> local_ 'a -> unit
    constraint 'd = [> read ]

  type nonrec ('a, 'd, 'w) t = local_ (read_write, seek) t -> 'a -> unit
    constraint 'd = [> read ]

  let[@inline] pos t len = buf_pos_exn t ~pos:0 ~len
  let uadv = unsafe_advance

  let tail_padded_fixed_string ~padding ~len t (local_ src) =
    Bigstring.set_tail_padded_fixed_string ~padding ~len t.buf ~pos:(pos t len) src;
    uadv t len
  ;;

  let head_padded_fixed_string ~padding ~len t (local_ src) =
    Bigstring.set_head_padded_fixed_string ~padding ~len t.buf ~pos:(pos t len) src;
    uadv t len
  ;;

  let bytes ~str_pos ~len t (local_ src) =
    Bigstring.From_bytes.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    uadv t len
  ;;

  let string ~str_pos ~len t (local_ src) =
    Bigstring.From_string.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    uadv t len
  ;;

  let bigstring ~str_pos ~len t (local_ src) =
    Bigstring.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(pos t len);
    uadv t len
  ;;

  let byteso ?(str_pos = 0) ?len t (local_ src) =
    bytes
      t
      src
      ~str_pos
      ~len:
        (match len with
         | None -> Bytes.length src - str_pos
         | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t (local_ src) =
    string
      t
      src
      ~str_pos
      ~len:
        (match len with
         | None -> String.length src - str_pos
         | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t (local_ src) =
    bigstring
      t
      src
      ~str_pos
      ~len:
        (match len with
         | None -> Bigstring.length src - str_pos
         | Some len -> len)
  ;;

  open Bigstring

  let len = 1

  let[@inline always] char t c =
    Bigstring.unsafe_set t.buf (pos t len) c;
    uadv t len
  ;;

  let[@inline always] uint8_trunc t i =
    unsafe_set_uint8 t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int8_trunc t i =
    unsafe_set_int8 t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let len = 2

  let[@inline always] int16_be_trunc t i =
    unsafe_set_int16_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int16_le_trunc t i =
    unsafe_set_int16_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] uint16_be_trunc t i =
    unsafe_set_uint16_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] uint16_le_trunc t i =
    unsafe_set_uint16_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let len = 4

  let[@inline always] int32_be_trunc t i =
    unsafe_set_int32_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int32_t_be t i =
    unsafe_set_int32_t_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int32_le_trunc t i =
    unsafe_set_int32_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int32_t_le t i =
    unsafe_set_int32_t_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] uint32_be_trunc t i =
    unsafe_set_uint32_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] uint32_le_trunc t i =
    unsafe_set_uint32_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let len = 8

  let[@inline always] int64_be t i =
    unsafe_set_int64_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int64_le t i =
    unsafe_set_int64_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] uint64_be_trunc t i =
    unsafe_set_uint64_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] uint64_le_trunc t i =
    unsafe_set_uint64_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int64_t_be t (local_ i) =
    unsafe_set_int64_t_be t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let[@inline always] int64_t_le t (local_ i) =
    unsafe_set_int64_t_le t.buf i ~pos:(pos t len);
    uadv t len
  ;;

  let decimal t i = uadv t (Itoa.poke_decimal t ~pos:0 i)
  let padded_decimal ~len t i = uadv t (Itoa.poke_padded_decimal t ~pos:0 ~len i)

  let date_string_iso8601_extended t date =
    Date_string.poke_iso8601_extended t ~pos:0 date;
    uadv t Date_string.len_iso8601_extended
  ;;

  module Int_repr = struct
    let[@inline always] uint8 t i = uint8_trunc t (IR.Uint8.to_base_int i)
    let[@inline always] uint16_be t i = uint16_be_trunc t (IR.Uint16.to_base_int i)
    let[@inline always] uint16_le t i = uint16_le_trunc t (IR.Uint16.to_base_int i)
    let[@inline always] uint32_be t i = int32_t_be t (IR.Uint32.to_base_int32_trunc i)
    let[@inline always] uint32_le t i = int32_t_le t (IR.Uint32.to_base_int32_trunc i)
    let[@inline always] uint64_be t i = int64_t_be t (IR.Uint64.to_base_int64_trunc i)
    let[@inline always] uint64_le t i = int64_t_le t (IR.Uint64.to_base_int64_trunc i)
    let[@inline always] int8 t i = int8_trunc t (IR.Int8.to_base_int i)
    let[@inline always] int16_be t i = int16_be_trunc t (IR.Int16.to_base_int i)
    let[@inline always] int16_le t i = int16_le_trunc t (IR.Int16.to_base_int i)
    let[@inline always] int32_be t i = int32_t_be t (IR.Int32.to_base_int32 i)
    let[@inline always] int32_le t i = int32_t_le t (IR.Int32.to_base_int32 i)
    let[@inline always] int64_be t i = int64_t_be t i
    let[@inline always] int64_le t i = int64_t_le t i
  end
end

module Peek = struct
  type 'seek src = (read, 'seek) t

  module To_bytes =
    Base_for_tests.Test_blit.Make_distinct_and_test (Char_elt) (T_src) (Bytes_dst)

  module To_bigstring =
    Base_for_tests.Test_blit.Make_distinct_and_test (Char_elt) (T_src) (Bigstring_dst)

  module To_string = String_dst

  type nonrec ('a, 'd, 'w) t__local = local_ ('d, 'w) t -> pos:int -> local_ 'a
    constraint 'd = [> read ]

  type nonrec ('a, 'd, 'w) t = local_ ('d, 'w) t -> pos:int -> 'a
    constraint 'd = [> read ]

  let spos = buf_pos_exn (* "safe position" *)

  let tail_padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_tail_padded_fixed_string t.buf ~padding ~len ~pos:(spos t ~len ~pos) ()
  ;;

  let head_padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_head_padded_fixed_string t.buf ~padding ~len ~pos:(spos t ~len ~pos) ()
  ;;

  let bytes ~str_pos ~len t ~pos =
    let dst = Bytes.create (len + str_pos) in
    Bigstring.To_bytes.blit
      ~src:t.buf
      ~src_pos:(spos t ~len ~pos)
      ~len
      ~dst
      ~dst_pos:str_pos;
    dst
  ;;

  let string ~str_pos ~len t ~pos =
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t ~pos)
  ;;

  let bigstring ~str_pos ~len t ~pos =
    let dst = Bigstring.create (len + str_pos) in
    Bigstring.blit ~src:t.buf ~src_pos:(spos t ~len ~pos) ~len ~dst ~dst_pos:str_pos;
    dst
  ;;

  let byteso ?(str_pos = 0) ?len t ~pos =
    bytes
      t
      ~pos
      ~str_pos
      ~len:
        (match len with
         | None -> length t - pos
         | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t ~pos =
    string
      t
      ~pos
      ~str_pos
      ~len:
        (match len with
         | None -> length t - pos
         | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t ~pos =
    bigstring
      t
      ~pos
      ~str_pos
      ~len:
        (match len with
         | None -> length t - pos
         | Some len -> len)
  ;;

  let index t ?(pos = 0) ?(len = length t - pos) c =
    let pos = spos t ~len ~pos in
    Option.map (Bigstring.find ~pos ~len c t.buf) ~f:(fun x -> x - t.lo) [@nontail]
  ;;

  let rindex t ?(pos = 0) ?(len = length t - pos) c =
    let pos = spos t ~len ~pos in
    Option.map (Bigstring.rfind ~pos ~len c t.buf) ~f:(fun x -> x - t.lo) [@nontail]
  ;;

  module Local = struct
    let tail_padded_fixed_string ~padding ~len t ~pos = exclave_
      Bigstring.get_tail_padded_fixed_string_local
        t.buf
        ~padding
        ~len
        ~pos:(spos t ~len ~pos)
        ()
    ;;

    let head_padded_fixed_string ~padding ~len t ~pos = exclave_
      Bigstring.get_head_padded_fixed_string_local
        t.buf
        ~padding
        ~len
        ~pos:(spos t ~len ~pos)
        ()
    ;;

    let bytes ~str_pos ~len t ~pos = exclave_
      let dst = Bytes.create_local (len + str_pos) in
      Bigstring.To_bytes.blit
        ~src:t.buf
        ~src_pos:(spos t ~len ~pos)
        ~len
        ~dst
        ~dst_pos:str_pos;
      dst
    ;;

    let string ~str_pos ~len t ~pos = exclave_
      Bytes.unsafe_to_string
        ~no_mutation_while_string_reachable:(bytes ~str_pos ~len t ~pos)
    ;;

    let byteso ?(str_pos = 0) ?len t ~pos = exclave_
      bytes
        t
        ~pos
        ~str_pos
        ~len:
          (match len with
           | None -> length t - pos
           | Some len -> len)
    ;;

    let stringo ?(str_pos = 0) ?len t ~pos = exclave_
      string
        t
        ~pos
        ~str_pos
        ~len:
          (match len with
           | None -> length t - pos
           | Some len -> len)
    ;;

    open Bigstring

    let len = 8

    let[@inline always] int64_t_be t ~pos = exclave_
      Local.unsafe_get_int64_t_be t.buf ~pos:(spos t ~len ~pos) [@nontail]
    ;;

    let[@inline always] int64_t_le t ~pos = exclave_
      Local.unsafe_get_int64_t_le t.buf ~pos:(spos t ~len ~pos) [@nontail]
    ;;
  end

  open Bigstring

  let[@inline always] char t ~pos = get_char t pos
  let len = 1
  let[@inline always] uint8 t ~pos = unsafe_get_uint8 t.buf ~pos:(spos t ~len ~pos)
  let[@inline always] int8 t ~pos = unsafe_get_int8 t.buf ~pos:(spos t ~len ~pos)
  let len = 2
  let[@inline always] int16_be t ~pos = unsafe_get_int16_be t.buf ~pos:(spos t ~len ~pos)
  let[@inline always] int16_le t ~pos = unsafe_get_int16_le t.buf ~pos:(spos t ~len ~pos)

  let[@inline always] uint16_be t ~pos =
    unsafe_get_uint16_be t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] uint16_le t ~pos =
    unsafe_get_uint16_le t.buf ~pos:(spos t ~len ~pos)
  ;;

  let len = 4
  let[@inline always] int32_be t ~pos = unsafe_get_int32_be t.buf ~pos:(spos t ~len ~pos)

  let[@inline always] int32_t_be t ~pos =
    unsafe_get_int32_t_be t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] int32_le t ~pos = unsafe_get_int32_le t.buf ~pos:(spos t ~len ~pos)

  let[@inline always] int32_t_le t ~pos =
    unsafe_get_int32_t_le t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] uint32_be t ~pos =
    unsafe_get_uint32_be t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] uint32_le t ~pos =
    unsafe_get_uint32_le t.buf ~pos:(spos t ~len ~pos)
  ;;

  let len = 8

  let[@inline always] int64_be_exn t ~pos =
    unsafe_get_int64_be_exn t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] int64_le_exn t ~pos =
    unsafe_get_int64_le_exn t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] uint64_be_exn t ~pos =
    unsafe_get_uint64_be_exn t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] uint64_le_exn t ~pos =
    unsafe_get_uint64_le_exn t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] int64_t_be t ~pos =
    unsafe_get_int64_t_be t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] int64_t_le t ~pos =
    unsafe_get_int64_t_le t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] int64_be_trunc t ~pos =
    unsafe_get_int64_be_trunc t.buf ~pos:(spos t ~len ~pos)
  ;;

  let[@inline always] int64_le_trunc t ~pos =
    unsafe_get_int64_le_trunc t.buf ~pos:(spos t ~len ~pos)
  ;;

  module Int_repr = struct
    let[@inline always] uint8 t ~pos = IR.Uint8.of_base_int_trunc (uint8 t ~pos)
    let[@inline always] uint16_be t ~pos = IR.Uint16.of_base_int_trunc (uint16_be t ~pos)
    let[@inline always] uint16_le t ~pos = IR.Uint16.of_base_int_trunc (uint16_le t ~pos)

    let[@inline always] uint32_be t ~pos =
      IR.Uint32.of_base_int32_trunc (int32_t_be t ~pos)
    ;;

    let[@inline always] uint32_le t ~pos =
      IR.Uint32.of_base_int32_trunc (int32_t_le t ~pos)
    ;;

    let[@inline always] uint64_be t ~pos =
      IR.Uint64.of_base_int64_trunc (int64_t_be t ~pos)
    ;;

    let[@inline always] uint64_le t ~pos =
      IR.Uint64.of_base_int64_trunc (int64_t_le t ~pos)
    ;;

    let[@inline always] int8 t ~pos = IR.Int8.of_base_int_trunc (int8 t ~pos)
    let[@inline always] int16_be t ~pos = IR.Int16.of_base_int_trunc (int16_be t ~pos)
    let[@inline always] int16_le t ~pos = IR.Int16.of_base_int_trunc (int16_le t ~pos)
    let[@inline always] int32_be t ~pos = IR.Int32.of_base_int32 (int32_t_be t ~pos)
    let[@inline always] int32_le t ~pos = IR.Int32.of_base_int32 (int32_t_le t ~pos)
    let[@inline always] int64_be t ~pos = int64_t_be t ~pos
    let[@inline always] int64_le t ~pos = int64_t_le t ~pos
  end
end

module Poke = struct
  type nonrec ('a, 'd, 'w) t__local =
    local_ (read_write, 'w) t -> pos:int -> local_ 'a -> unit
    constraint 'd = [> read ]

  type nonrec ('a, 'd, 'w) t = local_ (read_write, 'w) t -> pos:int -> 'a -> unit
    constraint 'd = [> read ]

  let spos = buf_pos_exn (* "safe position" *)

  let tail_padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_tail_padded_fixed_string ~padding ~len t.buf ~pos:(spos t ~len ~pos) src
  ;;

  let head_padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_head_padded_fixed_string ~padding ~len t.buf ~pos:(spos t ~len ~pos) src
  ;;

  let bytes ~str_pos ~len t ~pos src =
    Bigstring.From_bytes.blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:t.buf
      ~dst_pos:(spos t ~len ~pos)
  ;;

  let string ~str_pos ~len t ~pos src =
    Bigstring.From_string.blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:t.buf
      ~dst_pos:(spos t ~len ~pos)
  ;;

  let bigstring ~str_pos ~len t ~pos src =
    Bigstring.blit ~src ~src_pos:str_pos ~len ~dst:t.buf ~dst_pos:(spos t ~len ~pos)
  ;;

  let byteso ?(str_pos = 0) ?len t ~pos src =
    bytes
      t
      ~str_pos
      ~pos
      src
      ~len:
        (match len with
         | None -> Bytes.length src - str_pos
         | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t ~pos src =
    string
      t
      ~str_pos
      ~pos
      src
      ~len:
        (match len with
         | None -> String.length src - str_pos
         | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t ~pos src =
    bigstring
      t
      ~str_pos
      ~pos
      src
      ~len:
        (match len with
         | None -> Bigstring.length src - str_pos
         | Some len -> len)
  ;;

  open Bigstring

  let len = 1
  let[@inline always] char t ~pos c = set_char t pos c

  let[@inline always] uint8_trunc t ~pos i =
    unsafe_set_uint8 t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int8_trunc t ~pos i =
    unsafe_set_int8 t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let len = 2

  let[@inline always] int16_be_trunc t ~pos i =
    unsafe_set_int16_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int16_le_trunc t ~pos i =
    unsafe_set_int16_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] uint16_be_trunc t ~pos i =
    unsafe_set_uint16_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] uint16_le_trunc t ~pos i =
    unsafe_set_uint16_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let len = 4

  let[@inline always] int32_be_trunc t ~pos i =
    unsafe_set_int32_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int32_t_be t ~pos i =
    unsafe_set_int32_t_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int32_le_trunc t ~pos i =
    unsafe_set_int32_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int32_t_le t ~pos i =
    unsafe_set_int32_t_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] uint32_be_trunc t ~pos i =
    unsafe_set_uint32_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] uint32_le_trunc t ~pos i =
    unsafe_set_uint32_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let len = 8

  let[@inline always] int64_be t ~pos i =
    unsafe_set_int64_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int64_le t ~pos i =
    unsafe_set_int64_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] uint64_be_trunc t ~pos i =
    unsafe_set_uint64_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] uint64_le_trunc t ~pos i =
    unsafe_set_uint64_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int64_t_be t ~pos i =
    unsafe_set_int64_t_be t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let[@inline always] int64_t_le t ~pos i =
    unsafe_set_int64_t_le t.buf ~pos:(spos t ~len ~pos) i
  ;;

  let decimal = Itoa.poke_decimal
  let padded_decimal = Itoa.poke_padded_decimal
  let date_string_iso8601_extended = Date_string.poke_iso8601_extended

  module Int_repr = struct
    let[@inline always] uint8 t ~pos i = uint8_trunc t ~pos (IR.Uint8.to_base_int i)

    let[@inline always] uint16_be t ~pos i =
      uint16_be_trunc t ~pos (IR.Uint16.to_base_int i)
    ;;

    let[@inline always] uint16_le t ~pos i =
      uint16_le_trunc t ~pos (IR.Uint16.to_base_int i)
    ;;

    let[@inline always] uint32_be t ~pos i =
      int32_t_be t ~pos (IR.Uint32.to_base_int32_trunc i)
    ;;

    let[@inline always] uint32_le t ~pos i =
      int32_t_le t ~pos (IR.Uint32.to_base_int32_trunc i)
    ;;

    let[@inline always] uint64_be t ~pos i =
      int64_t_be t ~pos (IR.Uint64.to_base_int64_trunc i)
    ;;

    let[@inline always] uint64_le t ~pos i =
      int64_t_le t ~pos (IR.Uint64.to_base_int64_trunc i)
    ;;

    let[@inline always] int8 t ~pos i = int8_trunc t ~pos (IR.Int8.to_base_int i)
    let[@inline always] int16_be t ~pos i = int16_be_trunc t ~pos (IR.Int16.to_base_int i)
    let[@inline always] int16_le t ~pos i = int16_le_trunc t ~pos (IR.Int16.to_base_int i)
    let[@inline always] int32_be t ~pos i = int32_t_be t ~pos (IR.Int32.to_base_int32 i)
    let[@inline always] int32_le t ~pos i = int32_t_le t ~pos (IR.Int32.to_base_int32 i)
    let[@inline always] int64_be t ~pos i = int64_t_be t ~pos i
    let[@inline always] int64_le t ~pos i = int64_t_le t ~pos i
  end
end
