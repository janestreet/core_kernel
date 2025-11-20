[@@@ocaml.flambda_o3]

open! Core
open Iobuf_numeric
open Iobuf_safe
open Iobuf_type
module IR = Int_repr
include Iobuf_unsafe_intf.Definitions

module Consume = struct
  (* copy of Consume with pos replaced by an unsafe version *)

  type 'loc src = 'loc Consume.src

  module To_bytes = struct
    include Consume.To_bytes

    let blit = unsafe_blit
  end

  module To_bigstring = struct
    include Consume.To_bigstring

    let blit = unsafe_blit
  end

  module To_string = Consume.To_string

  type ('a, 'd, 'w, 'l) t = ('a, 'd, 'w, 'l) Consume.t
  type ('a, 'd, 'w, 'l) t__local = ('a, 'd, 'w, 'l) Consume.t__local

  let uadv t n x =
    unsafe_advance t n;
    x
  [@@inline always]
  ;;

  let uadv_local t n (local_ x) =
    unsafe_advance t n;
    x
  [@@inline always]
  ;;

  let upos t len = unsafe_buf_pos t ~pos:0 ~len

  let tail_padded_fixed_string ~padding ~len t =
    uadv
      t
      len
      (Bigstring.get_tail_padded_fixed_string
         ([%template buf [@mode local]] t)
         ~pos:(upos t len)
         ~padding
         ~len
         ())
  ;;

  let head_padded_fixed_string ~padding ~len t =
    uadv
      t
      len
      (Bigstring.get_head_padded_fixed_string
         ([%template buf [@mode local]] t)
         ~pos:(upos t len)
         ~padding
         ~len
         ())
  ;;

  let bytes = Consume.bytes
  let string = Consume.string
  let bigstring = Consume.bigstring
  let byteso = Consume.byteso
  let stringo = Consume.stringo
  let bigstringo = Consume.bigstringo

  module Local = struct
    let tail_padded_fixed_string ~padding ~len t = exclave_
      uadv_local
        t
        len
        (Bigstring.get_tail_padded_fixed_string_local
           ([%template buf [@mode local]] t)
           ~pos:(upos t len)
           ~padding
           ~len
           ())
    ;;

    let head_padded_fixed_string ~padding ~len t = exclave_
      uadv_local
        t
        len
        (Bigstring.get_head_padded_fixed_string_local
           ([%template buf [@mode local]] t)
           ~pos:(upos t len)
           ~padding
           ~len
           ())
    ;;

    let bytes = Consume.Local.bytes
    let string = Consume.Local.string
    let byteso = Consume.Local.byteso
    let stringo = Consume.Local.stringo

    open Bigstring

    let len = 8

    let[@inline always] int64_t_be t = exclave_
      uadv_local
        t
        len
        (Local.unsafe_get_int64_t_be ([%template buf [@mode local]] t) ~pos:(upos t len))
      [@nontail]
    ;;

    let[@inline always] int64_t_le t = exclave_
      uadv_local
        t
        len
        (Local.unsafe_get_int64_t_le ([%template buf [@mode local]] t) ~pos:(upos t len))
      [@nontail]
    ;;
  end

  open Bigstring

  let len = 1

  let[@inline always] char t =
    uadv t len (Bigstring.unsafe_get ([%template buf [@mode local]] t) (upos t len))
  ;;

  let[@inline always] uint8 t =
    uadv t len (unsafe_get_uint8 ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int8 t =
    uadv t len (unsafe_get_int8 ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let len = 2

  let[@inline always] int16_be t =
    uadv t len (unsafe_get_int16_be ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int16_le t =
    uadv t len (unsafe_get_int16_le ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] uint16_be t =
    uadv t len (unsafe_get_uint16_be ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] uint16_le t =
    uadv t len (unsafe_get_uint16_le ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let len = 4

  let[@inline always] int32_be t =
    uadv t len (unsafe_get_int32_be ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int32_t_be t =
    uadv t len (unsafe_get_int32_t_be ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int32_le t =
    uadv t len (unsafe_get_int32_le ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int32_t_le t =
    uadv t len (unsafe_get_int32_t_le ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] uint32_be t =
    uadv t len (unsafe_get_uint32_be ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] uint32_le t =
    uadv t len (unsafe_get_uint32_le ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let len = 8

  let[@inline always] int64_be_exn t =
    uadv
      t
      len
      (unsafe_get_int64_be_exn ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int64_le_exn t =
    uadv
      t
      len
      (unsafe_get_int64_le_exn ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] uint64_be_exn t =
    uadv
      t
      len
      (unsafe_get_uint64_be_exn ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] uint64_le_exn t =
    uadv
      t
      len
      (unsafe_get_uint64_le_exn ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int64_t_be t =
    uadv t len (unsafe_get_int64_t_be ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int64_t_le t =
    uadv t len (unsafe_get_int64_t_le ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int64_be_trunc t =
    uadv
      t
      len
      (unsafe_get_int64_be_trunc ([%template buf [@mode local]] t) ~pos:(upos t len))
  ;;

  let[@inline always] int64_le_trunc t =
    uadv
      t
      len
      (unsafe_get_int64_le_trunc ([%template buf [@mode local]] t) ~pos:(upos t len))
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
  type ('a, 'd, 'w, 'l) t = ('a, 'd, 'w, 'l) Fill.t
  type ('a, 'd, 'w, 'l) t__local = ('a, 'd, 'w, 'l) Fill.t__local

  (* copy with unsafe pos *)

  let upos t len = unsafe_buf_pos t ~pos:0 ~len
  let uadv t n = unsafe_advance t n

  let tail_padded_fixed_string ~padding ~len t src =
    Bigstring.set_tail_padded_fixed_string
      ~padding
      ~len
      ([%template buf [@mode local]] t)
      ~pos:(upos t len)
      src;
    uadv t len
  ;;

  let head_padded_fixed_string ~padding ~len t src =
    Bigstring.set_head_padded_fixed_string
      ~padding
      ~len
      ([%template buf [@mode local]] t)
      ~pos:(upos t len)
      src;
    uadv t len
  ;;

  let bytes ~str_pos ~len t src =
    Bigstring.From_bytes.blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:(upos t len);
    uadv t len
  ;;

  let string ~str_pos ~len t src =
    Bigstring.From_string.blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:(upos t len);
    uadv t len
  ;;

  let bigstring ~str_pos ~len t src =
    Bigstring.blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:(upos t len);
    uadv t len
  ;;

  let byteso ?(str_pos = 0) ?len t src =
    bytes
      t
      src
      ~str_pos
      ~len:
        (match len with
         | None -> Bytes.length src - str_pos
         | Some len -> len)
  ;;

  let stringo ?(str_pos = 0) ?len t src =
    string
      t
      src
      ~str_pos
      ~len:
        (match len with
         | None -> String.length src - str_pos
         | Some len -> len)
  ;;

  let bigstringo ?(str_pos = 0) ?len t src =
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
    Bigstring.unsafe_set ([%template buf [@mode local]] t) (upos t len) c;
    uadv t len
  ;;

  let len = 2

  let[@inline always] int16_be_trunc t i =
    unsafe_set_int16_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int16_le_trunc t i =
    unsafe_set_int16_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] uint16_be_trunc t i =
    unsafe_set_uint16_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] uint16_le_trunc t i =
    unsafe_set_uint16_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let len = 4

  let[@inline always] int32_be_trunc t i =
    unsafe_set_int32_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int32_t_be t i =
    unsafe_set_int32_t_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int32_le_trunc t i =
    unsafe_set_int32_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int32_t_le t i =
    unsafe_set_int32_t_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] uint32_be_trunc t i =
    unsafe_set_uint32_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] uint32_le_trunc t i =
    unsafe_set_uint32_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let len = 8

  let[@inline always] int64_be t i =
    unsafe_set_int64_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int64_le t i =
    unsafe_set_int64_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] uint64_be_trunc t i =
    unsafe_set_uint64_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] uint64_le_trunc t i =
    unsafe_set_uint64_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int64_t_be t i =
    unsafe_set_int64_t_be ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  let[@inline always] int64_t_le t i =
    unsafe_set_int64_t_le ([%template buf [@mode local]] t) i ~pos:(upos t len);
    uadv t len
  ;;

  (* Bigstring int8 accessors are slow C calls. Use the fast char primitive. *)
  let[@inline always] uint8_trunc t i = char t (Char.unsafe_of_int i)
  let[@inline always] int8_trunc t i = char t (Char.unsafe_of_int i)
  let decimal t i = uadv t (Itoa.unsafe_poke_decimal t ~pos:0 i)
  let padded_decimal ~len t i = uadv t (Itoa.unsafe_poke_padded_decimal t ~pos:0 ~len i)

  let date_string_iso8601_extended t date =
    Date_string.unsafe_poke_iso8601_extended t ~pos:0 date;
    uadv t Date_string.len_iso8601_extended
  ;;

  module Int_repr = struct
    let[@inline always] uint8 t i = char t (Char.unsafe_of_int (IR.Uint8.to_base_int i))
    let[@inline always] uint16_be t i = uint16_be_trunc t (IR.Uint16.to_base_int i)
    let[@inline always] uint16_le t i = uint16_le_trunc t (IR.Uint16.to_base_int i)
    let[@inline always] uint32_be t i = int32_t_be t (IR.Uint32.to_base_int32_trunc i)
    let[@inline always] uint32_le t i = int32_t_le t (IR.Uint32.to_base_int32_trunc i)
    let[@inline always] uint64_be t i = int64_t_be t (IR.Uint64.to_base_int64_trunc i)
    let[@inline always] uint64_le t i = int64_t_le t (IR.Uint64.to_base_int64_trunc i)
    let[@inline always] int8 t i = char t (Char.unsafe_of_int (IR.Int8.to_base_int i))
    let[@inline always] int16_be t i = int16_be_trunc t (IR.Int16.to_base_int i)
    let[@inline always] int16_le t i = int16_le_trunc t (IR.Int16.to_base_int i)
    let[@inline always] int32_be t i = int32_t_be t (IR.Int32.to_base_int32 i)
    let[@inline always] int32_le t i = int32_t_le t (IR.Int32.to_base_int32 i)
    let[@inline always] int64_be t i = int64_t_be t i
    let[@inline always] int64_le t i = int64_t_le t i
  end
end

module Peek = struct
  type ('seek, 'loc) src = ('seek, 'loc) Peek.src

  module To_bytes = struct
    include Peek.To_bytes

    let blit = unsafe_blit
  end

  module To_bigstring = struct
    include Peek.To_bigstring

    let blit = unsafe_blit
  end

  module To_string = Peek.To_string

  type ('a, 'd, 'w, 'l) t = ('a, 'd, 'w, 'l) Peek.t
  type ('a, 'd, 'w, 'l) t__local = ('a, 'd, 'w, 'l) Peek.t__local

  let upos = unsafe_buf_pos

  let tail_padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_tail_padded_fixed_string
      ([%template buf [@mode local]] t)
      ~padding
      ~len
      ~pos:(upos t ~len ~pos)
      () [@nontail]
  ;;

  let head_padded_fixed_string ~padding ~len t ~pos =
    Bigstring.get_head_padded_fixed_string
      ([%template buf [@mode local]] t)
      ~padding
      ~len
      ~pos:(upos t ~len ~pos)
      () [@nontail]
  ;;

  let bytes ~str_pos ~len t ~pos =
    let dst = Bytes.create (len + str_pos) in
    Bigstring.To_bytes.unsafe_blit
      ~src:([%template buf [@mode local]] t)
      ~src_pos:(upos t ~len ~pos)
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
    Bigstring.unsafe_blit
      ~src:([%template buf [@mode local]] t)
      ~src_pos:(upos t ~len ~pos)
      ~len
      ~dst
      ~dst_pos:str_pos;
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

  let index_or_neg t ?(pos = 0) ?(len = length t - pos) c =
    let pos = unsafe_buf_pos t ~pos ~len in
    let idx = Bigstring.unsafe_find ~pos ~len ([%template buf [@mode local]] t) c in
    if idx < 0 then -1 else idx - t.lo
  ;;

  let rindex_or_neg t ?(pos = 0) ?(len = length t - pos) c =
    let pos = unsafe_buf_pos t ~pos ~len in
    let idx = Bigstring.unsafe_rfind ~pos ~len ([%template buf [@mode local]] t) c in
    if idx < 0 then -1 else idx - t.lo
  ;;

  let index__local t ?pos ?len c =
    let index = index_or_neg t ?pos ?len c in
    exclave_ if index < 0 then None else Some index
  ;;

  let rindex__local t ?pos ?len c =
    let index = rindex_or_neg t ?pos ?len c in
    exclave_ if index < 0 then None else Some index
  ;;

  module Local = struct
    let tail_padded_fixed_string ~padding ~len t ~pos = exclave_
      Bigstring.get_tail_padded_fixed_string_local
        ([%template buf [@mode local]] t)
        ~padding
        ~len
        ~pos:(upos t ~len ~pos)
        ()
    ;;

    let head_padded_fixed_string ~padding ~len t ~pos = exclave_
      Bigstring.get_head_padded_fixed_string_local
        ([%template buf [@mode local]] t)
        ~padding
        ~len
        ~pos:(upos t ~len ~pos)
        ()
    ;;

    let bytes ~str_pos ~len t ~pos = exclave_
      let dst = Bytes.create_local (len + str_pos) in
      Bigstring.To_bytes.unsafe_blit
        ~src:([%template buf [@mode local]] t)
        ~src_pos:(upos t ~len ~pos)
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
      Local.unsafe_get_int64_t_be
        ([%template buf [@mode local]] t)
        ~pos:(upos t ~len ~pos) [@nontail]
    ;;

    let[@inline always] int64_t_le t ~pos = exclave_
      Local.unsafe_get_int64_t_le
        ([%template buf [@mode local]] t)
        ~pos:(upos t ~len ~pos) [@nontail]
    ;;
  end

  open Bigstring

  let len = 1

  let[@inline always] char t ~pos =
    Bigstring.unsafe_get ([%template buf [@mode local]] t) (upos t ~len ~pos)
  ;;

  let[@inline always] uint8 t ~pos =
    unsafe_get_uint8 ([%template buf [@mode local]] t) ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int8 t ~pos =
    unsafe_get_int8 ([%template buf [@mode local]] t) ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let len = 2

  let[@inline always] int16_be t ~pos =
    unsafe_get_int16_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int16_le t ~pos =
    unsafe_get_int16_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] uint16_be t ~pos =
    unsafe_get_uint16_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] uint16_le t ~pos =
    unsafe_get_uint16_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let len = 4

  let[@inline always] int32_be t ~pos =
    unsafe_get_int32_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int32_t_be t ~pos =
    unsafe_get_int32_t_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int32_le t ~pos =
    unsafe_get_int32_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int32_t_le t ~pos =
    unsafe_get_int32_t_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] uint32_be t ~pos =
    unsafe_get_uint32_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] uint32_le t ~pos =
    unsafe_get_uint32_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let len = 8

  let[@inline always] int64_be_exn t ~pos =
    unsafe_get_int64_be_exn
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int64_le_exn t ~pos =
    unsafe_get_int64_le_exn
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] uint64_be_exn t ~pos =
    unsafe_get_uint64_be_exn
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] uint64_le_exn t ~pos =
    unsafe_get_uint64_le_exn
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int64_t_be t ~pos =
    unsafe_get_int64_t_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int64_t_le t ~pos =
    unsafe_get_int64_t_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int64_be_trunc t ~pos =
    unsafe_get_int64_be_trunc
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
  ;;

  let[@inline always] int64_le_trunc t ~pos =
    unsafe_get_int64_le_trunc
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos) [@nontail]
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
  type ('a, 'd, 'w, 'l) t = ('a, 'd, 'w, 'l) Poke.t
  type ('a, 'd, 'w, 'l) t__local = ('a, 'd, 'w, 'l) Poke.t__local

  let upos = unsafe_buf_pos

  let tail_padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_tail_padded_fixed_string
      ~padding
      ~len
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      src [@nontail]
  ;;

  let head_padded_fixed_string ~padding ~len t ~pos src =
    Bigstring.set_head_padded_fixed_string
      ~padding
      ~len
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      src [@nontail]
  ;;

  let bytes ~str_pos ~len t ~pos src =
    let blit =
      if unsafe_is_safe
      then Bigstring.From_bytes.blit
      else Bigstring.From_bytes.unsafe_blit
    in
    blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:(upos t ~len ~pos) [@nontail]
  ;;

  let string ~str_pos ~len t ~pos src =
    let blit =
      if unsafe_is_safe
      then Bigstring.From_string.blit
      else Bigstring.From_string.unsafe_blit
    in
    blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:(upos t ~len ~pos) [@nontail]
  ;;

  let bigstring ~str_pos ~len t ~pos src =
    let blit = if unsafe_is_safe then Bigstring.blit else Bigstring.unsafe_blit in
    blit
      ~src
      ~src_pos:str_pos
      ~len
      ~dst:([%template buf [@mode local]] t)
      ~dst_pos:(upos t ~len ~pos) [@nontail]
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

  let[@inline always] char t ~pos c =
    Bigstring.unsafe_set ([%template buf [@mode local]] t) (upos t ~len ~pos) c
  ;;

  let[@inline always] uint8_trunc t ~pos i =
    unsafe_set_uint8
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int8_trunc t ~pos i =
    unsafe_set_int8 ([%template buf [@mode local]] t) ~pos:(upos t ~len ~pos) i [@nontail]
  ;;

  let len = 2

  let[@inline always] int16_be_trunc t ~pos i =
    unsafe_set_int16_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int16_le_trunc t ~pos i =
    unsafe_set_int16_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] uint16_be_trunc t ~pos i =
    unsafe_set_uint16_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] uint16_le_trunc t ~pos i =
    unsafe_set_uint16_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let len = 4

  let[@inline always] int32_be_trunc t ~pos i =
    unsafe_set_int32_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int32_t_be t ~pos i =
    unsafe_set_int32_t_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int32_le_trunc t ~pos i =
    unsafe_set_int32_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int32_t_le t ~pos i =
    unsafe_set_int32_t_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] uint32_be_trunc t ~pos i =
    unsafe_set_uint32_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] uint32_le_trunc t ~pos i =
    unsafe_set_uint32_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let len = 8

  let[@inline always] int64_be t ~pos i =
    unsafe_set_int64_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int64_le t ~pos i =
    unsafe_set_int64_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] uint64_be_trunc t ~pos i =
    unsafe_set_uint64_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] uint64_le_trunc t ~pos i =
    unsafe_set_uint64_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int64_t_be t ~pos i =
    unsafe_set_int64_t_be
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let[@inline always] int64_t_le t ~pos i =
    unsafe_set_int64_t_le
      ([%template buf [@mode local]] t)
      ~pos:(upos t ~len ~pos)
      i [@nontail]
  ;;

  let decimal = Itoa.unsafe_poke_decimal
  let padded_decimal = Itoa.unsafe_poke_padded_decimal
  let date_string_iso8601_extended = Date_string.unsafe_poke_iso8601_extended

  module Int_repr = struct
    let[@inline always] uint8 t ~pos i = uint8_trunc t ~pos (IR.Uint8.to_base_int i)

    let[@inline always] uint16_be t ~pos i =
      int16_be_trunc t ~pos (IR.Uint16.to_base_int i)
    ;;

    let[@inline always] uint16_le t ~pos i =
      int16_le_trunc t ~pos (IR.Uint16.to_base_int i)
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
