open! Core
open Iobuf_type_intf.Definitions

module Definitions = struct
  (** Collections of access functions. These abstract over [Iobuf.Consume], [Iobuf.Fill],
      [Iobuf.Peek], and [Iobuf.Poke].

      Make all labeled arguments mandatory in [string] and [bigstring] to avoid accidental
      allocation in, e.g., [Iobuf.Poke.string]. For convenience, [stringo] and
      [bigstringo] are available by analogy between [blit] and [blito].

      [_trunc] functions silently truncate values that don't fit. For example,
      [Iobuf.Unsafe.Poke.int8 128] effectively writes -128. *)
  module type Accessors_common = sig
    (** [('d, 'w, 'l) Iobuf.t] accessor function manipulating ['a], either writing it to
        the iobuf or reading it from the iobuf. *)

    type ('a, 'd, 'w, 'l) t constraint 'd = [> read ]
    type ('a, 'd, 'w, 'l) t__local constraint 'd = [> read ]

    val char : (char, 'd, 'w, 'l) t
  end

  module type Accessors_read = sig
    include Accessors_common

    val int8 : (int, 'd, 'w, 'l) t
    val int16_be : (int, 'd, 'w, 'l) t
    val int16_le : (int, 'd, 'w, 'l) t
    val int32_be : (int, 'd, 'w, 'l) t
    val int32_le : (int, 'd, 'w, 'l) t
    val int64_be_exn : (int, 'd, 'w, 'l) t
    val int64_le_exn : (int, 'd, 'w, 'l) t
    val int64_be_trunc : (int, 'd, 'w, 'l) t
    val int64_le_trunc : (int, 'd, 'w, 'l) t
    val uint8 : (int, 'd, 'w, 'l) t
    val uint16_be : (int, 'd, 'w, 'l) t
    val uint16_le : (int, 'd, 'w, 'l) t
    val uint32_be : (int, 'd, 'w, 'l) t
    val uint32_le : (int, 'd, 'w, 'l) t
    val uint64_be_exn : (int, 'd, 'w, 'l) t
    val uint64_le_exn : (int, 'd, 'w, 'l) t
    val int64_t_be : (Int64.t, 'd, 'w, 'l) t
    val int64_t_le : (Int64.t, 'd, 'w, 'l) t
    val head_padded_fixed_string : padding:char -> len:int -> (string, 'd, 'w, 'l) t
    val tail_padded_fixed_string : padding:char -> len:int -> (string, 'd, 'w, 'l) t
    val string : str_pos:int -> len:int -> (string, 'd, 'w, 'l) t
    val bytes : str_pos:int -> len:int -> (Bytes.t, 'd, 'w, 'l) t
    val bigstring : str_pos:int -> len:int -> (Bigstring.t, 'd, 'w, 'l) t
    val stringo : ?str_pos:local_ int -> ?len:local_ int -> local_ (string, 'd, 'w, 'l) t
    val byteso : ?str_pos:local_ int -> ?len:local_ int -> local_ (Bytes.t, 'd, 'w, 'l) t

    val bigstringo
      :  ?str_pos:local_ int
      -> ?len:local_ int
      -> local_ (Bigstring.t, 'd, 'w, 'l) t

    module Local : sig
      val int64_t_be : (Int64.t, 'd, 'w, 'l) t__local
      val int64_t_le : (Int64.t, 'd, 'w, 'l) t__local

      val head_padded_fixed_string
        :  padding:char
        -> len:int
        -> (string, 'd, 'w, 'l) t__local

      val tail_padded_fixed_string
        :  padding:char
        -> len:int
        -> (string, 'd, 'w, 'l) t__local

      val string : str_pos:int -> len:int -> (string, 'd, 'w, 'l) t__local
      val bytes : str_pos:int -> len:int -> (Bytes.t, 'd, 'w, 'l) t__local

      val stringo
        :  ?str_pos:local_ int
        -> ?len:local_ int
        -> local_ (string, 'd, 'w, 'l) t__local

      val byteso
        :  ?str_pos:local_ int
        -> ?len:local_ int
        -> local_ (Bytes.t, 'd, 'w, 'l) t__local
    end

    module Int_repr : sig
      val int8 : (Int_repr.Int8.t, 'd, 'w, 'l) t
      val int16_be : (Int_repr.Int16.t, 'd, 'w, 'l) t
      val int16_le : (Int_repr.Int16.t, 'd, 'w, 'l) t
      val int32_be : (Int_repr.Int32.t, 'd, 'w, 'l) t
      val int32_le : (Int_repr.Int32.t, 'd, 'w, 'l) t
      val int64_be : (Int_repr.Int64.t, 'd, 'w, 'l) t
      val int64_le : (Int_repr.Int64.t, 'd, 'w, 'l) t
      val uint8 : (Int_repr.Uint8.t, 'd, 'w, 'l) t
      val uint16_be : (Int_repr.Uint16.t, 'd, 'w, 'l) t
      val uint16_le : (Int_repr.Uint16.t, 'd, 'w, 'l) t
      val uint32_be : (Int_repr.Uint32.t, 'd, 'w, 'l) t
      val uint32_le : (Int_repr.Uint32.t, 'd, 'w, 'l) t
      val uint64_be : (Int_repr.Uint64.t, 'd, 'w, 'l) t
      val uint64_le : (Int_repr.Uint64.t, 'd, 'w, 'l) t
    end
  end

  module type Accessors_write = sig
    include Accessors_common

    val int8_trunc : (int, 'd, 'w, 'l) t
    val int16_be_trunc : (int, 'd, 'w, 'l) t
    val int16_le_trunc : (int, 'd, 'w, 'l) t
    val int32_be_trunc : (int, 'd, 'w, 'l) t
    val int32_le_trunc : (int, 'd, 'w, 'l) t
    val int64_be : (int, 'd, 'w, 'l) t
    val int64_le : (int, 'd, 'w, 'l) t
    val uint8_trunc : (int, 'd, 'w, 'l) t
    val uint16_be_trunc : (int, 'd, 'w, 'l) t
    val uint16_le_trunc : (int, 'd, 'w, 'l) t
    val uint32_be_trunc : (int, 'd, 'w, 'l) t
    val uint32_le_trunc : (int, 'd, 'w, 'l) t
    val uint64_be_trunc : (int, 'd, 'w, 'l) t
    val uint64_le_trunc : (int, 'd, 'w, 'l) t
    val int64_t_be : (Int64.t, 'd, 'w, 'l) t__local
    val int64_t_le : (Int64.t, 'd, 'w, 'l) t__local

    val head_padded_fixed_string
      :  padding:char
      -> len:int
      -> (string, 'd, 'w, 'l) t__local

    val tail_padded_fixed_string
      :  padding:char
      -> len:int
      -> (string, 'd, 'w, 'l) t__local

    val string : str_pos:int -> len:int -> (string, 'd, 'w, 'l) t__local
    val bytes : str_pos:int -> len:int -> (Bytes.t, 'd, 'w, 'l) t__local
    val bigstring : str_pos:int -> len:int -> (Bigstring.t, 'd, 'w, 'l) t__local

    val stringo
      :  ?str_pos:local_ int
      -> ?len:local_ int
      -> local_ (string, 'd, 'w, 'l) t__local

    val byteso
      :  ?str_pos:local_ int
      -> ?len:local_ int
      -> local_ (Bytes.t, 'd, 'w, 'l) t__local

    val bigstringo
      :  ?str_pos:local_ int
      -> ?len:local_ int
      -> local_ (Bigstring.t, 'd, 'w, 'l) t__local

    module Int_repr : sig
      val int8 : (Int_repr.Int8.t, 'd, 'w, 'l) t
      val int16_be : (Int_repr.Int16.t, 'd, 'w, 'l) t
      val int16_le : (Int_repr.Int16.t, 'd, 'w, 'l) t
      val int32_be : (Int_repr.Int32.t, 'd, 'w, 'l) t
      val int32_le : (Int_repr.Int32.t, 'd, 'w, 'l) t
      val int64_be : (Int_repr.Int64.t, 'd, 'w, 'l) t
      val int64_le : (Int_repr.Int64.t, 'd, 'w, 'l) t
      val uint8 : (Int_repr.Uint8.t, 'd, 'w, 'l) t
      val uint16_be : (Int_repr.Uint16.t, 'd, 'w, 'l) t
      val uint16_le : (Int_repr.Uint16.t, 'd, 'w, 'l) t
      val uint32_be : (Int_repr.Uint32.t, 'd, 'w, 'l) t
      val uint32_le : (Int_repr.Uint32.t, 'd, 'w, 'l) t
      val uint64_be : (Int_repr.Uint64.t, 'd, 'w, 'l) t
      val uint64_le : (Int_repr.Uint64.t, 'd, 'w, 'l) t
    end
  end

  (** The [src_pos] argument of {!Core.Blit.blit} doesn't make sense here. *)

  type ('src, 'dst) consuming_blit =
    src:local_ 'src -> dst:local_ 'dst -> dst_pos:int -> len:int -> unit

  type ('src, 'dst) consuming_blito =
    src:local_ 'src
    -> ?src_len:int (** Default is [Iobuf.length src]. *)
    -> dst:local_ 'dst
    -> ?dst_pos:int (** Default is [0]. *)
    -> unit
    -> unit

  module type Consuming_blit = sig
    type _ src
    type _ dst

    val blito : (_ src, _ dst) consuming_blito
    val blit : (_ src, _ dst) consuming_blit
    val unsafe_blit : (_ src, _ dst) consuming_blit

    (** [subo] defaults to using [Iobuf.length src] *)
    val subo : ?len:int -> local_ _ src -> _ dst

    val sub : local_ _ src -> len:int -> _ dst
  end

  module type Consume_safe = sig @@ portable
    type (_, _, _) iobuf

    (** [To_bytes.blito ~src ~dst ~dst_pos ~src_len ()] reads [src_len] bytes from [src],
        advancing [src]'s window accordingly, and writes them into [dst] starting at
        [dst_pos]. By default [dst_pos = 0] and [src_len = length src]. It is an error if
        [dst_pos] and [src_len] don't specify a valid region of [dst] or if
        [src_len > length src]. *)
    type 'loc src = (read, seek, 'loc) iobuf

    module To_bytes :
      Consuming_blit with type 'loc src := 'loc src with type _ dst := Bytes.t

    module To_bigstring :
      Consuming_blit with type 'loc src := 'loc src with type _ dst := Bigstring.t

    module To_string : sig
      (** [subo] defaults to using [Iobuf.length src]. *)
      val subo : ?len:int -> _ src -> string

      val sub : _ src -> len:int -> string
    end

    include
      Accessors_read
      with type ('a, 'r, 's, 'l) t = local_ (([> read ] as 'r), seek, 'l) iobuf -> 'a
      with type ('a, 'r, 's, 'l) t__local =
        local_ (([> read ] as 'r), seek, 'l) iobuf -> local_ 'a
  end

  module type Fill_safe = sig
    type (_, _, _) iobuf

    include
      Accessors_write
      with type ('a, 'd, 'w, 'l) t = local_ (read_write, seek, 'l) iobuf -> 'a -> unit
      with type ('a, 'd, 'w, 'l) t__local =
        local_ (read_write, seek, 'l) iobuf -> local_ 'a -> unit

    (** [decimal t int] is equivalent to [Iobuf.Fill.string t (Int.to_string int)], but
        with improved efficiency and no intermediate allocation.

        In other words: It fills the decimal representation of [int] to [t]. [t] is
        advanced by the number of characters written and no terminator is added. If
        sufficient space is not available, [decimal] will raise. *)
    val decimal : (int, _, _, _) t

    (** Same as [decimal t int], but padding to [len] with prefix '0's. *)
    val padded_decimal : len:int -> (int, _, _, _) t

    (** [date_string_iso8601_extended t date] is equivalent to
        [Iobuf.Fill.string t (Date.to_string date)], but with improved efficiency and no
        intermediate allocation.

        In other words: It fills the ISO 8601 extended representation (YYYY-MM-DD) of
        [date] to [t]. [t] is advanced by 10 characters and no terminator is added. If
        sufficient space is not available, [date] will raise. *)
    val date_string_iso8601_extended : (Date.t, _, _, _) t
  end

  module type Peek_common = sig @@ portable
    type ('rw, 'seek, 'loc) iobuf
    type ('seek, 'loc) src = (read, 'seek, 'loc) iobuf

    (** Similar to [Consume.To_*], but do not advance the buffer. *)

    module To_bytes :
      Blit.S1_phantom2_distinct
      with type (_, 'seek, 'loc) src := ('seek, 'loc) src
      with type (_, _, _) dst := Bytes.t

    module To_bigstring :
      Blit.S1_phantom2_distinct
      with type (_, 'seek, 'loc) src := ('seek, 'loc) src
      with type (_, _, _) dst := Bigstring.t

    module To_string : sig
      val sub : (_ src, string) Base.Blit.sub
      val subo : (_ src, string) Base.Blit.subo
    end

    include
      Accessors_read
      with type ('a, 'd, 'w, 'l) t = local_ ('d, 'w, 'l) iobuf -> pos:int -> 'a
      with type ('a, 'd, 'w, 'l) t__local =
        local_ ('d, 'w, 'l) iobuf -> pos:int -> local_ 'a
  end

  module type Peek_safe = sig
    include Peek_common (** @open *)

    (** [index ?pos ?len t c] returns [Some i] for the smallest [i >= pos] such that
        [char t i = c], or [None] if there is no such [i].

        @param pos default = 0
        @param len default = [length t - pos] *)
    val index
      :  local_ ([> read ], _, _) iobuf
      -> ?pos:int
      -> ?len:int
      -> char
      -> int option

    (** [rindex ?pos ?len t c] returns [Some i] for the largest [i >= pos] such that
        [char t i = c], or [None] if there is no such [i].

        @param pos default = 0
        @param len default = [length t - pos] *)
    val rindex
      :  local_ ([> read ], _, _) iobuf
      -> ?pos:int
      -> ?len:int
      -> char
      -> int option
  end

  module type Poke_safe = sig
    type (_, _, _) iobuf

    (** [decimal t ~pos i] returns the number of bytes written at [pos]. *)
    val decimal : local_ (read_write, 'w, 'l) iobuf -> pos:int -> int -> int

    (** Same as [decimal t int], but padding to [len] with prefix '0's. *)
    val padded_decimal
      :  local_ (read_write, 'w, 'l) iobuf
      -> pos:int
      -> len:int
      -> int
      -> int

    include
      Accessors_write
      with type ('a, 'd, 'w, 'l) t =
        local_ (read_write, 'w, 'l) iobuf -> pos:int -> 'a -> unit
      with type ('a, 'd, 'w, 'l) t__local =
        local_ (read_write, 'w, 'l) iobuf -> pos:int -> local_ 'a -> unit

    (** Same as [Fill.date_string_iso8601_extended t date], but does not advance [t]. *)
    val date_string_iso8601_extended : (Date.t, _, _, _) t
  end
end

module type Iobuf_safe = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Consume :
    Consume_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t

  module Fill :
    Fill_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t

  module Peek :
    Peek_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t

  module Poke :
    Poke_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t
end
