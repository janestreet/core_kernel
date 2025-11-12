open! Core
open Iobuf_type_intf.Definitions

module Definitions = struct
  module type Bin_io = sig
    type ('rw, 'seek, 'loc) t

    (** Read and advance *)

    val consume
      :  'a Bin_prot.Type_class.reader
      -> ([> read ], seek, global) t @ local
      -> 'a

    val unsafe_consume
      :  'a Bin_prot.Type_class.reader
      -> ([> read ], seek, global) t @ local
      -> 'a

    (** Read without advancing *)

    val peek : 'a Bin_prot.Read.reader -> (_, _, global) t @ local -> pos:int -> 'a
    val unsafe_peek : 'a Bin_prot.Read.reader -> (_, _, global) t @ local -> pos:int -> 'a

    (** Write and advance *)

    val fill
      :  'a Bin_prot.Type_class.writer
      -> (read_write, seek, global) t @ local
      -> 'a
      -> unit

    val fill__local
      :  'a Bin_prot.Size.sizer__local
      -> 'a Bin_prot.Write.writer__local
      -> (read_write, seek, global) t @ local
      -> 'a @ local
      -> unit

    val unsafe_fill
      :  'a Bin_prot.Type_class.writer
      -> (read_write, seek, global) t @ local
      -> 'a
      -> unit

    val unsafe_fill__local
      :  'a Bin_prot.Size.sizer__local
      -> 'a Bin_prot.Write.writer__local
      -> (read_write, seek, global) t @ local
      -> 'a @ local
      -> unit

    (** Write without advancing *)

    val poke
      :  'a Bin_prot.Type_class.writer
      -> (read_write, _, global) t @ local
      -> pos:int
      -> 'a
      -> unit

    val poke_size
      :  'a Bin_prot.Type_class.writer
      -> (read_write, _, global) t @ local
      -> pos:int
      -> 'a
      -> int

    val unsafe_poke
      :  'a Bin_prot.Type_class.writer
      -> (read_write, _, global) t @ local
      -> pos:int
      -> 'a
      -> unit

    val unsafe_poke_size
      :  'a Bin_prot.Type_class.writer
      -> (read_write, _, global) t @ local
      -> pos:int
      -> 'a
      -> int

    val%template unsafe_poke_with_known_size
      :  ('a Bin_prot.Write.writer[@mode m])
      -> (read_write, _, global) t @ local
      -> pos:int
      -> size:int
      -> 'a @ m
      -> unit
    [@@mode m = (global, local)]

    (** Include length of bin_prot payload as a header *)

    val header_bytes : int

    val consume_with_header
      :  ([> read ], seek, global) t @ local
      -> 'a Bin_prot.Type_class.reader
      -> 'a Or_error.t

    val fill_with_header
      :  ([> write ], seek, global) t @ local
      -> 'a Bin_prot.Type_class.writer
      -> 'a
      -> unit Or_error.t

    val fill_with_header__local
      :  ([> write ], seek, global) t @ local
      -> 'a Bin_prot.Size.sizer__local
      -> 'a Bin_prot.Write.writer__local
      -> 'a @ local
      -> unit Or_error.t
  end
end

module type Iobuf_bin_io = sig @@ portable
  include module type of struct
    include Definitions
  end

  include Bin_io with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t
end
