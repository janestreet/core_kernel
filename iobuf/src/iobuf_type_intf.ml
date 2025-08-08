open! Core

module Definitions : sig
  (** [no_seek] and [seek] are phantom types used in a similar manner to [read] and
      [read_write]. *)

  (** Like [read]. *)
  type no_seek [@@deriving sexp_of]

  (** Like [read_write]. *)
  type seek = private no_seek [@@deriving sexp_of]
end = struct
  type no_seek [@@deriving sexp_of]
  type seek = private no_seek [@@deriving sexp_of]
end

module type Iobuf_type = sig
  include module type of struct
    include Definitions
  end

  module Repr : sig
    type t =
      { mutable buf : Bigstring.t
      ; mutable lo_min : int
      ; mutable lo : int
      ; mutable hi : int
      ; mutable hi_max : int
      }
    [@@deriving fields ~getters ~direct_iterators:(iter, set_all_mutable_fields), sexp_of]
  end

  type repr = Repr.t =
    { mutable buf : Bigstring.t
    ; mutable lo_min : int
    ; mutable lo : int
    ; mutable hi : int
    ; mutable hi_max : int
    }

  type ('rw, 'seek) t = Repr.t [@@deriving globalize]
  type ('rw, 'seek) iobuf := ('rw, 'seek) t

  val globalize0 : ('rw, _) t -> ('rw, _) t

  module With_shallow_sexp : sig
    type ('rw, 'seek) t = ('rw, 'seek) iobuf [@@deriving globalize, sexp_of]
  end

  [%%template:
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  val of_bigstring : ?pos:int -> ?len:int -> Bigstring.t -> (_, _) t
  val of_bigstring_sub : pos:int -> len:int -> Bigstring.t -> (_, _) t
  val unsafe_of_bigstring_sub : pos:int -> len:int -> Bigstring.t -> (_, _) t]

  val advance : (_, seek) t -> int -> unit
  val bad_range : pos:int -> len:int -> (_, _) t -> _
  val buf_pos_exn : (_, _) t -> pos:int -> len:int -> int
  val check_range : (_, _) t -> pos:int -> len:int -> unit
  val create : len:int -> (_, _) t
  val fail : (_, _) t -> string -> 'a -> ('a -> Sexp.t) -> _
  val get_char : ([> read ], _) t -> int -> char
  val length : (_, _) t -> int
  val set_bounds_and_buffer : src:(_, _) t -> dst:(_, _) t -> unit

  val set_bounds_and_buffer_sub
    :  pos:int
    -> len:int
    -> src:(_, _) t
    -> dst:(_, _) t
    -> unit

  val set_char : ([> write ], _) t -> int -> char -> unit
  val unsafe_advance : (_, seek) t -> int -> unit
  val unsafe_buf_pos : (_, _) t -> pos:int -> len:int -> int
  val unsafe_is_safe : bool

  module Bytes_dst : sig
    type t = bytes [@@deriving sexp_of]

    val create : len:int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val unsafe_blit : ((_, _) iobuf, t) Blit.blit
  end

  module Bigstring_dst : sig
    type t = Bigstring.t [@@deriving sexp_of]

    val create : len:int -> t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val unsafe_blit : ((_, _) iobuf, t) Blit.blit
  end

  module String_dst : sig
    val sub : ((_, _) iobuf, string) Blit.sub
    val subo : ((_, _) iobuf, string) Blit.subo
  end

  module Char_elt : sig
    type t = char [@@deriving equal ~localize]

    val of_bool : bool -> t
  end

  module T_src : sig
    type t = Repr.t

    val create : len:int -> (_, _) iobuf
    val length : (_, _) iobuf -> int
    val get : (_, _) iobuf -> int -> char
    val set : (_, _) iobuf -> int -> char -> unit
  end
end
