open! Core

module Definitions = struct
  (** [no_seek] and [seek] are phantom types used in a similar manner to [read] and
      [read_write]. *)

  (** Like [read]. *)
  type no_seek [@@deriving sexp_of]

  (** Like [read_write]. *)
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

  val globalize0 : local_ ('rw, _) t -> ('rw, _) t

  module With_shallow_sexp : sig
    type ('rw, 'seek) t = ('rw, 'seek) iobuf [@@deriving globalize, sexp_of]
  end

  val advance : local_ (_, seek) t -> int -> unit
  val bad_range : pos:int -> len:int -> local_ (_, _) t -> _
  val bigstring_view : pos:int -> len:int -> Bigstring.t -> local_ (_, _) t
  val buf_pos_exn : local_ (_, _) t -> pos:int -> len:int -> int
  val check_range : local_ (_, _) t -> pos:int -> len:int -> unit
  val create : len:int -> (_, _) t
  val fail : local_ (_, _) t -> string -> 'a -> ('a -> Sexp.t) -> _
  val get_char : local_ ([> read ], _) t -> int -> char
  val length : local_ (_, _) t -> int
  val of_bigstring : ?pos:local_ int -> ?len:local_ int -> Bigstring.t -> (_, _) t

  val of_bigstring__local
    :  ?pos:local_ int
    -> ?len:local_ int
    -> Bigstring.t
    -> local_ (_, _) t

  val set_bounds_and_buffer : src:local_ (_, _) t -> dst:local_ (_, _) t -> unit

  val set_bounds_and_buffer_sub
    :  pos:int
    -> len:int
    -> src:local_ (_, _) t
    -> dst:local_ (_, _) t
    -> unit

  val set_char : local_ ([> write ], _) t -> int -> char -> unit
  val unsafe_advance : local_ (_, seek) t -> int -> unit
  val unsafe_bigstring_view : pos:int -> len:int -> Bigstring.t -> local_ (_, _) t
  val unsafe_buf_pos : local_ (_, _) t -> pos:int -> len:int -> int
  val unsafe_is_safe : bool

  module Bytes_dst : sig
    type t = bytes [@@deriving sexp_of]

    val create : len:int -> t
    val length : local_ t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val unsafe_blit : ((_, _) iobuf, t) Blit.blit
  end

  module Bigstring_dst : sig
    type t = Bigstring.t [@@deriving sexp_of]

    val create : len:int -> t
    val length : local_ t -> int
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
    val length : local_ (_, _) iobuf -> int
    val get : (_, _) iobuf -> int -> char
    val set : (_, _) iobuf -> int -> char -> unit
  end
end
