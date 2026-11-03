open! Core

module Definitions : sig @@ portable
  (** [no_seek] and [seek] are phantom types used in a similar manner to [read] and
      [read_write]. *)

  (** Like [read]. *)
  type no_seek [@@deriving sexp_of ~localize ~stackify]

  (** Like [read_write]. *)
  type seek = private no_seek [@@deriving sexp_of ~localize ~stackify]

  type global = Modes.At_locality.global [@@deriving sexp_of ~localize ~stackify]
  type local = Modes.At_locality.local [@@deriving sexp_of ~localize ~stackify]
end = struct
  type no_seek [@@deriving sexp_of ~localize ~stackify]
  type seek = private no_seek [@@deriving sexp_of ~localize ~stackify]
  type global = Modes.At_locality.global [@@deriving sexp_of ~localize ~stackify]
  type local = Modes.At_locality.local [@@deriving sexp_of ~localize ~stackify]
end

module type Iobuf_type = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Repr : sig
    type 'loc t =
      { mutable buf : (Bigstring.t, 'loc) Modes.At_locality.t @@ local
      ; mutable lo_min : int
      ; mutable lo : int
      ; mutable hi : int
      ; mutable hi_max : int
      }
    [@@deriving
      fields ~getters ~direct_iterators:(iter, set_all_mutable_fields), sexp_of ~localize]
  end

  type 'loc repr = 'loc Repr.t =
    { mutable buf : (Bigstring.t, 'loc) Modes.At_locality.t @@ local
    ; mutable lo_min : int
    ; mutable lo : int
    ; mutable hi : int
    ; mutable hi_max : int
    }

  type ('rw, 'seek, 'loc) t = 'loc Repr.t
  type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t

  val globalize : [ `deprecated ]
  val globalize_shared : ('rw, 'seek, global) t @ local -> ('rw, 'seek, global) t
  val globalize_copied : ('rw, 'seek, _) t @ local -> ('rw, 'seek, _) t

  module With_shallow_sexp : sig
    type ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) iobuf [@@deriving sexp_of ~localize]

    val globalize : [ `deprecated ]
  end

  [%%template:
  type 'a locality = 'a
  type _ locality = local [@@mode local]]

  [%%template:
  [@@@alloc.default a @ m = (heap_global, stack_local)]
  [@@@mode.default l = (global, m)]

  val of_bigstring
    :  ?pos:int @ local
    -> ?len:int @ local
    -> Bigstring.t @ l
    -> (_, _, (_ locality[@mode l])) t @ m

  val of_bigstring_sub
    :  pos:int
    -> len:int
    -> Bigstring.t @ l
    -> (_, _, (_ locality[@mode l])) t @ m

  val unsafe_of_bigstring_sub
    :  pos:int
    -> len:int
    -> Bigstring.t @ l
    -> (_, _, (_ locality[@mode l])) t @ m]

  val advance : (_, seek, _) t @ local -> int -> unit
  val bad_range : pos:int -> len:int -> (_, _, _) t @ local -> _
  val buf : (_, _, global) t @ local -> Bigstring.t [@@zero_alloc]

  [%%template:
    val buf : (_, _, _) t @ local -> Bigstring.t @ local [@@mode local] [@@zero_alloc]]

  val buf_pos_exn : (_, _, _) t @ local -> pos:int -> len:int -> int
  val check_range : (_, _, _) t @ local -> pos:int -> len:int -> unit
  val create : len:int -> (_, _, _) t
  val fail : (_, _, _) t @ local -> string -> 'a -> ('a -> Sexp.t) -> _
  val get_char : ([> read ], _, _) t @ local -> int -> char
  val length : (_, _, _) t @ local -> int

  val set_bounds_and_buffer
    :  src:(_, _, global) t @ local
    -> dst:(_, _, _) t @ local
    -> unit

  val set_bounds_and_buffer_sub
    :  pos:int
    -> len:int
    -> src:(_, _, global) t @ local
    -> dst:(_, _, _) t @ local
    -> unit

  val set_char : ([> write ], _, _) t @ local -> int -> char -> unit
  val unsafe_advance : (_, seek, _) t @ local -> int -> unit
  val unsafe_buf_pos : (_, _, _) t @ local -> pos:int -> len:int -> int
  val unsafe_is_safe : bool

  module Bytes_dst : sig
    type t = bytes [@@deriving sexp_of]

    val create : len:int -> t
    val length : t @ local -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val unsafe_blit : ((_, _, _) iobuf, t) Blit.blit
  end

  module Bigstring_dst : sig
    type t = Bigstring.t [@@deriving sexp_of]

    val create : len:int -> t
    val length : t @ local -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val unsafe_blit : ((_, _, _) iobuf, t) Blit.blit
  end

  module String_dst : sig
    val sub : ((_, _, _) iobuf, string) Blit.sub
    val subo : ((_, _, _) iobuf, string) Blit.subo
  end

  module Char_elt : sig
    type t = char [@@deriving equal ~localize]

    val of_bool : bool -> t
  end

  module T_src : sig
    type 'loc t = 'loc Repr.t

    val create : len:int -> (_, _, _) iobuf
    val length : (_, _, _) iobuf @ local -> int
    val get : (_, _, _) iobuf -> int -> char
    val set : (_, _, _) iobuf -> int -> char -> unit
  end
end
