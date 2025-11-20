open! Core

module Definitions : sig @@ portable
  (** [no_seek] and [seek] are phantom types used in a similar manner to [read] and
      [read_write]. *)

  (** Like [read]. *)
  type no_seek [@@deriving sexp_of]

  (** Like [read_write]. *)
  type seek = private no_seek [@@deriving sexp_of]

  type global = Modes.At_locality.global [@@deriving sexp_of]
  type local = Modes.At_locality.local [@@deriving sexp_of]
end = struct
  type no_seek [@@deriving sexp_of]
  type seek = private no_seek [@@deriving sexp_of]
  type global = Modes.At_locality.global [@@deriving sexp_of]
  type local = Modes.At_locality.local [@@deriving sexp_of]
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
    [@@deriving fields ~getters ~direct_iterators:(iter, set_all_mutable_fields), sexp_of]
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
  val globalize_shared : local_ ('rw, 'seek, global) t -> ('rw, 'seek, global) t
  val globalize_copied : local_ ('rw, 'seek, _) t -> ('rw, 'seek, _) t

  module With_shallow_sexp : sig
    type ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) iobuf [@@deriving sexp_of]

    val globalize : [ `deprecated ]
  end

  [%%template:
  type 'a locality := 'a
  type _ locality := local [@@mode local]]

  [%%template:
  [@@@alloc.default a @ m = (heap_global, stack_local)]
  [@@@mode.default l = (global, m)]

  val of_bigstring
    :  ?pos:local_ int
    -> ?len:local_ int
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

  val advance : local_ (_, seek, _) t -> int -> unit
  val bad_range : pos:int -> len:int -> local_ (_, _, _) t -> _
  val buf : local_ (_, _, global) t -> Bigstring.t

  val%template buf : local_ (_, _, _) t -> local_ Bigstring.t [@@mode local]

  val buf_pos_exn : local_ (_, _, _) t -> pos:int -> len:int -> int
  val check_range : local_ (_, _, _) t -> pos:int -> len:int -> unit
  val create : len:int -> (_, _, _) t
  val fail : local_ (_, _, _) t -> string -> 'a -> ('a -> Sexp.t) -> _
  val get_char : local_ ([> read ], _, _) t -> int -> char
  val length : local_ (_, _, _) t -> int

  val set_bounds_and_buffer
    :  src:local_ (_, _, global) t
    -> dst:local_ (_, _, _) t
    -> unit

  val set_bounds_and_buffer_sub
    :  pos:int
    -> len:int
    -> src:local_ (_, _, global) t
    -> dst:local_ (_, _, _) t
    -> unit

  val set_char : local_ ([> write ], _, _) t -> int -> char -> unit
  val unsafe_advance : local_ (_, seek, _) t -> int -> unit
  val unsafe_buf_pos : local_ (_, _, _) t -> pos:int -> len:int -> int
  val unsafe_is_safe : bool

  module Bytes_dst : sig
    type t = bytes [@@deriving sexp_of]

    val create : len:int -> t
    val length : local_ t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val unsafe_blit : ((_, _, _) iobuf, t) Blit.blit
  end

  module Bigstring_dst : sig
    type t = Bigstring.t [@@deriving sexp_of]

    val create : len:int -> t
    val length : local_ t -> int
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
    val length : local_ (_, _, _) iobuf -> int
    val get : (_, _, _) iobuf -> int -> char
    val set : (_, _, _) iobuf -> int -> char -> unit
  end
end
