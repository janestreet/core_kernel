open! Core
open Iobuf_type_intf.Definitions

module Definitions = struct
  module type Expert = sig
    type ('rw, 'seek, 'loc) t

    (** These accessors will not allocate, and are mainly here to assist in building
        low-cost syscall wrappers.

        One must be careful to avoid writing out of the limits (between [lo_min] and
        [hi_max]) of the [buf]. Doing so would violate the invariants of the parent
        [Iobuf]. *)

    val buf : local_ (_, _, global) t -> Bigstring.t

    val%template buf : local_ (_, _, _) t -> local_ Bigstring.t [@@mode local]

    val hi_max : local_ (_, _, _) t -> int
    val hi : local_ (_, _, _) t -> int
    val lo : local_ (_, _, _) t -> int
    val lo_min : local_ (_, _, _) t -> int

    (** These setters directly set fields in [t] without checking any invariants. *)
    val set_buf : local_ (_, _, _) t -> Bigstring.t -> unit

    val set_hi_max : local_ (_, _, _) t -> int -> unit
    val set_hi : local_ (_, _, _) t -> int -> unit
    val set_lo : local_ (_, _, _) t -> int -> unit
    val set_lo_min : local_ (_, _, _) t -> int -> unit

    (** [to_bigstring_shared t] and [to_iobuf_shared t] allocate new wrappers around the
        storage of [buf t], relative to [t]'s current bounds.

        These operations allow access outside the bounds and limits of [t], and without
        respect to its read/write access. Be careful not to violate [t]'s invariants. *)
    val to_bigstring_shared
      :  ?pos:int
      -> ?len:int
      -> local_ (_, _, global) t
      -> Bigstring.t

    (** [reinitialize_of_bigstring t bigstring] reinitializes [t] with backing
        [bigstring], and the window and limits specified starting at [pos] and of length
        [len]. *)
    val reinitialize_of_bigstring
      :  local_ (_, _, _) t
      -> pos:int
      -> len:int
      -> Bigstring.t
      -> unit

    (** As [reinitialize_of_bigstring] but without checking, and requires explicit
        specification of bounds. *)
    val unsafe_reinitialize
      :  local_ (_, _, _) t
      -> lo_min:int
      -> lo:int
      -> hi:int
      -> hi_max:int
      -> Bigstring.t
      -> unit

    (** These versions of [set_bounds_and_buffer] allow [~src] to be read-only. [~dst]
        will be writable through [~src] aliases even though the type does not reflect
        this! *)
    val set_bounds_and_buffer
      :  src:local_ ('data, _, global) t
      -> dst:local_ ('data, seek, _) t
      -> unit

    val set_bounds_and_buffer_sub
      :  pos:int
      -> len:int
      -> src:local_ ('data, _, global) t
      -> dst:local_ ('data, seek, _) t
      -> unit

    (** Similar to [protect_window_bounds_and_buffer], but does not save/restore the
        buffer or bounds. Mixing this with functions like [set_bounds_and_buffer] or
        [narrow] is unsafe; you should not modify anyything but the window inside [f]. *)
    val protect_window
      :  local_ ('rw, _, 'loc) t
      -> f:local_ (local_ ('rw, seek, 'loc) t -> 'a)
      -> 'a

    (** As [protect_window] but does not enforce that the closure should not let the
        buffer escape. Letting the buffer escape is dangerous and almost certainly not
        what you wanted. *)
    val protect_window_global_deprecated
      :  ('rw, _, 'loc) t
      -> f:local_ (('rw, seek, 'loc) t -> 'a)
      -> 'a

    val protect_window_1
      :  local_ ('rw, _, 'loc) t
      -> 'a
      -> f:local_ (local_ ('rw, seek, 'loc) t -> 'a -> 'b)
      -> 'b

    val protect_window_1_global_deprecated
      :  ('rw, _, 'loc) t
      -> 'a
      -> f:local_ (('rw, seek, 'loc) t -> 'a -> 'b)
      -> 'b

    val protect_window_2
      :  local_ ('rw, _, 'loc) t
      -> 'a
      -> 'b
      -> f:local_ (local_ ('rw, seek, 'loc) t -> 'a -> 'b -> 'c)
      -> 'c

    val protect_window_2_global_deprecated
      :  ('rw, _, 'loc) t
      -> 'a
      -> 'b
      -> f:local_ (('rw, seek, 'loc) t -> 'a -> 'b -> 'c)
      -> 'c

    (** Similar to [protect_window] but returns a local value. *)
    val protect_window__local
      : ('a : value_or_null) 'rw 'sk 'loc.
      local_ ('rw, 'sk, 'loc) t
      -> f:local_ (local_ ('rw, seek, 'loc) t -> local_ 'a)
      -> local_ 'a

    (** Computes the position within [buf] for [pos] relative to our window. Checks [len]
        bytes are available. *)
    val buf_pos_exn : local_ (_, _, _) t -> pos:int -> len:int -> int

    (** As [buf_pos_exn] without checks. *)
    val unsafe_buf_pos : local_ (_, _, _) t -> pos:int -> len:int -> int
  end
end

module type Iobuf_expert = sig @@ portable
  include module type of struct
    include Definitions
  end

  include Expert with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t
end
