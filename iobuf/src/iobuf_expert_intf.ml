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

    val buf : (_, _, global) t -> Bigstring.t

    val%template buf : (_, _, _) t -> Bigstring.t [@@mode local]

    val hi_max : (_, _, _) t -> int
    val hi : (_, _, _) t -> int
    val lo : (_, _, _) t -> int
    val lo_min : (_, _, _) t -> int

    (** These setters directly set fields in [t] without checking any invariants. *)
    val set_buf : (_, _, _) t -> Bigstring.t -> unit

    val set_hi_max : (_, _, _) t -> int -> unit
    val set_hi : (_, _, _) t -> int -> unit
    val set_lo : (_, _, _) t -> int -> unit
    val set_lo_min : (_, _, _) t -> int -> unit

    (** [to_bigstring_shared t] and [to_iobuf_shared t] allocate new wrappers around the
        storage of [buf t], relative to [t]'s current bounds.

        These operations allow access outside the bounds and limits of [t], and without
        respect to its read/write access. Be careful not to violate [t]'s invariants. *)
    val to_bigstring_shared : ?pos:int -> ?len:int -> (_, _, global) t -> Bigstring.t

    (** [reinitialize_of_bigstring t bigstring] reinitializes [t] with backing
        [bigstring], and the window and limits specified starting at [pos] and of length
        [len]. *)
    val reinitialize_of_bigstring
      :  (_, _, _) t
      -> pos:int
      -> len:int
      -> Bigstring.t
      -> unit

    (** As [reinitialize_of_bigstring] but without checking, and requires explicit
        specification of bounds. *)
    val unsafe_reinitialize
      :  (_, _, _) t
      -> lo_min:int
      -> lo:int
      -> hi:int
      -> hi_max:int
      -> Bigstring.t
      -> unit

    (** These versions of [set_bounds_and_buffer] allow [~src] to be read-only. [~dst]
        will be writable through [~src] aliases even though the type does not reflect
        this! *)
    val set_bounds_and_buffer : src:('data, _, global) t -> dst:('data, seek, _) t -> unit

    val set_bounds_and_buffer_sub
      :  pos:int
      -> len:int
      -> src:('data, _, global) t
      -> dst:('data, seek, _) t
      -> unit

    (** Similar to [protect_window_bounds_and_buffer], but does not save/restore the
        buffer or bounds. Mixing this with functions like [set_bounds_and_buffer] or
        [narrow] is unsafe; you should not modify anyything but the window inside [f]. *)
    val protect_window : ('rw, _, 'loc) t -> f:(('rw, seek, 'loc) t -> 'a) -> 'a

    (** As [protect_window] but does not enforce that the closure should not let the
        buffer escape. Letting the buffer escape is dangerous and almost certainly not
        what you wanted. *)
    val protect_window_global_deprecated
      :  ('rw, _, 'loc) t
      -> f:(('rw, seek, 'loc) t -> 'a)
      -> 'a

    val protect_window_1
      :  ('rw, _, 'loc) t
      -> 'a
      -> f:(('rw, seek, 'loc) t -> 'a -> 'b)
      -> 'b

    val protect_window_1_global_deprecated
      :  ('rw, _, 'loc) t
      -> 'a
      -> f:(('rw, seek, 'loc) t -> 'a -> 'b)
      -> 'b

    val protect_window_2
      :  ('rw, _, 'loc) t
      -> 'a
      -> 'b
      -> f:(('rw, seek, 'loc) t -> 'a -> 'b -> 'c)
      -> 'c

    val protect_window_2_global_deprecated
      :  ('rw, _, 'loc) t
      -> 'a
      -> 'b
      -> f:(('rw, seek, 'loc) t -> 'a -> 'b -> 'c)
      -> 'c

    (** Similar to [protect_window] but returns a local value. *)
    val protect_window__local
      : 'a 'rw 'sk 'loc.
      ('rw, 'sk, 'loc) t -> f:(('rw, seek, 'loc) t -> 'a) -> 'a

    (** Computes the position within [buf] for [pos] relative to our window. Checks [len]
        bytes are available. *)
    val buf_pos_exn : (_, _, _) t -> pos:int -> len:int -> int

    (** As [buf_pos_exn] without checks. *)
    val unsafe_buf_pos : (_, _, _) t -> pos:int -> len:int -> int
  end
end

module type Iobuf_expert = sig
  include module type of struct
    include Definitions
  end

  include Expert with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t
end
