(** See {{!Iobuf} [Iobuf]} for documentation. *)

open! Core
open Iobuf_type_intf.Definitions

module Definitions = struct
  (** An iobuf window bound, either upper or lower. You can't see its int value, but you
      can save and restore it. *)
  module type Bound = sig
    type ('d, 'w) iobuf

    type t = private int (*_ performance hack: avoid the write barrier *)
    [@@deriving compare ~localize, sexp_of]

    val window : local_ (_, _) iobuf -> t
    val limit : local_ (_, _) iobuf -> t
    val restore : t -> local_ (_, seek) iobuf -> unit
  end

  module type Basic = sig
    type nonrec seek = seek [@@deriving globalize, sexp_of]
    type nonrec no_seek = no_seek [@@deriving globalize, sexp_of]
    type ('rw, 'seek) t [@@deriving globalize]

    (** Globalize as if [t] had zero type parameters. Works because the parameters are
        phantom types, and do not represent actual values that need to be globalized. *)
    val globalize0 : local_ ('rw, _) t -> ('rw, _) t

    module With_shallow_sexp : sig
      (** [With_shallow_sexp.t] has a [sexp_of] that shows the windows and limits of the
          underlying bigstring, but no data. We do this rather than deriving sexp_of on
          [t] because it is much more likely to be noise than useful information, and so
          callers should probably not display the iobuf at all. *)
      type nonrec ('rw, 'seek) t = ('rw, 'seek) t [@@deriving sexp_of, globalize]
    end

    include Invariant.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

    (** {2 Creation} *)

    (** [create ~len] creates a new iobuf, backed by a bigstring of length [len], with the
        limits and window set to the entire bigstring. *)
    val create : len:int -> (_, _) t

    (** [empty] is an immutable [t] of size 0. *)
    val empty : (read, no_seek) t

    (** [of_bigstring bigstring ~pos ~len] returns an iobuf backed by [bigstring], with
        the window and limits specified starting at [pos] and of length [len]. *)
    val of_bigstring
      :  ?pos:local_ int (** default is [0] *)
      -> ?len:local_ int (** default is [Bigstring.length bigstring - pos] *)
      -> Bigstring.t
      -> ([< read_write ], _) t
    (** forbid [immutable] to prevent aliasing *)

    (** [of_bigstring__local] is like [of_bigstring], but it allocates the iobuf record
        locally. *)
    val of_bigstring__local
      :  ?pos:local_ int
      -> ?len:local_ int
      -> Bigstring.t
      -> local_ ([< read_write ], _) t

    (** More efficient than the above (optional arguments are costly). *)
    val unsafe_bigstring_view
      :  pos:int
      -> len:int
      -> Bigstring.t
      -> local_ ([< read_write ], _) t

    val bigstring_view
      :  pos:int
      -> len:int
      -> Bigstring.t
      -> local_ ([< read_write ], _) t

    (** [of_string s] returns a new iobuf whose contents are [s]. *)
    val of_string : local_ string -> (_, _) t

    (** [of_string__local] is like [of_string], but it allocates the iobuf record locally.
        This still performs global allocation of the backing buffer. *)
    val of_string__local : local_ string -> local_ (_, _) t

    (** [sub_shared t ~pos ~len] returns a new iobuf with limits and window set to the
        subrange of [t]'s window specified by [pos] and [len]. [sub_shared] preserves data
        permissions, but allows arbitrary seek permissions on the resulting iobuf. *)
    val sub_shared : ?pos:local_ int -> ?len:local_ int -> local_ ('d, _) t -> ('d, _) t

    (** [sub_shared__local] is like [sub_shared], but it allocates the iobuf record
        locally. *)
    val sub_shared__local
      :  ?pos:local_ int
      -> ?len:local_ int
      -> local_ ('d, _) t
      -> local_ ('d, _) t

    (** More efficient than the above (optional arguments are costly). *)
    val unsafe_sub_shared : pos:int -> len:int -> local_ ('d, _) t -> local_ ('d, _) t

    (** [copy t] returns a new iobuf whose contents are the same as those in the window of
        [t]. *)
    val copy : local_ (_, _) t -> (_, _) t

    (** [clone t] returns a new iobuf that is a deep-copy of [t] including an exact copy
        of the underlying buffer and bounds. This means data outside the window is copied
        as well. *)
    val clone : local_ (_, _) t -> (_, _) t

    (** [transfer ~src ~dst] makes the window of [dst] into a copy of the window of [src].
        Like [blito], [transfer] will raise if [Iobuf.length dst] < [Iobuf.length src].

        It is a utility function defined as [reset dst; blito ~src ~dst; flip_lo dst]. *)
    val transfer : src:local_ ([> read ], _) t -> dst:local_ ([> write ], seek) t -> unit

    (** [set_bounds_and_buffer ~src ~dst] copies bounds metadata (i.e., limits and window)
        and shallowly copies the buffer (data pointer) from [src] to [dst]. It does not
        access data, but does allow access through [dst]. This makes [dst] an alias of
        [src].

        Because [set_bounds_and_buffer] creates an alias, we disallow immutable [src] and
        [dst] using [[> write]]. Otherwise, one of [src] or [dst] could be
        [read_write :> read] and the other [immutable :> read], which would allow you to
        write the [immutable] alias's data through the [read_write] alias.

        [set_bounds_and_buffer] is typically used with a frame iobuf that need only be
        allocated once. This frame can be updated repeatedly and handed to users, without
        further allocation. Allocation-sensitive applications need this. *)
    val set_bounds_and_buffer
      :  src:local_ (([> write ] as 'data), _) t
      -> dst:local_ ('data, seek) t
      -> unit

    (** [set_bounds_and_buffer_sub ~pos ~len ~src ~dst] is a more efficient version of
        [set_bounds_and_buffer ~src:(Iobuf.sub_shared ~pos ~len src) ~dst].

        [set_bounds_and_buffer ~src ~dst] is not the same as
        [set_bounds_and_buffer_sub ~dst ~src ~len:(Iobuf.length src)] because the limits
        are narrowed in the latter case.

        [~len] and [~pos] are mandatory for performance reasons, in concert with
        [@@inline]. If they were optional, allocation would be necessary when passing a
        non-default, non-constant value, which is an important use case. *)
    val set_bounds_and_buffer_sub
      :  pos:int
      -> len:int
      -> src:local_ (([> write ] as 'data), _) t
      -> dst:local_ ('data, seek) t
      -> unit

    (** {2 Generalization}

        One may wonder why you'd want to call [no_seek], given that a cast is already
        possible, e.g., [t : (_, seek) t :> (_, no_seek) t]. It turns out that if you want
        to define some [f : (_, _) t -> unit] of your own that can be conveniently applied
        to [seek] iobufs without the user having to cast [seek] up, you need this
        [no_seek] function.

        [read_only] is more of a historical convenience now that [read_write] is a
        polymorphic variant, as one can now explicitly specify the general type for an
        argument with something like [t : (_ perms, _) t :> (read, _) t]. *)

    val read_only : ([> read ], 's) t -> (read, 's) t
    val read_only__local : local_ ([> read ], 's) t -> local_ (read, 's) t
    val no_seek : ('r, _) t -> ('r, no_seek) t
    val no_seek__local : local_ ('r, _) t -> local_ ('r, no_seek) t

    (** {2 Accessors} *)

    (** [capacity t] returns the size of [t]'s limits subrange. The capacity of an iobuf
        can be reduced via [narrow]. *)
    val capacity : local_ (_, _) t -> int

    (** [length t] returns the size of [t]'s window. *)
    val length : local_ (_, _) t -> int

    (** [length_lo t] returns the length that [t]'s window would have after calling
        [flip_lo], without actually changing the window. This is the number of bytes
        between the lower limit and the start of the window.

        When you're writing to the window, you can think of this as the number of bytes
        already written. When reading from the window, this can mean the number of bytes
        already consumed.

        This is equivalent to:
        {[
          Iobuf.Expert.(lo t - lo_min t)
        ]}
        . *)
    val length_lo : local_ (_, _) t -> int

    (** [length_hi t] returns the length that [t]'s window would have after calling
        [flip_hi], without actually changing the window. This is the number of bytes
        between the end of the window and the upper limit of the buffer.

        This is equivalent to:
        {[
          Iobuf.Expert.(hi_max t - hi t)
        ]}
        . *)
    val length_hi : local_ (_, _) t -> int

    (** [is_empty t] is [length t = 0]. *)
    val is_empty : local_ (_, _) t -> bool

    (** {2 Changing the limits} *)

    (** [narrow t] sets [t]'s limits to the current window. *)
    val narrow : local_ (_, seek) t -> unit

    (** [narrow_lo t] sets [t]'s lower limit to the beginning of the current window. *)
    val narrow_lo : local_ (_, seek) t -> unit

    (** [narrow_hi t] sets [t]'s upper limit to the end of the current window. *)
    val narrow_hi : local_ (_, seek) t -> unit

    (** {2 Comparison} *)

    (** [memcmp a b] first compares the length of [a] and [b]'s windows and then compares
        the bytes in the windows for equivalence. *)
    val memcmp : local_ (_, _) t -> local_ (_, _) t -> int

    (** {2 Changing the window} *)

    (** One can call [Lo_bound.window t] to get a snapshot of the lower bound of the
        window, and then later restore that snapshot with [Lo_bound.restore]. This is
        useful for speculatively parsing, and then rewinding when there isn't enough data
        to finish.

        Similarly for [Hi_bound.window] and [Lo_bound.restore].

        Using a snapshot with a different iobuf, even a sub iobuf of the snapshotted one,
        has unspecified results. An exception may be raised, or a silent error may occur.
        However, the safety guarantees of the iobuf will not be violated, i.e., the
        attempt will not enlarge the limits of the subject iobuf. *)

    module type Bound = Bound with type ('d, 'w) iobuf := ('d, 'w) t

    module Lo_bound : Bound
    module Hi_bound : Bound

    (** [advance t amount] advances the lower bound of the window by [amount]. It is an
        error to advance past the upper bound of the window or the lower limit. *)
    val advance : local_ (_, seek) t -> int -> unit

    (** [unsafe_advance] is like [advance] but with no bounds checking, so incorrect usage
        can easily cause segfaults. *)
    val unsafe_advance : local_ (_, seek) t -> int -> unit

    (** [resize t] sets the length of [t]'s window, provided it does not exceed limits. *)
    val resize : local_ (_, seek) t -> len:int -> unit

    (** [unsafe_resize] is like [resize] but with no bounds checking, so incorrect usage
        can easily cause segfaults. *)
    val unsafe_resize : local_ (_, seek) t -> len:int -> unit

    (** [rewind t] sets the lower bound of the window to the lower limit. *)
    val rewind : local_ (_, seek) t -> unit

    (** [reset t] sets the window to the limits. *)
    val reset : local_ (_, seek) t -> unit

    (** [flip_lo t] sets the window to range from the lower limit to the lower bound of
        the old window. This is typically called after a series of [Fill]s, to reposition
        the window in preparation to [Consume] the newly written data.

        The bounded version narrows the effective limit. This can preserve some data near
        the limit, such as a hypothetical packet header (in the case of [bounded_flip_lo])
        or unfilled suffix of a buffer (in [bounded_flip_hi]). *)
    val flip_lo : local_ (_, seek) t -> unit

    val bounded_flip_lo : local_ (_, seek) t -> Lo_bound.t -> unit

    (** [compact t] copies data from the window to the lower limit of the iobuf and sets
        the window to range from the end of the copied data to the upper limit. This is
        typically called after a series of [Consume]s to save unread data and prepare for
        the next series of [Fill]s and [flip_lo]. *)
    val compact : local_ (read_write, seek) t -> unit

    val bounded_compact : local_ (read_write, seek) t -> Lo_bound.t -> Hi_bound.t -> unit

    (** [flip_hi t] sets the window to range from the the upper bound of the current
        window to the upper limit. This operation is dual to [flip_lo] and is typically
        called when the data in the current (narrowed) window has been processed and the
        window needs to be positioned over the remaining data in the buffer. For example:

        {[
          (* ... determine initial_data_len ... *)
          Iobuf.resize buf ~len:initial_data_len;
          (* ... and process initial data ... *)
          Iobuf.flip_hi buf
        ]}

        Now the window of [buf] ranges over the remainder of the data. *)
    val flip_hi : local_ (_, seek) t -> unit

    val bounded_flip_hi : local_ (_, seek) t -> Hi_bound.t -> unit

    (** [protect_window_bounds_and_buffer t ~f] calls [f t] with [t]'s bounds set to its
        current window, and restores [t]'s window, bounds, and buffer afterward. *)
    val protect_window_bounds_and_buffer
      :  ('rw, no_seek) t
      -> f:local_ (('rw, seek) t -> 'a)
      -> 'a

    (** [protect_window_bounds_and_buffer__local] is similar to
        [protect_window_bounds_and_buffer] except that it returns a local value *)
    val protect_window_bounds_and_buffer__local
      :  ('rw, no_seek) t
      -> f:local_ (('rw, seek) t -> local_ 'a)
      -> local_ 'a

    (** [protect_window_bounds_and_buffer_1 t x ~f] is a more efficient version of
        [protect_window_bounds_and_buffer t ~f:(fun t -> f t x)]. *)
    val protect_window_bounds_and_buffer_1
      :  ('rw, no_seek) t
      -> 'a
      -> f:local_ (('rw, seek) t -> 'a -> 'b)
      -> 'b

    (** [protect_window_bounds_and_buffer_2 t x y ~f] is a more efficient version of
        [protect_window_bounds_and_buffer t ~f:(fun t -> f t x y)]. *)
    val protect_window_bounds_and_buffer_2
      :  ('rw, no_seek) t
      -> 'a
      -> 'b
      -> f:local_ (('rw, seek) t -> 'a -> 'b -> 'c)
      -> 'c

    (** [protect_window_bounds_and_buffer_3 t x y z ~f] is a more efficient version of
        [protect_window_bounds_and_buffer t ~f:(fun t -> f t x y z)]. *)
    val protect_window_bounds_and_buffer_3
      :  ('rw, no_seek) t
      -> 'a
      -> 'b
      -> 'c
      -> f:local_ (('rw, seek) t -> 'a -> 'b -> 'c -> 'd)
      -> 'd

    (** {2 Getting and setting data}

        "consume" and "fill" functions access data at the lower bound of the window and
        advance the lower bound of the window. "peek" and "poke" functions access data but
        do not advance the window. *)

    (** [to_string t] returns the bytes in [t] as a string. It does not alter the window. *)
    val to_string : ?len:int -> local_ ([> read ], _) t -> string

    (** Equivalent to [Hexdump.to_string_hum]. Renders [t]'s windows and limits. *)
    val to_string_hum : ?max_lines:int -> local_ (_, _) t -> string

    (** [to_bytes t] returns the bytes in [t] as a bytes. It does not alter the window. *)
    val to_bytes : ?len:int -> local_ (_, _) t -> Bytes.t

    (** [of_bytes b] returns a new iobuf whose contents is [b]. *)
    val of_bytes : Bytes.t -> (_, _) t

    (** [memset t ~pos ~len c] fills [t] with [c] within the range [\[pos, pos + len)]. *)
    val memset : local_ (read_write, _) t -> pos:int -> len:int -> char -> unit

    (** [unsafe_memset t ~pos ~len c] fills [t] with [c] within the range
        [\[pos, pos + len)], without bounds checks. *)
    val unsafe_memset : local_ (read_write, _) t -> pos:int -> len:int -> char -> unit

    (** [memset]s a buffer to zero. *)
    val zero : local_ (read_write, _) t -> unit

    (** Create a new iobuf whose contents are the appended contents of the passed array. *)
    val concat : ([> read ], _) t array -> (_, _) t

    val contains : ([> read ], _) t -> substring:local_ Bigstring.t -> bool
  end
end

module type Iobuf_basic = sig
  include module type of struct
    include Definitions
  end

  include Basic with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t
end
