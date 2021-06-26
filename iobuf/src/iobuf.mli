(** A non-moving (in the GC sense) contiguous range of bytes, useful for I/O operations.

    An iobuf consists of:

    - bigstring
    - limits -- a subrange of the bigstring
    - window -- a subrange of the limits

    All iobuf operations are restricted to operate within the limits.  Initially, the
    window of an iobuf is identical to its limits.  A phantom type, the "seek" permission,
    controls whether or not code is allowed to change the limits and window.  With seek
    permission, the limits can be [narrow]ed, but can never be widened, and the window can
    be set to an arbitrary subrange of the limits.

    A phantom type controls whether code can read and write bytes in the bigstring (within
    the limits) or can only read them.

    To present a restricted view of an iobuf to a client, one can create a sub-iobuf or
    add a type constraint.

    Functions operate on the window unless the documentation or naming indicates
    otherwise. *)

open! Core
open Iobuf_intf

type nonrec seek = seek [@@deriving sexp_of]
type nonrec no_seek = no_seek [@@deriving sexp_of]

(** This type is a compiler witness that 'rw and 'seek do not affect layout; it enables
    wider use of unboxed GADTs. *)
type t_repr

(** The first type parameter controls whether the iobuf can be written to.  The second
    type parameter controls whether the window and limits can be changed.

    See the [Perms] module for information on how the first type parameter is used.

    To allow [no_seek] or [seek] access, a function's type uses [_] rather than [no_seek]
    as the type argument to [t].  Using [_] allows the function to be directly applied to
    either permission.  Using a specific permission would require code to use coercion
    [:>].

    There is no [t_of_sexp].  One should use [Iobuf.Hexdump.t_of_sexp] or [@sexp.opaque]
    as desired. *)
type (-'data_perm_read_write, +'seek_permission) t = private t_repr

(** [t_with_shallow_sexp] has a [sexp_of] that shows the windows and limits of
    the underlying bigstring, but no data. We do this rather than deriving sexp_of
    on [t] because it is much more likely to be noise than useful information, and
    so callers should probably not display the iobuf at all. *)
type ('rw, 'seek) t_with_shallow_sexp = ('rw, 'seek) t [@@deriving sexp_of]

include Invariant.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Window.Hexdump] submodule that renders the contents of [t]'s window. *)
module Window : Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Limits.Hexdump] submodule that renders the contents of [t]'s limits. *)
module Limits : Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Hexdump] submodule that renders the contents of [t]'s window and limits
    using indices relative to the limits. *)
include Compound_hexdump with type ('rw, 'seek) t := ('rw, 'seek) t

(** Provides a [Debug.Hexdump] submodule that renders the contents of [t]'s window,
    limits, and underlying bigstring using indices relative to the bigstring. *)
module Debug : Compound_hexdump with type ('rw, 'seek) t := ('rw, 'seek) t

(** {2 Creation} *)

(** [create ~len] creates a new iobuf, backed by a bigstring of length [len],
    with the limits and window set to the entire bigstring. *)
val create : len:int -> (_, _) t

(** [empty] is an immutable [t] of size 0. *)
val empty : (read, no_seek) t


(** [of_bigstring bigstring ~pos ~len] returns an iobuf backed by [bigstring], with the
    window and limits specified starting at [pos] and of length [len]. *)
val of_bigstring
  :  ?pos:int (** default is [0] *)
  -> ?len:int (** default is [Bigstring.length bigstring - pos] *)
  -> Bigstring.t
  -> ([< read_write ], _) t
(** forbid [immutable] to prevent aliasing *)

(** [of_string s] returns a new iobuf whose contents are [s]. *)
val of_string : string -> (_, _) t

(** [sub_shared t ~pos ~len] returns a new iobuf with limits and window set to the
    subrange of [t]'s window specified by [pos] and [len].  [sub_shared] preserves data
    permissions, but allows arbitrary seek permissions on the resulting iobuf. *)
val sub_shared : ?pos:int -> ?len:int -> ('d, _) t -> ('d, _) t

(** [copy t] returns a new iobuf whose contents are the same as those in the window of
    [t]. *)
val copy : (_, _) t -> (_, _) t

(** [clone t] returns a new iobuf that is a deep-copy of [t] including an exact copy of
    the underlying buffer and bounds. This means data outside the window is copied as
    well. *)
val clone : (_, _) t -> (_, _) t

(** [transfer ~src ~dst] makes the window of [dst] into a copy of the window of [src].
    Like [blito], [transfer] will raise if [Iobuf.length dst] < [Iobuf.length src].

    It is a utility function defined as [reset dst; blito ~src ~dst; flip_lo dst]. *)
val transfer : src:([> read ], _) t -> dst:([> write ], seek) t -> unit

(** [set_bounds_and_buffer ~src ~dst] copies bounds metadata (i.e., limits and window) and
    shallowly copies the buffer (data pointer) from [src] to [dst].  It does not access
    data, but does allow access through [dst].  This makes [dst] an alias of [src].

    Because [set_bounds_and_buffer] creates an alias, we disallow immutable [src] and
    [dst] using [[> write]].  Otherwise, one of [src] or [dst] could be [read_write :>
    read] and the other [immutable :> read], which would allow you to write the
    [immutable] alias's data through the [read_write] alias.

    [set_bounds_and_buffer] is typically used with a frame iobuf that need only be
    allocated once.  This frame can be updated repeatedly and handed to users, without
    further allocation.  Allocation-sensitive applications need this. *)
val set_bounds_and_buffer
  :  src:(([> write ] as 'data), _) t
  -> dst:('data, seek) t
  -> unit

(** [set_bounds_and_buffer_sub ~pos ~len ~src ~dst] is a more efficient version of
    [set_bounds_and_buffer ~src:(Iobuf.sub_shared ~pos ~len src) ~dst].

    [set_bounds_and_buffer ~src ~dst] is not the same as [set_bounds_and_buffer_sub ~dst
    ~src ~len:(Iobuf.length src)] because the limits are narrowed in the latter case.

    [~len] and [~pos] are mandatory for performance reasons, in concert with [@@inline].
    If they were optional, allocation would be necessary when passing a non-default,
    non-constant value, which is an important use case. *)
val set_bounds_and_buffer_sub
  :  pos:int
  -> len:int
  -> src:(([> write ] as 'data), _) t
  -> dst:('data, seek) t
  -> unit
[@@inline]

(** {2 Generalization}

    One may wonder why you'd want to call [no_seek], given that a cast is already
    possible, e.g., [t : (_, seek) t :> (_, no_seek) t].  It turns out that if you want to
    define some [f : (_, _) t -> unit] of your own that can be conveniently applied to
    [seek] iobufs without the user having to cast [seek] up, you need this [no_seek]
    function.

    [read_only] is more of a historical convenience now that [read_write] is a polymorphic
    variant, as one can now explicitly specify the general type for an argument with
    something like [t : (_ perms, _) t :> (read, _) t]. *)
val read_only : ([> read ], 's) t -> (read, 's) t

val no_seek : ('r, _) t -> ('r, no_seek) t

(** {2 Accessors} *)

(** [capacity t] returns the size of [t]'s limits subrange.  The capacity of an iobuf can
    be reduced via [narrow]. *)
val capacity : (_, _) t -> int

(** [length t] returns the size of [t]'s window. *)
val length : (_, _) t -> int

(** [length_lo t] returns the length that [t]'s window would have after calling [flip_lo],
    without actually changing the window. This is the number of bytes between the lower
    limit and the start of the window.

    When you're writing to the window, you can think of this as the number of bytes
    already written. When reading from the window, this can mean the number of bytes
    already consumed.

    This is equivalent to: {[ Iobuf.Expert.(lo t - lo_min t)]}. *)
val length_lo : (_, _) t -> int

(** [length_hi t] returns the length that [t]'s window would have after calling [flip_hi],
    without actually changing the window. This is the number of bytes between the end of
    the window and the upper limit of the buffer.

    This is equivalent to: {[ Iobuf.Expert.(hi_max t - hi t) ]}. *)
val length_hi : (_, _) t -> int

(** [is_empty t] is [length t = 0]. *)
val is_empty : (_, _) t -> bool

(** {2 Changing the limits} *)

(** [narrow t] sets [t]'s limits to the current window. *)
val narrow : (_, seek) t -> unit

(** [narrow_lo t] sets [t]'s lower limit to the beginning of the current window. *)
val narrow_lo : (_, seek) t -> unit

(** [narrow_hi t] sets [t]'s upper limit to the end of the current window. *)
val narrow_hi : (_, seek) t -> unit

(** {2 Comparison} *)

(** [memcmp a b] first compares the length of [a] and [b]'s windows and then compares the
    bytes in the windows for equivalence. *)
val memcmp : (_, _) t -> (_, _) t -> int

(** {2 Changing the window} *)

(** One can call [Lo_bound.window t] to get a snapshot of the lower bound of the window,
    and then later restore that snapshot with [Lo_bound.restore].  This is useful for
    speculatively parsing, and then rewinding when there isn't enough data to finish.

    Similarly for [Hi_bound.window] and [Lo_bound.restore].

    Using a snapshot with a different iobuf, even a sub iobuf of the snapshotted one, has
    unspecified results.  An exception may be raised, or a silent error may occur.
    However, the safety guarantees of the iobuf will not be violated, i.e., the attempt
    will not enlarge the limits of the subject iobuf. *)

module type Bound = Bound with type ('d, 'w) iobuf := ('d, 'w) t

module Lo_bound : Bound
module Hi_bound : Bound


(** [advance t amount] advances the lower bound of the window by [amount].  It is an error
    to advance past the upper bound of the window or the lower limit. *)
val advance : (_, seek) t -> int -> unit

(** [unsafe_advance] is like [advance] but with no bounds checking, so incorrect usage can
    easily cause segfaults. *)
val unsafe_advance : (_, seek) t -> int -> unit


(** [resize t] sets the length of [t]'s window, provided it does not exceed limits. *)
val resize : (_, seek) t -> len:int -> unit

(** [unsafe_resize] is like [resize] but with no bounds checking, so incorrect usage can
    easily cause segfaults. *)
val unsafe_resize : (_, seek) t -> len:int -> unit

(** [rewind t] sets the lower bound of the window to the lower limit. *)
val rewind : (_, seek) t -> unit

(** [reset t] sets the window to the limits. *)
val reset : (_, seek) t -> unit

(** [flip_lo t] sets the window to range from the lower limit to the lower bound of the
    old window.  This is typically called after a series of [Fill]s, to reposition the
    window in preparation to [Consume] the newly written data.

    The bounded version narrows the effective limit.  This can preserve some data near the
    limit, such as a hypothetical packet header (in the case of [bounded_flip_lo]) or
    unfilled suffix of a buffer (in [bounded_flip_hi]). *)
val flip_lo : (_, seek) t -> unit

val bounded_flip_lo : (_, seek) t -> Lo_bound.t -> unit

(** [compact t] copies data from the window to the lower limit of the iobuf and sets the
    window to range from the end of the copied data to the upper limit.  This is typically
    called after a series of [Consume]s to save unread data and prepare for the next
    series of [Fill]s and [flip_lo]. *)
val compact : (read_write, seek) t -> unit

val bounded_compact : (read_write, seek) t -> Lo_bound.t -> Hi_bound.t -> unit

(** [flip_hi t] sets the window to range from the the upper bound of the current window to
    the upper limit.  This operation is dual to [flip_lo] and is typically called when the
    data in the current (narrowed) window has been processed and the window needs to be
    positioned over the remaining data in the buffer.  For example:

    {[
      (* ... determine initial_data_len ... *)
      Iobuf.resize buf ~len:initial_data_len;
      (* ... and process initial data ... *)
      Iobuf.flip_hi buf;
    ]}

    Now the window of [buf] ranges over the remainder of the data. *)
val flip_hi : (_, seek) t -> unit

val bounded_flip_hi : (_, seek) t -> Hi_bound.t -> unit

(** [protect_window_and_bounds t ~f] calls [f t] with [t]'s bounds set to its current
    window, and restores [t]'s window, bounds, and buffer afterward. *)
val protect_window_and_bounds : ('rw, no_seek) t -> f:(('rw, seek) t -> 'a) -> 'a

(** [protect_window_and_bounds_1 t x ~f] is a more efficient version of
    [protect_window_and_bounds t ~f:(fun t -> f t x)]. *)
val protect_window_and_bounds_1
  :  ('rw, no_seek) t
  -> 'a
  -> f:(('rw, seek) t -> 'a -> 'b)
  -> 'b

(** [protect_window_and_bounds_2 t x y ~f] is a more efficient version of
    [protect_window_and_bounds t ~f:(fun t -> f t x y)]. *)
val protect_window_and_bounds_2
  :  ('rw, no_seek) t
  -> 'a
  -> 'b
  -> f:(('rw, seek) t -> 'a -> 'b -> 'c)
  -> 'c

(** [protect_window_and_bounds_3 t x y z ~f] is a more efficient version of
    [protect_window_and_bounds t ~f:(fun t -> f t x y z)]. *)
val protect_window_and_bounds_3
  :  ('rw, no_seek) t
  -> 'a
  -> 'b
  -> 'c
  -> f:(('rw, seek) t -> 'a -> 'b -> 'c -> 'd)
  -> 'd

(** {2 Getting and setting data}

    "consume" and "fill" functions access data at the lower bound of the window and
    advance the lower bound of the window. "peek" and "poke" functions access data but do
    not advance the window. *)

(** [to_string t] returns the bytes in [t] as a string.  It does not alter the window. *)
val to_string : ?len:int -> ([> read ], _) t -> string

(** Equivalent to [Hexdump.to_string_hum].  Renders [t]'s windows and limits. *)
val to_string_hum : ?max_lines:int -> (_, _) t -> string

(** [to_bytes t] returns the bytes in [t] as a bytes.  It does not alter the window. *)
val to_bytes : ?len:int -> (_, _) t -> Bytes.t

(** [of_bytes b] returns a new iobuf whose contents is [b]. *)
val of_bytes : Bytes.t -> (_, _) t


(** [Consume.string t ~len] reads [len] characters (all, by default) from [t] into a new
    string and advances the lower bound of the window accordingly.

    [Consume.bin_prot X.bin_read_t t] returns the initial [X.t] in [t], advancing past the
    bytes read. *)
module Consume : sig
  (** [To_bytes.blito ~src ~dst ~dst_pos ~src_len ()] reads [src_len] bytes from [src],
      advancing [src]'s window accordingly, and writes them into [dst] starting at
      [dst_pos].  By default [dst_pos = 0] and [src_len = length src].  It is an error if
      [dst_pos] and [src_len] don't specify a valid region of [dst] or if [src_len >
      length src]. *)
  type src = (read, seek) t

  module To_bytes : Consuming_blit with type src := src with type dst := Bytes.t
  module To_bigstring : Consuming_blit with type src := src with type dst := Bigstring.t

  module To_string : sig
    (** [subo] defaults to using [Iobuf.length src]. *)
    val subo : ?len:int -> src -> string

    val sub : src -> len:int -> string
  end

  include
    Accessors_read
    with type ('a, 'r, 's) t = (([> read ] as 'r), seek) t -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end

(** [Fill.bin_prot X.bin_write_t t x] writes [x] to [t] in bin-prot form, advancing past
    the bytes written. *)
module Fill : sig
  include
    Accessors_write
    with type ('a, 'd, 'w) t = (read_write, seek) t -> 'a -> unit
    with type 'a bin_prot := 'a Bin_prot.Type_class.writer

  (** [decimal t int] is equivalent to [Iobuf.Fill.string t (Int.to_string int)], but with
      improved efficiency and no intermediate allocation.

      In other words: It fills the decimal representation of [int] to [t].  [t] is
      advanced by the number of characters written and no terminator is added.  If
      sufficient space is not available, [decimal] will raise. *)
  val decimal : (int, _, _) t
end

(** [Peek] and [Poke] functions access a value at [pos] from the lower bound of the window
    and do not advance.

    [Peek.bin_prot X.bin_read_t t] returns the initial [X.t] in [t] without advancing.

    Following the [bin_prot] protocol, the representation of [x] is [X.bin_size_t x] bytes
    long.  [Peek.], [Poke.], [Consume.], and [Fill.bin_prot] do not add any size prefix or
    other framing to the [bin_prot] representation. *)
module Peek : sig
  (** Similar to [Consume.To_*], but do not advance the buffer. *)
  type 'seek src = (read, 'seek) t

  module To_bytes :
    Blit.S1_distinct with type 'seek src := 'seek src with type _ dst := Bytes.t

  module To_bigstring :
    Blit.S1_distinct with type 'seek src := 'seek src with type _ dst := Bigstring.t

  module To_string : sig
    val sub : (_ src, string) Base.Blit.sub
    val subo : (_ src, string) Base.Blit.subo
  end

  val index : ([> read ], _) t -> ?pos:int -> ?len:int -> char -> int option
  val unsafe_index : ([> read ], _) t -> ?pos:int -> ?len:int -> char -> int

  include
    Accessors_read
    with type ('a, 'd, 'w) t = ('d, 'w) t -> pos:int -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end


(** [Poke.bin_prot X.bin_write_t t x] writes [x] to the beginning of [t] in binary form
    without advancing.  You can use [X.bin_size_t] to tell how long it was.
    [X.bin_write_t] is only allowed to write that portion of the buffer you have access
    to. *)
module Poke : sig
  (** [decimal t ~pos i] returns the number of bytes written at [pos]. *)
  val decimal : (read_write, 'w) t -> pos:int -> int -> int

  (** As [bin_prot] but returns the number of bytes written. *)
  val bin_prot_size
    :  'a Bin_prot.Type_class.writer
    -> (read_write, _) t
    -> pos:int
    -> 'a
    -> int

  include
    Accessors_write
    with type ('a, 'd, 'w) t = (read_write, 'w) t -> pos:int -> 'a -> unit
    with type 'a bin_prot := 'a Bin_prot.Type_class.writer
end

(** [Unsafe] has submodules that are like their corresponding module, except with no range
    checks.  Hence, mistaken uses can cause segfaults.  Be careful! *)
module Unsafe : sig
  module Consume : module type of Consume
  module Fill : module type of Fill
  module Peek : module type of Peek
  module Poke : module type of Poke
end

(** The number of bytes in the length prefix of [consume_bin_prot] and [fill_bin_prot]. *)
val bin_prot_length_prefix_bytes : int

(** [fill_bin_prot] writes a bin-prot value to the lower bound of the window, prefixed by
    its length, and advances by the amount written.  [fill_bin_prot] returns an error if
    the window is too small to write the value.

    [consume_bin_prot t reader] reads a bin-prot value from the lower bound of the window,
    which should have been written using [fill_bin_prot], and advances the window by the
    amount read.  [consume_bin_prot] returns an error if there is not a complete message
    in the window and in that case the window is left unchanged.

    Don't use these without a good reason, as they are incompatible with similar functions
    in [Reader] and [Writer].  They use a 4-byte length rather than an 8-byte length. *)
val fill_bin_prot
  :  ([> write ], seek) t
  -> 'a Bin_prot.Type_class.writer
  -> 'a
  -> unit Or_error.t

val consume_bin_prot
  :  ([> read ], seek) t
  -> 'a Bin_prot.Type_class.reader
  -> 'a Or_error.t

(** [Blit] copies between iobufs and advances neither [src] nor [dst]. *)
module Blit : sig
  val blit : (([> read ], _) t, ([> write ], _) t) Base.Blit.blit
  val blito : (([> read ], _) t, ([> write ], _) t) Base.Blit.blito
  val unsafe_blit : (([> read ], _) t, ([> write ], _) t) Base.Blit.blit
  val sub : (([> read ], _) t, (_, _) t) Base.Blit.sub
  val subo : (([> read ], _) t, (_, _) t) Base.Blit.subo

  (** Copies as much as possible (returning the number of bytes copied) without running
      out of either buffer's window. *)
  val blit_maximal
    :  src:([> read ], _) t
    -> ?src_pos:int
    -> dst:([> write ], _) t
    -> ?dst_pos:int
    -> unit
    -> int
end

(** [Blit_consume] copies between iobufs and advances [src] but does not advance [dst]. *)
module Blit_consume : sig
  val blit
    :  src:([> read ], seek) t
    -> dst:([> write ], _) t
    -> dst_pos:int
    -> len:int
    -> unit

  val blito
    :  src:([> read ], seek) t
    -> ?src_len:int
    -> dst:([> write ], _) t
    -> ?dst_pos:int
    -> unit
    -> unit

  val unsafe_blit
    :  src:([> read ], seek) t
    -> dst:([> write ], _) t
    -> dst_pos:int
    -> len:int
    -> unit

  val sub : ([> read ], seek) t -> len:int -> (_, _) t
  val subo : ?len:int -> ([> read ], seek) t -> (_, _) t

  val blit_maximal
    :  src:([> read ], seek) t
    -> dst:([> write ], _) t
    -> ?dst_pos:int
    -> unit
    -> int
end

(** [Blit_fill] copies between iobufs and advances [dst] but does not advance [src]. *)
module Blit_fill : sig
  val blit
    :  src:([> read ], _) t
    -> src_pos:int
    -> dst:([> write ], seek) t
    -> len:int
    -> unit

  val blito
    :  src:([> read ], _) t
    -> ?src_pos:int
    -> ?src_len:int
    -> dst:([> write ], seek) t
    -> unit
    -> unit

  val unsafe_blit
    :  src:([> read ], _) t
    -> src_pos:int
    -> dst:([> write ], seek) t
    -> len:int
    -> unit

  val blit_maximal
    :  src:([> read ], _) t
    -> ?src_pos:int
    -> dst:([> write ], seek) t
    -> unit
    -> int
end

(** [Blit_consume_and_fill] copies between iobufs and advances both [src] and [dst]. *)
module Blit_consume_and_fill : sig
  val blit : src:([> read ], seek) t -> dst:([> write ], seek) t -> len:int -> unit

  val blito
    :  src:([> read ], seek) t
    -> ?src_len:int
    -> dst:([> write ], seek) t
    -> unit
    -> unit

  val unsafe_blit : src:([> read ], seek) t -> dst:([> write ], seek) t -> len:int -> unit
  val blit_maximal : src:([> read ], seek) t -> dst:([> write ], seek) t -> int
end

(** [memset t ~pos ~len c] fills [t] with [c] within the range [\[pos, pos + len)]. *)
val memset : (read_write, _) t -> pos:int -> len:int -> char -> unit

(** [memset]s a buffer to zero. *)
val zero
  :  (read_write, _) t
  -> unit

(** Create a new iobuf whose contents are the appended contents of the passed array. *)
val concat : ([> read ], _) t array -> (_, _) t

(** {2 Expert} *)

(** The [Expert] module is for building efficient out-of-module [Iobuf] abstractions. *)
module Expert : sig
  (** These accessors will not allocate, and are mainly here to assist in building
      low-cost syscall wrappers.

      One must be careful to avoid writing out of the limits (between [lo_min] and
      [hi_max]) of the [buf].  Doing so would violate the invariants of the parent
      [Iobuf]. *)
  val buf : (_, _) t -> Bigstring.t

  val hi_max : (_, _) t -> int
  val hi : (_, _) t -> int
  val lo : (_, _) t -> int
  val lo_min : (_, _) t -> int

  (** These setters directly set fields in [t] without checking any invariants. *)
  val set_buf : (_, _) t -> Bigstring.t -> unit

  val set_hi_max : (_, _) t -> int -> unit
  val set_hi : (_, _) t -> int -> unit
  val set_lo : (_, _) t -> int -> unit
  val set_lo_min : (_, _) t -> int -> unit

  (** [to_bigstring_shared t] and [to_iobuf_shared t] allocate new wrappers around the
      storage of [buf t], relative to [t]'s current bounds.

      These operations allow access outside the bounds and limits of [t], and without
      respect to its read/write access.  Be careful not to violate [t]'s invariants. *)
  val to_bigstring_shared : ?pos:int -> ?len:int -> (_, _) t -> Bigstring.t

  (** [reinitialize_of_bigstring t bigstring] reinitializes [t] with backing [bigstring],
      and the window and limits specified starting at [pos] and of length [len]. *)
  val reinitialize_of_bigstring : (_, _) t -> pos:int -> len:int -> Bigstring.t -> unit

  (** These versions of [set_bounds_and_buffer] allow [~src] to be read-only.  [~dst] will
      be writable through [~src] aliases even though the type does not reflect this! *)
  val set_bounds_and_buffer : src:('data, _) t -> dst:('data, seek) t -> unit

  val set_bounds_and_buffer_sub
    :  pos:int
    -> len:int
    -> src:('data, _) t
    -> dst:('data, seek) t
    -> unit

  (** Similar to [protect_window_and_bounds], but does not save/restore the buffer or
      bounds. Mixing this with functions like [set_bounds_and_buffer] or [narrow] is
      unsafe; you should not modify anyything but the window inside [f]. *)
  val protect_window : ('rw, _) t -> f:(('rw, seek) t -> 'a) -> 'a
end

module type Accessors_common = Accessors_common
module type Accessors_read = Accessors_read
module type Accessors_write = Accessors_write
module type Consuming_blit = Consuming_blit

type nonrec ('src, 'dst) consuming_blito = ('src, 'dst) consuming_blito
