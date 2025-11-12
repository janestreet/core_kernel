open! Core

module Definitions = struct
  include Iobuf_basic_intf.Definitions
  include Iobuf_bin_io_intf.Definitions
  include Iobuf_blit_intf.Definitions
  include Iobuf_expert_intf.Definitions
  include Iobuf_hexdump_intf.Definitions
  include Iobuf_numeric_intf.Definitions
  include Iobuf_safe_intf.Definitions
  include Iobuf_type_intf.Definitions
  include Iobuf_unsafe_intf.Definitions
end

module type Iobuf = sig @@ portable
  (** A non-moving (in the GC sense) contiguous range of bytes, useful for I/O operations.

      An iobuf consists of:

      - bigstring
      - limits -- a subrange of the bigstring
      - window -- a subrange of the limits

      All iobuf operations are restricted to operate within the limits. Initially, the
      window of an iobuf is identical to its limits. A phantom type, the "seek"
      permission, controls whether or not code is allowed to change the limits and window.
      With seek permission, the limits can be [narrow]ed, but can never be widened, and
      the window can be set to an arbitrary subrange of the limits.

      A phantom type controls whether code can read and write bytes in the bigstring
      (within the limits) or can only read them.

      To present a restricted view of an iobuf to a client, one can create a sub-iobuf or
      add a type constraint.

      Functions operate on the window unless the documentation or naming indicates
      otherwise. *)

  include module type of struct
    include Definitions
  end

  module Repr : sig
    (** This type is a compiler witness that 'rw and 'seek do not affect layout; it
        enables wider use of unboxed GADTs. *)
    type 'loc t : value mod non_float portable
  end

  (** The first type parameter controls whether the iobuf can be written to. The second
      type parameter controls whether the window and limits can be changed. The third type
      parameter controls whether the [Bigstring.t] buffer must be global or not.

      See the [Perms] module for information on how the first type parameter is used.

      To allow [no_seek] or [seek] access, a function's type uses [_] rather than
      [no_seek] as the type argument to [t]. Using [_] allows the function to be directly
      applied to either permission. Using a specific permission would require code to use
      coercion [:>].

      There is no [t_of_sexp]. One should use [Iobuf.Hexdump.t_of_sexp] or [@sexp.opaque]
      as desired. *)
  type (-'data_perm_read_write, +'seek_permission, 'buffer_locality) t =
    private
    'buffer_locality Repr.t

  include Basic with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** Provides a [Window.Hexdump] submodule that renders the contents of [t]'s window. *)
  module Window : Hexdump.S3 with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** Provides a [Limits.Hexdump] submodule that renders the contents of [t]'s limits. *)
  module Limits : Hexdump.S3 with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** Provides a [Hexdump] submodule that renders the contents of [t]'s window and limits
      using indices relative to the limits. *)
  module Hexdump : Hexdump_all with type ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) t

  (** Provides a [Debug.Hexdump] submodule that renders the contents of [t]'s window,
      limits, and underlying bigstring using indices relative to the bigstring. *)
  module Debug : sig
    module Hexdump : Hexdump_all with type ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) t
  end

  (** [Blit] copies between iobufs and advances neither [src] nor [dst]. *)
  module Blit : Blit with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** [Blit_consume] copies between iobufs and advances [src] but does not advance [dst]. *)
  module Blit_consume :
    Blit_consume with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** [Blit_fill] copies between iobufs and advances [dst] but does not advance [src]. *)
  module Blit_fill : Blit_fill with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** [Blit_consume_and_fill] copies between iobufs and advances both [src] and [dst]. *)
  module Blit_consume_and_fill :
    Blit_consume_and_fill with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  module Itoa : Itoa (** @inline *)

  module Date_string : Date_string (** @inline *)

  (** [Consume.string t ~len] reads [len] characters (all, by default) from [t] into a new
      string and advances the lower bound of the window accordingly.

      [Consume.bin_prot X.bin_read_t t] returns the initial [X.t] in [t], advancing past
      the bytes read. *)
  module Consume : Consume_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t

  (** [Fill.bin_prot X.bin_write_t t x] writes [x] to [t] in bin-prot form, advancing past
      the bytes written. *)
  module Fill : Fill_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t

  (** [Peek] and [Poke] functions access a value at [pos] from the lower bound of the
      window and do not advance.

      [Peek.bin_prot X.bin_read_t t] returns the initial [X.t] in [t] without advancing.

      Following the [bin_prot] protocol, the representation of [x] is [X.bin_size_t x]
      bytes long. [Peek.], [Poke.], [Consume.], and [Fill.bin_prot] do not add any size
      prefix or other framing to the [bin_prot] representation. *)
  module Peek : Peek_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t

  (** [Poke.bin_prot X.bin_write_t t x] writes [x] to the beginning of [t] in binary form
      without advancing. You can use [X.bin_size_t] to tell how long it was.
      [X.bin_write_t] is only allowed to write that portion of the buffer you have access
      to. *)
  module Poke : Poke_safe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t

  (** [Unsafe] has submodules that are like their corresponding module, except with no
      range checks. Hence, mistaken uses can cause segfaults. Be careful! *)
  module Unsafe : sig
    module Consume :
      Consume_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t

    module Fill : Fill_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t
    module Peek : Peek_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t
    module Poke : Poke_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) t
  end

  module Bin_io : Bin_io with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

  (** {2 Expert} *)

  (** The [Expert] module is for building efficient out-of-module [Iobuf] abstractions. *)
  module Expert : Expert with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t
  (** @inline *)
end
