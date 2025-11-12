open! Core
open Iobuf_safe_intf.Definitions

module Definitions = struct
  module type Consume_unsafe = sig
    include Consume_safe (** @open *)
  end

  module type Fill_unsafe = sig
    include Fill_safe (** @open *)
  end

  module type Peek_unsafe = sig
    include Peek_common (** @open *)

    (** Like [Peek.index] but with no bounds checks, and returns a negative number rather
        than [None] when the character is not found. *)
    val index_or_neg
      :  local_ ([> read ], _, _) iobuf
      -> ?pos:local_ int
      -> ?len:local_ int
      -> char
      -> int

    (** Like [Peek.rindex] but with no bounds checks, and returns a negative number rather
        than [None] when the character is not found. *)
    val rindex_or_neg
      :  local_ ([> read ], _, _) iobuf
      -> ?pos:local_ int
      -> ?len:local_ int
      -> char
      -> int

    (** Like [Peek.index] but with no bounds checks. *)
    val index__local
      :  local_ ([> read ], _, _) iobuf
      -> ?pos:local_ int
      -> ?len:local_ int
      -> char
      -> local_ int option

    (** Like [Peek.rindex] but with no bounds checks. *)
    val rindex__local
      :  local_ ([> read ], _, _) iobuf
      -> ?pos:local_ int
      -> ?len:local_ int
      -> char
      -> local_ int option
  end

  module type Poke_unsafe = sig
    include Poke_safe (** @open *)
  end
end

module type Iobuf_unsafe = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Consume :
    Consume_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t

  module Fill :
    Fill_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t

  module Peek :
    Peek_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t

  module Poke :
    Poke_unsafe with type ('rw, 'seek, 'loc) iobuf := ('rw, 'seek, 'loc) Iobuf_type.t
end
