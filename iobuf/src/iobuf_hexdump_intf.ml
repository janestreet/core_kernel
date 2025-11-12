open! Core

module Definitions = struct
  module type Hexdump_all = sig
    type nonrec ('rw, 'seek, 'loc) t [@@deriving sexp_of]

    val to_string_hum : ?max_lines:int -> local_ (_, _, _) t -> string
    val to_sequence : ?max_lines:int -> (_, _, _) t -> string Sequence.t
  end
end

module type Iobuf_hexdump = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Window :
    Hexdump.S3 with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t

  module Limits :
    Hexdump.S3 with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t

  module Hexdump :
    Hexdump_all with type ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) Iobuf_type.t

  module Debug : sig
    module Hexdump :
      Hexdump_all with type ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) Iobuf_type.t
  end
end
