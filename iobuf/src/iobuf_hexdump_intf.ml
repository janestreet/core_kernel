open! Core

module Definitions = struct
  module type Hexdump_all = sig
    type nonrec ('rw, 'seek) t [@@deriving sexp_of]

    val to_string_hum : ?max_lines:int -> local_ (_, _) t -> string
    val to_sequence : ?max_lines:int -> (_, _) t -> string Sequence.t
  end
end

module type Iobuf_hexdump = sig
  include module type of struct
    include Definitions
  end

  module Window : Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t
  module Limits : Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t
  module Hexdump : Hexdump_all with type ('rw, 'seek) t = ('rw, 'seek) Iobuf_type.t

  module Debug : sig
    module Hexdump : Hexdump_all with type ('rw, 'seek) t = ('rw, 'seek) Iobuf_type.t
  end
end
