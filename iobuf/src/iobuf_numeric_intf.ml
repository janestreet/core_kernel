open! Core

module Definitions = struct
  module type Itoa = sig
    (** [num_digits x] returns the number of digits in [x].

        ([num_digits 0] is defined as 1). *)
    val num_digits : int -> int
  end

  module type Date_string = sig
    (** [len_iso8601_extended] is the length (in bytes) of a date in YYYY-MM-DD format,
        i.e. 10. *)
    val len_iso8601_extended : int
  end
end

module type Iobuf_numeric = sig @@ portable
  include module type of struct
    include Definitions
  end

  open Iobuf_type

  module Itoa : sig
    include Itoa

    val poke_decimal : local_ ([> write ], _, _) t -> pos:int -> int -> int

    val poke_padded_decimal
      :  local_ ([> write ], _, _) t
      -> pos:int
      -> len:int
      -> int
      -> int

    val unsafe_poke_decimal : local_ ([> write ], _, _) t -> pos:int -> int -> int

    val unsafe_poke_padded_decimal
      :  local_ ([> write ], _, _) t
      -> pos:int
      -> len:int
      -> int
      -> int
  end

  module Date_string : sig
    include Date_string

    val poke_iso8601_extended : local_ ([> write ], _, _) t -> pos:int -> Date.t -> unit

    val unsafe_poke_iso8601_extended
      :  local_ ([> write ], _, _) t
      -> pos:int
      -> Date.t
      -> unit
  end
end
