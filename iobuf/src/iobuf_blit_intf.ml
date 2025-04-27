open! Core
open Iobuf_type_intf.Definitions

module Definitions = struct
  module type Blit = sig
    type ('rw, 'seek) t

    val blit : (([> read ], _) t, ([> write ], _) t) Base.Blit.blit
    val blito : (([> read ], _) t, ([> write ], _) t) Base.Blit.blito
    val unsafe_blit : (([> read ], _) t, ([> write ], _) t) Base.Blit.blit
    val sub : (([> read ], _) t, (_, _) t) Base.Blit.sub
    val subo : (([> read ], _) t, (_, _) t) Base.Blit.subo

    (** Copies as much as possible (returning the number of bytes copied) without running
        out of either buffer's window. *)
    val blit_maximal
      :  src:local_ ([> read ], _) t
      -> ?src_pos:int
      -> dst:local_ ([> write ], _) t
      -> ?dst_pos:int
      -> unit
      -> int
  end

  module type Blit_consume = sig
    type ('rw, 'seek) t

    val blit
      :  src:local_ ([> read ], seek) t
      -> dst:local_ ([> write ], _) t
      -> dst_pos:int
      -> len:int
      -> unit

    val blito
      :  src:local_ ([> read ], seek) t
      -> ?src_len:int
      -> dst:local_ ([> write ], _) t
      -> ?dst_pos:int
      -> unit
      -> unit

    val unsafe_blit
      :  src:local_ ([> read ], seek) t
      -> dst:local_ ([> write ], _) t
      -> dst_pos:int
      -> len:int
      -> unit

    val sub : ([> read ], seek) t -> len:int -> (_, _) t
    val subo : ?len:int -> ([> read ], seek) t -> (_, _) t

    val blit_maximal
      :  src:local_ ([> read ], seek) t
      -> dst:local_ ([> write ], _) t
      -> ?dst_pos:int
      -> unit
      -> int
  end

  module type Blit_fill = sig
    type ('rw, 'seek) t

    val blit
      :  src:local_ ([> read ], _) t
      -> src_pos:int
      -> dst:local_ ([> write ], seek) t
      -> len:int
      -> unit

    val blito
      :  src:local_ ([> read ], _) t
      -> ?src_pos:int
      -> ?src_len:int
      -> dst:local_ ([> write ], seek) t
      -> unit
      -> unit

    val unsafe_blit
      :  src:local_ ([> read ], _) t
      -> src_pos:int
      -> dst:local_ ([> write ], seek) t
      -> len:int
      -> unit

    val blit_maximal
      :  src:local_ ([> read ], _) t
      -> ?src_pos:int
      -> dst:local_ ([> write ], seek) t
      -> unit
      -> int
  end

  module type Blit_consume_and_fill = sig
    type ('rw, 'seek) t

    val blit
      :  src:local_ ([> read ], seek) t
      -> dst:local_ ([> write ], seek) t
      -> len:int
      -> unit

    val blito
      :  src:local_ ([> read ], seek) t
      -> ?src_len:int
      -> dst:local_ ([> write ], seek) t
      -> unit
      -> unit

    val unsafe_blit
      :  src:local_ ([> read ], seek) t
      -> dst:local_ ([> write ], seek) t
      -> len:int
      -> unit

    val blit_maximal
      :  src:local_ ([> read ], seek) t
      -> dst:local_ ([> write ], seek) t
      -> int
  end
end

module type Iobuf_blit = sig
  include module type of struct
    include Definitions
  end

  module Blit : Blit with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t
  module Blit_consume : Blit_consume with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t
  module Blit_fill : Blit_fill with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t

  module Blit_consume_and_fill :
    Blit_consume_and_fill with type ('rw, 'seek) t := ('rw, 'seek) Iobuf_type.t
end
