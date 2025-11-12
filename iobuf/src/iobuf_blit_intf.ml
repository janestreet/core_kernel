open! Core
open Iobuf_type_intf.Definitions

module Definitions = struct
  module type Blit = sig
    type ('rw, 'seek, 'loc) t

    val blit : (([> read ], _, _) t, ([> write ], _, _) t) Base.Blit.blit
    val blito : (([> read ], _, _) t, ([> write ], _, _) t) Base.Blit.blito
    val unsafe_blit : (([> read ], _, _) t, ([> write ], _, _) t) Base.Blit.blit
    val sub : (([> read ], _, _) t, (_, _, _) t) Base.Blit.sub
    val subo : (([> read ], _, _) t, (_, _, _) t) Base.Blit.subo

    (** Copies as much as possible (returning the number of bytes copied) without running
        out of either buffer's window. *)
    val blit_maximal
      :  src:local_ ([> read ], _, _) t
      -> ?src_pos:int
      -> dst:local_ ([> write ], _, _) t
      -> ?dst_pos:int
      -> unit
      -> int
  end

  module type Blit_consume = sig
    type ('rw, 'seek, 'loc) t

    val blit
      :  src:local_ ([> read ], seek, _) t
      -> dst:local_ ([> write ], _, _) t
      -> dst_pos:int
      -> len:int
      -> unit

    val blito
      :  src:local_ ([> read ], seek, _) t
      -> ?src_len:int
      -> dst:local_ ([> write ], _, _) t
      -> ?dst_pos:int
      -> unit
      -> unit

    val unsafe_blit
      :  src:local_ ([> read ], seek, _) t
      -> dst:local_ ([> write ], _, _) t
      -> dst_pos:int
      -> len:int
      -> unit

    val sub : ([> read ], seek, _) t -> len:int -> (_, _, _) t
    val subo : ?len:int -> ([> read ], seek, _) t -> (_, _, _) t

    val blit_maximal
      :  src:local_ ([> read ], seek, _) t
      -> dst:local_ ([> write ], _, _) t
      -> ?dst_pos:int
      -> unit
      -> int
  end

  module type Blit_fill = sig
    type ('rw, 'seek, 'loc) t

    val blit
      :  src:local_ ([> read ], _, _) t
      -> src_pos:int
      -> dst:local_ ([> write ], seek, _) t
      -> len:int
      -> unit

    val blito
      :  src:local_ ([> read ], _, _) t
      -> ?src_pos:int
      -> ?src_len:int
      -> dst:local_ ([> write ], seek, _) t
      -> unit
      -> unit

    val unsafe_blit
      :  src:local_ ([> read ], _, _) t
      -> src_pos:int
      -> dst:local_ ([> write ], seek, _) t
      -> len:int
      -> unit

    val blit_maximal
      :  src:local_ ([> read ], _, _) t
      -> ?src_pos:int
      -> dst:local_ ([> write ], seek, _) t
      -> unit
      -> int
  end

  module type Blit_consume_and_fill = sig
    type ('rw, 'seek, 'loc) t

    val blit
      :  src:local_ ([> read ], seek, _) t
      -> dst:local_ ([> write ], seek, _) t
      -> len:int
      -> unit

    val blito
      :  src:local_ ([> read ], seek, _) t
      -> ?src_len:int
      -> dst:local_ ([> write ], seek, _) t
      -> unit
      -> unit

    val unsafe_blit
      :  src:local_ ([> read ], seek, _) t
      -> dst:local_ ([> write ], seek, _) t
      -> len:int
      -> unit

    val blit_maximal
      :  src:local_ ([> read ], seek, _) t
      -> dst:local_ ([> write ], seek, _) t
      -> int
  end
end

module type Iobuf_blit = sig @@ portable
  include module type of struct
    include Definitions
  end

  module Blit : Blit with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t

  module Blit_consume :
    Blit_consume with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t

  module Blit_fill :
    Blit_fill with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t

  module Blit_consume_and_fill :
    Blit_consume_and_fill
    with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) Iobuf_type.t
end
