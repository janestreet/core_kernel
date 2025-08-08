open! Core
open Iobuf_safe
open Iobuf_type
include Iobuf_hexdump_intf.Definitions

module For_hexdump = struct
  module T2 = struct
    type nonrec ('rw, 'seek) t = ('rw, 'seek) t
  end

  module Window_indexable = struct
    include T2

    let length t = length t
    let get t pos = Peek.char t ~pos
  end

  module Limits_indexable = struct
    include T2

    let length t = t.hi_max - t.lo_min
    let get t pos = Bigstring.get t.buf (t.lo_min + pos)
  end

  module Buffer_indexable = struct
    include T2

    let length t = Bigstring.length t.buf
    let get t pos = Bigstring.get t.buf pos
  end

  module Window = Hexdump.Of_indexable2 [@modality portable] (Window_indexable)
  module Limits = Hexdump.Of_indexable2 [@modality portable] (Limits_indexable)
  module Buffer = Hexdump.Of_indexable2 [@modality portable] (Buffer_indexable)

  module type Relative_indexable = sig
    val name : string
    val lo : (_, _) t -> int
    val hi : (_, _) t -> int
  end

  let portabilize_Relative_indexable =
    (Portability_hacks.magic_portable__first_class_module
     : (module Relative_indexable) -> (module Relative_indexable))
  ;;

  module type Compound_indexable = sig
    include Hexdump.S2 with type ('rw, 'seek) t := ('rw, 'seek) t

    val parts : (module Relative_indexable) list
  end

  module Make_compound_hexdump (Compound : Compound_indexable) = struct
    module Hexdump = struct
      include T2

      let relative_sequence ?max_lines t (module Relative : Relative_indexable) =
        let lo = Relative.lo t in
        let hi = Relative.hi t in
        Compound.Hexdump.to_sequence ?max_lines ~pos:lo ~len:(hi - lo) t
      ;;

      let to_sequence ?max_lines t =
        List.concat_map
          (Portability_hacks.magic_uncontended__first_class_module Compound.parts)
          ~f:(fun (module Relative) ->
            [ Sequence.singleton (String.capitalize Relative.name)
            ; relative_sequence ?max_lines t (module Relative)
              |> Sequence.map ~f:(fun line -> "  " ^ line)
            ])
        |> Sequence.of_list
        |> Sequence.concat
      ;;

      let to_string_hum ?max_lines t =
        let t = globalize0 t in
        to_sequence ?max_lines t |> Sequence.to_list |> String.concat ~sep:"\n"
      ;;

      let sexp_of_t _ _ t =
        List.map
          (Portability_hacks.magic_uncontended__first_class_module Compound.parts)
          ~f:(fun (module Relative) ->
            Relative.name, Sequence.to_list (relative_sequence t (module Relative)))
        |> [%sexp_of: (string * string list) list]
      ;;
    end
  end

  module Window_within_limits = struct
    let name = "window"
    let lo t = t.lo - t.lo_min
    let hi t = t.hi - t.lo_min
  end

  module Limits_within_limits = struct
    let name = "limits"
    let lo _ = 0
    let hi t = t.hi_max - t.lo_min
  end

  module Window_within_buffer = struct
    let name = "window"
    let lo t = t.lo
    let hi t = t.hi
  end

  module Limits_within_buffer = struct
    let name = "limits"
    let lo t = t.lo_min
    let hi t = t.hi_max
  end

  module Buffer_within_buffer = struct
    let name = "buffer"
    let lo _ = 0
    let hi t = Bigstring.length t.buf
  end

  module Window_and_limits = Make_compound_hexdump (struct
      include Limits

      let parts =
        [ (module Window_within_limits) |> portabilize_Relative_indexable
        ; (module Limits_within_limits) |> portabilize_Relative_indexable
        ]
      ;;
    end)

  module Window_and_limits_and_buffer = Make_compound_hexdump (struct
      include Buffer

      let parts =
        [ (module Window_within_buffer) |> portabilize_Relative_indexable
        ; (module Limits_within_buffer) |> portabilize_Relative_indexable
        ; (module Buffer_within_buffer) |> portabilize_Relative_indexable
        ]
      ;;
    end)
end

module Window = For_hexdump.Window
module Limits = For_hexdump.Limits
module Debug = For_hexdump.Window_and_limits_and_buffer
include For_hexdump.Window_and_limits
