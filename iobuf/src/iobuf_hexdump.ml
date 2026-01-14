open! Core
open Iobuf_safe
open Iobuf_type
include Iobuf_hexdump_intf.Definitions

module For_hexdump = struct
  module T3 = struct
    type nonrec ('rw, 'seek, 'loc) t = ('rw, 'seek, 'loc) t
  end

  module Window_indexable = struct
    include T3

    let length t = length t
    let get t pos = Peek.char t ~pos
  end

  module Limits_indexable = struct
    include T3

    let length t = t.hi_max - t.lo_min
    let get t pos = Bigstring.get ([%template buf [@mode local]] t) (t.lo_min + pos)
  end

  module Buffer_indexable = struct
    include T3

    let length t = Bigstring.length ([%template buf [@mode local]] t) [@nontail]
    let get t pos = Bigstring.get ([%template buf [@mode local]] t) pos
  end

  module Window = Hexdump.Of_indexable3 [@modality portable] (Window_indexable)
  module Limits = Hexdump.Of_indexable3 [@modality portable] (Limits_indexable)
  module Buffer = Hexdump.Of_indexable3 [@modality portable] (Buffer_indexable)

  module type Relative_indexable = sig @@ portable
    val name : string
    val lo : (_, _, _) t -> int
    val hi : (_, _, _) t -> int
  end

  module type Compound_indexable = sig @@ portable
    include Hexdump.S3 with type ('rw, 'seek, 'loc) t := ('rw, 'seek, 'loc) t

    val parts : (module Relative_indexable) list
  end

  module Make_compound_hexdump (Compound : Compound_indexable) = struct
    module Hexdump = struct
      include T3

      let relative_sequence ?max_lines t (module Relative : Relative_indexable) =
        let lo = Relative.lo t in
        let hi = Relative.hi t in
        Compound.Hexdump.to_sequence ?max_lines ~pos:lo ~len:(hi - lo) t
      ;;

      let to_sequence ?max_lines t =
        List.concat_map
          (Portability_hacks.magic_uncontended__promise_deeply_immutable_module
             Compound.parts)
          ~f:(fun (module Relative) ->
            [ Sequence.singleton (String.capitalize Relative.name)
            ; relative_sequence ?max_lines t (module Relative)
              |> Sequence.map ~f:(fun line -> "  " ^ line)
            ])
        |> Sequence.of_list
        |> Sequence.concat
      ;;

      let to_string_hum ?max_lines t =
        let t = globalize_copied t in
        to_sequence ?max_lines t |> Sequence.to_list |> String.concat ~sep:"\n"
      ;;

      let sexp_of_t _ _ _ t =
        List.map
          (Portability_hacks.magic_uncontended__promise_deeply_immutable_module
             Compound.parts)
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
    let hi t = Bigstring.length ([%template buf [@mode local]] t) [@nontail]
  end

  module Window_and_limits = Make_compound_hexdump (struct
      include Limits

      let parts : (module Relative_indexable) list =
        [ (module Window_within_limits); (module Limits_within_limits) ]
      ;;
    end)

  module Window_and_limits_and_buffer = Make_compound_hexdump (struct
      include Buffer

      let parts : (module Relative_indexable) list =
        [ (module Window_within_buffer)
        ; (module Limits_within_buffer)
        ; (module Buffer_within_buffer)
        ]
      ;;
    end)
end

module Window = For_hexdump.Window
module Limits = For_hexdump.Limits
module Debug = For_hexdump.Window_and_limits_and_buffer
include For_hexdump.Window_and_limits
