open! Import

module Stable = struct
  module V1 = struct
    type t = bytes [@@deriving bin_io, typerep]
    include (Base.Bytes : module type of struct include Base.Bytes end
             with type t := t)
  end
end

include Stable.V1

include Hexdump.Of_indexable (struct
    type t = bytes
    let length = length
    let get    = get
  end)

let gen = String.gen |> Quickcheck.Generator.map ~f:of_string
let obs = String.obs |> Quickcheck.Observer.unmap ~f:to_string

let shrinker =
  String.shrinker
  |> Quickcheck.Shrinker.map ~f:of_string ~f_inverse:to_string

let gen' char_gen =
  String.gen' char_gen
  |> Quickcheck.Generator.map ~f:of_string

let gen_with_length len char_gen =
  String.gen_with_length len char_gen
  |> Quickcheck.Generator.map ~f:of_string
