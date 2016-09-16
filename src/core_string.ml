open! Import
open Bin_prot.Std
open Typerep_lib.Std

module Char = Core_char
module Int  = Core_int
module List = Core_list

module Stable_workaround = struct
  module V1 = struct
    module T = struct
      type t = string [@@deriving hash, bin_io, sexp]
      include (Base.String : Base.Comparable.S
               with type t := t
               with type comparator_witness = Base.String.comparator_witness)
    end
    include T
    include Comparable.Stable.V1.Make (T)
  end
end

module Stable = struct
  module V1 = struct
    include Stable_workaround.V1
  end
end

module Caseless = struct
  module T = struct
    type t = string [@@deriving bin_io, sexp]
    include Base.String.Caseless
  end
  include T
  include Comparable.Make_binable_using_comparator(T)
  include Hashable.Make_binable(T)
end

type t = string [@@deriving typerep]

include (Base.String
         : module type of struct include Base.String end
         with type t := t
         with module Caseless := Base.String.Caseless)

include Identifiable.Extend (Base.String) (struct
    type t = string [@@deriving bin_io]
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Let_syntax

  let gen' ?length char_gen =
    let%bind length =
      match length with
      | None     -> return None
      | Some gen ->
        let%bind len = gen in
        return (Some (`Exactly len))
    in
    let%bind chars = List.gen' char_gen ?length in
    return (of_char_list chars)

  let gen = gen' Char.gen

  let obs =
    Observer.unmap (List.obs Char.obs)
      ~f:to_list

  let shrinker =
    Shrinker.map (List.shrinker Char.shrinker) ~f:of_char_list ~f_inverse:to_list

end

let gen'     = For_quickcheck.gen'
let gen      = For_quickcheck.gen
let obs      = For_quickcheck.obs
let shrinker = For_quickcheck.shrinker

let%test_module "Caseless Comparable" = (module struct
  let%test _ =
    Int.equal (Core_map.find_exn (Caseless.Map.of_alist_exn [("a", 4); ("b", 5)]) "A") 4

  let%test _ = Core_set.mem (Caseless.Set.of_list ["hello"; "world"]) "heLLO"
  let%test _ = Int.equal (Core_set.length (Caseless.Set.of_list ["a"; "A"])) 1
end)
