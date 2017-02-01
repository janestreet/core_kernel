open! Import

module Char = Core_char
module Int  = Core_int
module List = Core_list

module Stable_workaround = struct
  module V1 = struct
    module T = struct
      type t = string [@@deriving bin_io]
      include (Base.String : module type of struct include Base.String end
               with type t := t)
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
    type t = string [@@deriving bin_io]
    include (Base.String.Caseless : module type of struct include Base.String.Caseless end
             with type t := t)
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

include Hexdump.Of_indexable (struct
    type t = string
    let length = length
    let get    = get
  end)

module For_quickcheck = struct

  module Generator = Quickcheck.Generator
  module Observer  = Quickcheck.Observer
  module Shrinker  = Quickcheck.Shrinker

  open Generator.Let_syntax

  let default_length =
    let%bind size = Generator.size in
    (* Generating the empty string for every size 0 case is far more tests of the empty
       string than we need.  Instead, at size 0 we generate strings of length 0 and length
       1.  At size N>0 we generate strings of length N+1. *)
    if Int.equal size 0
    then Generator.weighted_union [ 1., return 0; 10., return 1 ]
    else return (size + 1)

  let gen' ?(length = default_length) char_gen =
    let%bind len   = length in
    let%bind chars = List.gen' char_gen ~length:(`Exactly len) in
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

let%test_module "Caseless Comparable" =
  (module struct
    let%test _ =
      Int.equal (Core_map.find_exn (Caseless.Map.of_alist_exn [("a", 4); ("b", 5)]) "A") 4

    let%test _ = Core_set.mem (Caseless.Set.of_list ["hello"; "world"]) "heLLO"
    let%test _ = Int.equal (Core_set.length (Caseless.Set.of_list ["a"; "A"])) 1

  end)

let%test_module "Caseless Hash" =
  (module struct
    let%test _ = Int.equal (Caseless.hash "Hello") (Caseless.hash "HELLO")
  end)
