open! Import

(* avoid getting shadowed by the similarly named modules in this file *)
module Core_map = Map
module Core_set = Set
module Core_hashtbl = Hashtbl
module Core_hash_set = Hash_set

module Hashtbl = struct
  module V1 (Elt : Hashtbl.Key_binable) : sig
    type 'a t = (Elt.t, 'a) Hashtbl.t [@@deriving sexp, bin_io]
  end = Hashtbl.Make_binable (Elt)
end

module Hash_set = struct
  module V1 (Elt : Core_hash_set.Elt_binable) : sig
    type t = Elt.t Core_hash_set.t [@@deriving sexp, bin_io]
  end = Hash_set.Make_binable (Elt)
end

module Hashable = struct
  module V1 = struct
    module type S = sig
      type key

      module Table : sig
        type 'a t = (key, 'a) Core_hashtbl.t [@@deriving sexp, bin_io]
      end

      module Hash_set : sig
        type t = key Core_hash_set.t [@@deriving sexp, bin_io]
      end
    end

    module Make (Key : Core_hashtbl.Key_binable) : S with type key := Key.t = struct
      module Table = Hashtbl.V1 (Key)
      module Hash_set = Hash_set.V1 (Key)
    end
  end
end
