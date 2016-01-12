(** The Stable versions of Hashtbl, Hash_set, Map, and Set are defined here rather than in
    their respective modules because:

    1. We guarantee their serializations independent of the implementation of those modules
    2. Given 1. it is cleaner (and still okay) to separate the code into a separate file *)

open Std_internal

module Comparable : sig
  module V1 : sig
    module type S = sig
      type key
      type comparator_witness

      module Map : sig
        type 'a t = (key, 'a, comparator_witness) Map.t
        include Stable1 with type 'a t := 'a t
      end

      module Set : sig
        type t = (key, comparator_witness) Set.t
        include Stable with type t := t
      end
    end

    module Make (
      Key : sig
        type t [@@deriving bin_io, sexp]
        include Comparator.S with type t := t
      end
    ) : S with type key := Key.t and type comparator_witness := Key.comparator_witness
  end
end

module Hashable : sig
  module V1 : sig
    module type S = sig
      type key

      module Table : sig
        type 'a t = (key, 'a) Hashtbl.t [@@deriving sexp, bin_io]
      end

      module Hash_set : sig
        type t = key Hash_set.t [@@deriving sexp, bin_io]
      end
    end

    module Make (Key : Hashtbl.Key_binable) : S with type key := Key.t
  end
end
