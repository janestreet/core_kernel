(** The Stable versions of Hashtbl, Hash_set, Map, and Set are defined here rather than in
    their respective modules because:

    1. We guarantee their serializations independent of the implementation of those modules
    2. Given 1. it is cleaner (and still okay) to separate the code into a separate file *)

open Std

module Set : sig
  module V1 (Elt : sig
    type t with bin_io, sexp
    include Comparator.S with type t := t
  end) : sig
    type t = (Elt.t, Elt.comparator_witness) Set.t with sexp, bin_io, compare
  end
end

module Hashtbl : sig
  module V1 (Key : Hashtbl.Key_binable) : sig
    type 'a t = (Key.t, 'a) Hashtbl.t with sexp, bin_io
  end
end

module Hash_set : sig
  module V1 (Elt : Hash_set.Elt_binable) : sig
    type t = Elt.t Hash_set.t with sexp, bin_io
  end
end

module Map : sig
  module V1 (Key : sig
    type t with bin_io, sexp
    include Comparator.S with type t := t
  end) : sig
    type 'a t = (Key.t, 'a, Key.comparator_witness) Map.t with sexp, bin_io, compare
  end
end
