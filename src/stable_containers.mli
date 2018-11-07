(** The Stable version of [Hashable] is defined here rather than in [Hashable], but the
    implementation is effectively the same. *)

open! Import

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
