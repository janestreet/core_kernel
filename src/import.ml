
(* We do not [include Base] here, and instead import modules that [Core_kernel] doesn't
   extend, because we want code in [Core_kernel] to be clear when it references a [Base]
   module that [Core_kernel] is overriding. *)
module Applicative               = Base.Applicative
module Array_permute             = Base.Array_permute
module Avltree                   = Base.Avltree
module Binary_search             = Base.Binary_search
module Caml                      = Base.Caml
module Commutative_group         = Base.Commutative_group
module Equal                     = Base.Equal
module Exn                       = Base.Exn
module Floatable                 = Base.Floatable
module Hash                      = Base.Hash
module Hasher                    = Base.Hasher
module Heap_block                = Base.Heap_block
module Invariant                 = Base.Invariant
module Intable                   = Base.Intable
module Maybe_bound               = Base.Maybe_bound
module Monad                     = Base.Monad
module Ordered_collection_common = Base.Ordered_collection_common
module Poly                      = Base.Poly
module Polymorphic_compare       = Base.Polymorphic_compare
module Popcount                  = Base.Popcount
module Pretty_printer            = Base.Pretty_printer
module Staged                    = Base.Staged
module Stringable                = Base.Stringable
module Validate                  = Base.Validate
module With_return               = Base.With_return
module Word_size                 = Base.Word_size

(* We do include [Base]'s top-level value and type bindings, because they don't cause
   any confusion, and duplicating them would be error prone. *)
include Base.Export

include Polymorphic_compare

module Bin_prot    = Bin_prot
module Fieldslib   = Fieldslib
module Typerep_lib = Typerep_lib
module Variantslib = Variantslib

include Bin_prot.Std

module Sexplib = struct
  include (Sexplib : (module type of struct include Sexplib end
                       with module Conv := Sexplib.Conv))

  module Conv = struct
    include Sexplib.Conv
    include
      (struct
        let sexp_of_float = sexp_of_float
      end : sig
         (* We want every float-to-sexp conversion done by [Core_kernel] to use its
            [Float.sexp_of_t], which respect [Sexp.of_float_style].  So we deprecate
            [Sexplib]'s [sexp_of_float] to avoid accidentally using it.  [Core_kernel.Std]
            re-exports [sexp_of_float] as [Float.sexp_of_t], so the deprecation only
            applies within [Core_kernel]. *)
         val sexp_of_float : float -> Sexp.t
           [@@deprecated "[since 2016-09] Use [Float.sexp_of_t]"]
       end)
  end
end

module Sexp = Sexplib.Sexp
