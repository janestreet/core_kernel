
(* We do not [include Base] here, and instead import modules that [Core_kernel] doesn't
   extend, because we want code in [Core_kernel] to be clear when it references a [Base]
   module that [Core_kernel] is overriding. *)
module Applicative               = Base.Applicative
module Avltree                   = Base.Avltree
module Backtrace                 = Base.Backtrace
module Binary_search             = Base.Binary_search
module Commutative_group         = Base.Commutative_group
module Equal                     = Base.Equal
module Exn                       = Base.Exn
module Floatable                 = Base.Floatable
module Hash                      = Base.Hash
module Hasher                    = Base.Hasher
module Heap_block                = Base.Heap_block
module Indexed_container         = Base.Indexed_container
module Invariant                 = Base.Invariant
module Intable                   = Base.Intable
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

include Stdio

include Base_for_tests

include Bin_prot.Std

module Field = Fieldslib.Field

module Sexp = Sexplib.Sexp

module From_sexplib =
  (Sexplib.Conv : sig

     type bigstring = Sexplib.Conv.bigstring [@@deriving sexp]
     type mat       = Sexplib.Conv.mat       [@@deriving sexp]
     type vec       = Sexplib.Conv.vec       [@@deriving sexp]

     (* [sexp_of_opaque] and [opaque_of_sexp] are used by the code generated from
        [[@@deriving sexp]], [[%sexp_of: ]], and [[%of_sexp: ]].  The type [_ sexp_opaque]
        expands to uses of [sexp_of_opaque] and [opaque_of_sexp]. *)
     val sexp_of_opaque : _ -> Sexp.t
     val opaque_of_sexp : Sexp.t -> _

     val sexp_of_pair : ('a -> Sexp.t) -> ('b -> Sexp.t) -> 'a * 'b -> Sexp.t
     val pair_of_sexp:  (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> 'a * 'b

     exception Of_sexp_error of exn * Sexp.t
     val of_sexp_error : string -> Sexp.t -> _
     val of_sexp_error_exn : exn -> Sexp.t -> _
   end)

include From_sexplib

(* [sexp_opaque] indicates to [ppx_sexp_conv] that a value should be rendered as [_], i.e.
   [Sexp.Atom "_"].  We define [sexp_opaque] here along with [@@deriving] so that other
   ppx's treat [sexp_opaque] correctly, by ignoring it and processing the underlying
   type. *)
type 'a sexp_opaque = 'a [@@deriving bin_io, compare, hash, typerep]

include
  (Typerep_lib.Std : (module type of struct include Typerep_lib.Std end
                       with module Type_equal := Typerep_lib.Std.Type_equal))

module Variant = Variantslib.Variant

let with_return = With_return.with_return

let am_running_inline_test = Ppx_inline_test_lib.Runtime.am_running_inline_test
