(* We don't just include Sexplib.Std because one can only define Hashtbl once in this
   module. *)

open! Import

include Core_pervasives (* [include]d first so that everything else shadows it *)

include Deprecate_pipe_bang
include Either.Export
include (Float : Interfaces.Robustly_comparable with type t := float)
include From_sexplib
include Interfaces
include Core_list.Infix
include Never_returns
include Ordering.Export
include Perms.Export
include Result.Export

module Array        = Core_array
module Bool         = Bool
module Char         = Core_char
module Comparator   = Comparator
module Exn          = Base.Exn
module Field        = Field
module Hashtbl      = Core_hashtbl
module Int          = Core_int
module Int32        = Core_int32
module Int63        = Core_int63
module Int64        = Core_int64
module Lazy         = Core_lazy
module Linked_queue = Linked_queue
module Map          = Core_map
module Nativeint    = Core_nativeint
module Ordering     = Ordering
module Random       = Core_random
module Ref          = Ref
module Result       = Result
module Set          = Core_set
module Sexp         = Core_sexp
module Staged       = Base.Staged
module String       = Core_string
module Type_equal   = Type_equal
module List = struct
  include Core_list

  (** [stable_dedup] Same as [dedup] but maintains the order of the list and doesn't allow
      compare function to be specified (otherwise, the implementation in terms of Set.t
      would hide a heavyweight functor instantiation at each call). *)
  let stable_dedup = Set.Poly.stable_dedup_list

  (* This function is staged to indicate that real work (the functor application) takes
     place after a partial application. *)
  let stable_dedup_staged (type a) ~(compare : a -> a -> int)
    : (a list -> a list) Staged.t =
    let module Set =
      Set.Make (struct
        type t = a
        let compare = compare
        (* [stable_dedup_list] never calls these *)
        let t_of_sexp _ = assert false
        let sexp_of_t _ = assert false
      end)
    in
    Staged.stage Set.stable_dedup_list
  ;;
end

type -'a return = 'a With_return.return = private {
  return : 'b. 'a -> 'b;
}

exception Bug of string [@@deriving sexp]

(** Raised if malloc in C bindings fail (errno * size). *)
exception C_malloc_exn of int * int (* errno, size *)
let () =
  Callback.register_exception "C_malloc_exn" (C_malloc_exn (0, 0))

exception Finally = Exn.Finally

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

let uw = function Some x -> x | None -> raise Not_found

(** [phys_same] is like [phys_equal], but with a more general type.  [phys_same] is useful
    when dealing with existential types, when one has a packed value and an unpacked value
    that one wants to check are physically equal.  One can't use [phys_equal] in such a
    situation because the types are different. *)
let phys_same (type a) (type b) (a : a) (b : b) = phys_equal a (Obj.magic b : a)

let ( %  ) = Int.( %  )
let ( /% ) = Int.( /% )
let ( // ) = Int.( // )

let (==>) a b = (not a) || b

let bprintf            = Printf.bprintf
let const              = Fn.const
let eprintf            = Printf.eprintf
let error              = Or_error.error
let error_s            = Or_error.error_s
let failwithf          = Base.Printf.failwithf
let failwithp          = Error.failwithp
let failwiths          = Error.failwiths
let force              = Base.Lazy.force
let fprintf            = Printf.fprintf
let ident              = Fn.id
let invalid_argf       = Base.Printf.invalid_argf
let is_none            = Option.is_none
let is_some            = Option.is_some
let ksprintf           = Printf.ksprintf
let ok_exn             = Or_error.ok_exn
let phys_equal         = Base.phys_equal
let phys_same          = phys_same
let printf             = Printf.printf
let protect            = Exn.protect
let protectx           = Exn.protectx
let raise_s            = Error.raise_s
let round              = Float.round
let sprintf            = Printf.sprintf
let stage              = Staged.stage
let unstage            = Staged.unstage
let with_return        = With_return.with_return
let with_return_option = With_return.with_return_option

(* With the following aliases, we are just making extra sure that the toplevel sexp
   converters line up with the ones in our modules. *)





include Typerep_lib.Std_internal

include (struct
  (* [deriving hash] in missing for [array] and [ref] since these types are mutable.
     (string is also mutable, but we pretend it isn't for hashing purposes) *)
  type 'a array  = 'a Array.    t [@@deriving bin_io, compare,       sexp, typerep]
  type bool      = Bool.        t [@@deriving bin_io, compare, hash, sexp, typerep]
  type char      = Char.        t [@@deriving bin_io, compare, hash, sexp, typerep]
  type float     = Float.       t [@@deriving bin_io, compare, hash, sexp, typerep]
  type int       = Int.         t [@@deriving bin_io, compare, hash, sexp, typerep]
  type int32     = Int32.       t [@@deriving bin_io, compare, hash, sexp, typerep]
  type int64     = Int64.       t [@@deriving bin_io, compare, hash, sexp, typerep]
  type 'a lazy_t = 'a Core_lazy.t [@@deriving bin_io, compare, hash, sexp, typerep]
  type 'a list   = 'a List.     t [@@deriving bin_io, compare, hash, sexp, typerep]
  type nativeint = Nativeint.   t [@@deriving bin_io, compare, hash, sexp, typerep]
  type 'a option = 'a Option.   t [@@deriving bin_io, compare, hash, sexp, typerep]
  type string    = String.      t [@@deriving bin_io, compare, hash, sexp, typerep]
  type 'a ref    = 'a Ref.      t [@@deriving bin_io, compare,       sexp, typerep]
  type unit      = Unit.        t [@@deriving bin_io, compare, hash, sexp, typerep]

  (* Bin_prot has optimized functions for float arrays *)
  type float_array = Bin_prot.Std.float_array [@@deriving bin_io]
  include (struct
    type float_array = Float.t array [@@deriving compare, sexp, typerep]
  end : sig
             type float_array [@@deriving compare, sexp, typerep]
           end with type float_array := float_array)
end : sig
           type 'a array  [@@deriving bin_io, compare,       sexp, typerep]
           type bool      [@@deriving bin_io, compare, hash, sexp, typerep]
           type char      [@@deriving bin_io, compare, hash, sexp, typerep]
           type float     [@@deriving bin_io, compare, hash, sexp, typerep]
           type int       [@@deriving bin_io, compare, hash, sexp, typerep]
           type int32     [@@deriving bin_io, compare, hash, sexp, typerep]
           type int64     [@@deriving bin_io, compare, hash, sexp, typerep]
           type 'a lazy_t [@@deriving bin_io, compare, hash, sexp, typerep]
           type 'a list   [@@deriving bin_io, compare, hash, sexp, typerep]
           type nativeint [@@deriving bin_io, compare, hash, sexp, typerep]
           type 'a option [@@deriving bin_io, compare, hash, sexp, typerep]
           type string    [@@deriving bin_io, compare, hash, sexp, typerep]
           type 'a ref    [@@deriving bin_io, compare,       sexp, typerep]
           type unit      [@@deriving bin_io, compare, hash, sexp, typerep]

           type float_array = float array [@@deriving bin_io, compare, sexp, typerep]
         end
with type 'a array    := 'a array
with type bool        := bool
with type char        := char
with type float       := float
with type int         := int
with type int32       := int32
with type int64       := int64
with type 'a list     := 'a list
with type nativeint   := nativeint
with type 'a option   := 'a option
with type string      := string
with type 'a lazy_t   := 'a lazy_t
with type 'a ref      := 'a ref
with type unit        := unit
)

let sexp_of_exn = Exn.sexp_of_t


(* The below declarations define converters for the special types recognized by pa-sexp.
   E.g. this allows the following to work:

   type t = { foo : int sexp_option } [@@deriving bin_io, compare, hash, sexp]

   [sexp_array], [sexp_bool], [sexp_list], and [sexp_option] allow a record field to be
   absent when converting from a sexp, and if absent, the field will take a default value
   of the appropriate type:

   {v
     sexp_array   [||]
     sexp_bool    false
     sexp_list    []
     sexp_option  None
   v}

   [sexp_opaque] causes the conversion to sexp to produce the atom {v <opaque> v}.

   For more documentation, see sexplib/README.md.
*)
type 'a sexp_array  = 'a array  [@@deriving bin_io, compare,       typerep]
type sexp_bool      = bool      [@@deriving bin_io, compare, hash, typerep]
type 'a sexp_list   = 'a list   [@@deriving bin_io, compare, hash, typerep]
type 'a sexp_option = 'a option [@@deriving bin_io, compare, hash, typerep]

type 'a sexp_opaque = 'a        [@@deriving bin_io, compare, hash, typerep]

(* The code below checks that the signatures in core_map.mli and core_set.mli are
   consistent with the generic map and set signatures defined in core_map_intf.ml
   and core_set_intf.ml. *)

let () =
  let module M : sig
    open Core_set_intf

    module Tree : sig
      type ('a, 'b) t

      include Creators_and_accessors2_with_comparator
        with type ('a, 'b) set  := ('a, 'b) t
        with type ('a, 'b) t    := ('a, 'b) t
        with type ('a, 'b) tree := ('a, 'b) t
    end

    type ('a, 'b) t

    include Accessors2
      with type ('a, 'b) t    := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) Tree.t

    include Creators2_with_comparator
      with type ('a, 'b) set  := ('a, 'b) t
      with type ('a, 'b) t    := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) Tree.t
  end = Set
  in
  ()

let () =
  let module M : sig
    open Core_map_intf

    module Tree : sig
      type ('a, 'b, 'c) t

      include Creators_and_accessors3_with_comparator
        with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
    end

    type ('a, 'b, 'c) t

    include Accessors3
      with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t

    include Creators3_with_comparator
      with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
  end = Map
  in
  ()

