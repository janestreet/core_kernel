open! Import
include Bin_prot.Std
include Hash.Builtin

(* Constrain Sexplib.Std to remove the Hashtbl module (and some other modules we don't
   care about, e.g. Ratio), which would later conflict with Hashtbl in
   Stable_containers. *)
include
  (Sexplib.Std : sig
     type nonrec 'a array  = 'a array  [@@deriving sexp]
     type nonrec bool      = bool      [@@deriving sexp]
     type nonrec char      = char      [@@deriving sexp]
     type nonrec exn       = exn       [@@deriving sexp_of]
     type nonrec float     = float     [@@deriving sexp]
     type nonrec 'a list   = 'a list   [@@deriving sexp]
     type nonrec 'a option = 'a option [@@deriving sexp]
     type nonrec string    = string    [@@deriving sexp]
     type nonrec 'a ref    = 'a ref    [@@deriving sexp]
     type nonrec unit      = unit      [@@deriving sexp]
   end
   with type 'a array  := 'a array
   with type bool      := bool
   with type char      := char
   with type exn       := exn
   with type float     := float
   with type 'a list   := 'a list
   with type 'a option := 'a option
   with type string    := string
   with type 'a ref    := 'a ref
   with type unit      := unit)

include
  (Std_internal : sig
     type nonrec int       = int       [@@deriving sexp]
     type nonrec int32     = int32     [@@deriving sexp]
     type nonrec int64     = int64     [@@deriving sexp]
     type nonrec nativeint = nativeint [@@deriving sexp]
   end
   with type int         := int
   with type int32       := int32
   with type int64       := int64
   with type nativeint   := nativeint)

type 'a sexp_option = 'a Std_internal.sexp_option [@@deriving bin_io, compare, hash]
type 'a sexp_list   = 'a Std_internal.sexp_list   [@@deriving bin_io, compare, hash]

(* Hack, because Sexp isn't Binable *)
module Sexp = struct
  type t = Sexplib.Sexp.t = Atom of string | List of t list [@@deriving bin_io, compare, hash]
  include (Sexplib.Sexp : module type of Sexplib.Sexp with type t := t)
end
