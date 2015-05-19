(** Module types and utilities for dealing with types that support the bin-io binary
    encoding. *)

open Bin_prot.Binable
open Bigarray

(* We copy the definition of the bigstring type here, because we cannot depend on
   bigstring.ml *)
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

module type S = S
module type S1 = S1
module type S2 = S2

(** [Of_binable*] functors are for when you want the binary representation of one type to
    be the same as that for some other isomorphic type. *)

module Of_binable
    (Binable : S)
    (M : sig
       type t
       val to_binable : t -> Binable.t
       val of_binable : Binable.t -> t
     end)
  : S with type t := M.t

module Of_binable1
    (Binable : S1)
    (M : sig
       type 'a t
       val to_binable : 'a t -> 'a Binable.t
       val of_binable : 'a Binable.t -> 'a t
     end)
  : S1 with type 'a t := 'a M.t

module Of_binable2
    (Binable : S2)
    (M : sig
       type ('a, 'b) t
       val to_binable : ('a, 'b) t -> ('a, 'b) Binable.t
       val of_binable : ('a, 'b) Binable.t -> ('a, 'b) t
     end)
  : S2 with type ('a, 'b) t := ('a, 'b) M.t

module Of_stringable (M : Stringable.S) : S with type t := M.t

type 'a m = (module S with type t = 'a)

val of_bigstring : 'a m -> bigstring -> 'a

val to_bigstring
  :  ?prefix_with_length:bool (* defaults to false *)
  -> 'a m
  -> 'a
  -> bigstring

val of_string : 'a m -> string -> 'a
val to_string : 'a m -> 'a -> string
