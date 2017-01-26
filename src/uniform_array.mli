(** Same semantics as ['a Array.t], except it's guaranteed that the representation array
    is not tagged with [Double_array_tag], the tag for float arrays.  This means it's
    safer to use in the presence of [Obj.magic], but it's slower than normal [Array] if
    you use it with floats.

    It can often be faster than [Array] if you use it with non-floats.
*)

open! Import

(** See [Core.Array] for comments. *)
type 'a t [@@deriving bin_io, sexp]


val empty : _ t

val create : len:int -> 'a -> 'a t

val init : int -> f:(int -> 'a) -> 'a t

val length : 'a t -> int

val get        : 'a t -> int -> 'a
val unsafe_get : 'a t -> int -> 'a

val set        : 'a t -> int -> 'a -> unit
val unsafe_set : 'a t -> int -> 'a -> unit

val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a -> unit) -> unit

val of_array : 'a array -> 'a t

(** [to_array t] returns a fresh array with the same contents as [t], rather than
    returning a reference to the underlying array. *)
val to_array : 'a t -> 'a array

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

include Blit.S1 with type 'a t := 'a t

val copy : 'a t -> 'a t

