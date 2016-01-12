(** An array of [Obj.t]s.

    This is useful to have as its own type because we can implement faster [get] and [set]
    functions that know that they aren't dealing with float arrays and save a test for it.
    We also have [set] avoid the write barrier (caml_modify) in certain situations.

    Just like with a regular [Array], the elements are boxed so they don't get copied by
    [sub], [get], [set], [blit], etc.

    The dynamic check this array module implements is something we hope to have
    implemented at a lower level (as part of the native compiler's code generation).
    Given that, the interface is somewhat spartan and intended for use within internal
    data structures.
*)

type t [@@deriving sexp_of]

include Blit.     S with type t := t
include Invariant.S with type t := t

(** [create ~len] returns an obj-array of length [len], all of whose indices have value
    [Obj.repr 0]. *)
val create : len:int -> t

(** [copy t] returns a new array with the same elements as [t]. *)
val copy : t -> t

val singleton : Obj.t -> t

val empty : t

val length : t -> int

(** [get t i] and [unsafe_get t i] return the object at index [i].  [set t i o] and
    [unsafe_set t i o] set index [i] to [o].  In no case is the object copied.  The
    [unsafe_*] variants omit the bounds check of [i]. *)
val get        : t -> int -> Obj.t
val unsafe_get : t -> int -> Obj.t
val set        : t -> int -> Obj.t -> unit
val unsafe_set : t -> int -> Obj.t -> unit

(** [unsafe_set_assuming_currently_int t i obj] sets index [i] of [t] to [obj], but only
    works correctly if [Obj.is_int (get t i)].  This precondition saves a dynamic
    check.

    [unsafe_set_int_assuming_currently_int] is similar, except the value being set is an
    int. *)
val unsafe_set_assuming_currently_int     : t -> int -> Obj.t -> unit
val unsafe_set_int_assuming_currently_int : t -> int -> int   -> unit

(** [unsafe_clear_if_pointer t i] prevents [t.(i)] from pointing to anything to prevent
    space leaks.  It does this by setting [t.(i)] to [Obj.repr 0].  As a performance hack,
    it only does this when [not (Obj.is_int t.(i))]. *)
val unsafe_clear_if_pointer : t -> int -> unit

(** [truncate t ~len] shortens [t]'s length to [len].  It is an error if [len <= 0] or
    [len > length t].*)
val truncate : t -> len:int -> unit
