(** An array of [Obj.t]s.

    This is useful to have as its own type because we can implement faster [get] and [set]
    functions that know that they aren't dealing with double arrays and save a test for
    it.  We also have [set] avoid the write barrier (caml_modify) in certain situations.
*)
type t

(** [create ~len] returns an obj-array of length [len], all of whose indices have value
    [Obj.repr 0]. *)
val create : len:int -> t

val singleton : Obj.t -> t

val empty : t

val length : t -> int

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

(** [sub t ~pos ~len] returns a new array containing the [len] elements of [t] starting at
    [pos]. *)
val sub : t -> pos:int -> len:int -> t

(** [copy t] returns a new array with the same elements as [t]. *)
val copy : t -> t

(** [blit] is like [Array.blit], except it uses our own for-loop to avoid caml_modify when
    possible.  It is still not comparable to a memcpy. *)
val blit        : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
val unsafe_blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit

(** [truncate t ~len] shortens [t]'s length to [len].  It is an error if [len <= 0] or
    [len > length t].*)
val truncate : t -> len:int -> unit
