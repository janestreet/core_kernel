open Common

type 'a t = 'a ref = { mutable contents : 'a }
[@@deriving bin_io, compare, sexp, typerep]

include Container.S1 with type 'a t := 'a t

val create : 'a -> 'a t

val (!) : 'a t -> 'a

val (:=) : 'a t -> 'a -> unit

(** [swap t1 t2] swaps the values in [t1] and [t2]. *)
val swap : 'a t -> 'a t -> unit

(** [replace t f] is [t := f !t] *)
val replace : 'a t -> ('a -> 'a) -> unit

(** [set_temporarily t a ~f] sets [t] to [a], calls [f ()], and then restores [t] to its
    value prior to [set_temporarily] being called, whether [f] returns or raises. *)
val set_temporarily : 'a t -> 'a -> f:(unit -> 'b) -> 'b

module Permissioned : sig
  type ('a, -'perms) t [@@deriving sexp, bin_io]

  include Container.S1_permissions
    with type ('a, 'perms) t := ('a, 'perms) t

  val create    : 'a -> ('a, [< _ perms]) t
  val read_only : ('a, [> read ]) t -> ('a, read) t

  (** [get] and [(!)] are two names for the same function. *)
  val (!)       : ('a, [> read ]) t -> 'a
  val get       : ('a, [> read ]) t -> 'a

  (** [set] and [(:=)] are two names for the same function. *)
  val set       : ('a, [> write ]) t -> 'a -> unit
  val (:=)      : ('a, [> write ]) t -> 'a -> unit

  val of_ref    : 'a ref -> ('a, [< read_write ]) t
  val to_ref    : ('a, [> read_write ]) t -> 'a ref

  val swap
    :  ('a, [> read_write ]) t
    -> ('a, [> read_write ]) t
    -> unit

  val replace : ('a, [> read_write ]) t -> ('a -> 'a) -> unit

  val set_temporarily
    :  ('a, [> read_write ]) t
    -> 'a
    -> f:(unit -> 'b)
    -> 'b
end
