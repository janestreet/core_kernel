(** A [Moption] is a mutable option, like ['a option ref], but with a more efficient
    implementation; in particular, [set_some] does not allocate. *)

open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : unit -> _ t

val is_none : _ t -> bool
val is_some : _ t -> bool

val get          : 'a t -> 'a option
val get_some_exn : 'a t -> 'a

val set : 'a t -> 'a option -> unit

val set_none : _  t ->       unit
val set_some : 'a t -> 'a -> unit
