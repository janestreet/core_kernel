(** OCaml's built in [bytes] type, currently equal to [string]. *)

type t = bytes

val create : int -> t
val length : t -> int
