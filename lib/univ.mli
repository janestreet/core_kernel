(** An extensible "universal" variant type.

    Every type id ([Type_equal.Id.t]) corresponds to one branch of the variant type.
*)

open Sexplib

type t with sexp_of

val type_id_name : t -> string

val create : 'a Type_equal.Id.t -> 'a -> t

(** [does_match t id] returns [true] iff [t] was created by [create id v]. *)
val does_match : t -> _ Type_equal.Id.t -> bool

(** [match_ t id] returns [Some v] if [t] was created by [create id v], and returns [None]
    otherwise.

    [match_exn t id] returns [v] if [t] was created by [create id v], and raises
    otherwise. *)
val match_    : t -> 'a Type_equal.Id.t -> 'a option
val match_exn : t -> 'a Type_equal.Id.t -> 'a
