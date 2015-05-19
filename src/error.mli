open Sexplib

include module type of Info

(* Note that the exception holds onto the [t]. *)
val raise : t -> _

val to_info : t -> Info.t
val of_info : Info.t -> t

(** [failwiths ?here message value sexp_of_value] raises an exception with the supplied
    [message] and [value], by constructing an [Error.t] and using [Error.raise].  As
    usual, the [sexp_of_value] is only applied when the value is converted to a sexp or a
    string.  So, if you mutate [value] in between the time you call [failwiths] and the
    time the error is displayed, those mutations will be reflected in the error message.

    [failwiths ?here s a f] = [Error.raise (Error.create ?here s a f)]

    The [pa_fail] preprocessor replaces [failwiths] with [failwiths ?here:_here_] so that
    one does not need to (and cannot) supply [_here_].  [pa_fail] does not add
    [?here:_here_] to [Error.failwiths].

    In this signature we write [?here:Lexing.position] rather than
    [?here:Source_code_position.t] to avoid a circular dependency.

    [failwithp here] is like [failwiths ~here], except that you can provide a source
    position yourself (which is only interesting if you don't provide [_here_]).
*)
val failwiths : ?here:Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _
val failwithp :       Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _

(** [Error.t] is NOT wire-compatible with [Error.Stable.V1.t].  See info.mli for
    details. *)
