open Sexplib

include module type of Info with type t = private Info.t

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

    When compiling with pa_fail (the default at jane street) the [?here] argument is
    automatically filled in with the value [_here_], and so cannot be passed manually.

    We say [?here:Lexing.position] rather than [?here:Source_code_position.t] to avoid a
    circular dependency.

    [failwithp here] is like [failwiths ~here], except that you can provide a source
    position yourself (which is only interesting if you don't provide [_here_]).
*)
val failwiths : ?here:Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _
val failwithp :       Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _
