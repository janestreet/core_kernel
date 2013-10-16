open Sexplib

include module type of Info with type t = private Info.t

(* Note that the exception holds onto the [t]. *)
val raise : t -> _

val to_info : t -> Info.t
val of_info : Info.t -> t

(** [failwiths message value sexp_of_value] raises an exception with the supplied
    [message] and [value], by constructing an [Error.t] and using [Error.raise].  As
    usual, the [sexp_of_value] is only applied when the value is converted to a sexp or a
    string.  So, if you mutate [value] in between the time you call [failwiths] and the
    time the error is displayed, those mutations will be reflected in the error message.

    [failwiths s a f] = [Error.raise (Error.create s a f)]

    [failwithp _here_ s a f] is just [failwiths] with the source code position tagged on.
    We use [Lexing.position] rather than [Source_code_position.t] to avoid a circular
    dependency.
*)
val failwiths :                    string -> 'a -> ('a -> Sexp.t) -> _
val failwithp : Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _

