open Sexplib

include Info_intf.S

(** Note that the exception raised by this function maintains a reference to the [t]
    passed in. *)
val raise : t -> _

val raise_s : Sexp.t -> _

val to_info : t -> Info.t
val of_info : Info.t -> t

(** {[
      failwiths ?strict ?here message a sexp_of_a
      = Error.raise (Error.create ?strict ?here s a sexp_of_a)
    ]}

   As with [Error.create], [sexp_of_a a] is lazily computed, when the error is converted
   to a sexp.  So, if [a] is mutated in the time between the call to [failwiths] and the
   sexp conversion, those mutations will be reflected in the error message.  Use
   [~strict:()] to force [sexp_of_a a] to be computed immediately.

   The [pa_fail] preprocessor replaces [failwiths] with [failwiths ?here:[%here]] so that
   one does not need to (and cannot) supply [[%here]].  [pa_fail] does not add
   [?here:[%here]] to [Error.failwiths].

   In this signature we write [?here:Lexing.position] rather than
   [?here:Source_code_position.t] to avoid a circular dependency.

   [failwithp here] is like [failwiths ~here], except that you can provide a source
   position yourself (which is only interesting if you don't provide [[%here]]). *)
val failwiths
  :  ?strict : unit
  -> ?here   : Lexing.position
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _

val failwithp
  :  ?strict : unit
  -> Lexing.position
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _

(** [Error.t] is NOT wire-compatible with [Error.Stable.V1.t].  See info.mli for
    details. *)
