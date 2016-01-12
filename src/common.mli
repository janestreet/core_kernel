(** Basic types and definitions required throughout the system. *)
open Sexplib

include module type of Core_pervasives

exception Bug of string

(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of exn * exn

(** Types for expressing read-write permissions in phantom types.  See the [Perms] module
    for details. *)
include module type of Perms.Export

(** [never_returns] should be used as the return type of functions that don't return and
    might block forever, rather than ['a] or [_].  This forces callers of such functions
    to have a call to [never_returns] at the call site, which makes it clear to readers
    what's going on. We do not intend to use this type for functions such as [failwithf]
    that always raise an exception. *)
type never_returns = Never_returns.never_returns [@@deriving sexp_of]
val never_returns : never_returns -> _

(** {6 Error handling} *)
(** See exn.mli *)
val protect  : f:(unit -> 'a)       -> finally:(unit -> unit) -> 'a
val protectx : f:('b   -> 'a) -> 'b -> finally:('b   -> unit) -> 'a

(** {6 Input Output}*)

(**{6 triple handling }*)
val fst3 : ('a * _  * _ ) -> 'a
val snd3 : (_  * 'a * _ ) -> 'a
val trd3 : (_  * _  * 'a) -> 'a

(**
   {6 Option handling}
*)
val uw : 'a option -> 'a

val is_none : 'a option -> bool
val is_some : 'a option -> bool

(** {6 Functions from fn.ml} *)
val (|!) : 'a -> ('a -> 'b) -> 'b

val ident : 'a -> 'a
val const : 'a -> _ -> 'a
val (==>) : bool -> bool -> bool

(** [Error.failwiths] *)
val failwiths
  :  ?strict : unit
  -> ?here:Lexing.position
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _

(** [Error.failwithp] *)
val failwithp
  :  ?strict : unit
  -> Lexing.position
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _

val failwithf    : ('r, unit, string, unit -> _) format4 -> 'r
val invalid_argf : ('r, unit, string, unit -> _) format4 -> 'r

(** [Error.raise_s] *)
val raise_s : Sexp.t -> _

(** [Or_error.ok_exn] *)
val ok_exn : 'a Or_error.t -> 'a

(** [Or_error.error] *)
val error
  :  ?strict : unit
  -> string
  -> 'a
  -> ('a -> Sexp.t)
  -> _ Or_error.t

(** [Or_error.error_s] *)
val error_s : Sexp.t -> _ Or_error.t

(** [with_return f] allows for something like the return statement in C within [f].  There
   are three ways [f] can terminate:

   1. If [f] calls [r.return x], then [x] is returned by [with_return].
   2. If [f] evaluates to a value [x], then [x] is returned by [with_return].
   3. If [f] raises an exception, it escapes [with_return].

   Here is a typical example:

   {[
   let find l ~f =
     with_return (fun r ->
        List.iter l ~f:(fun x -> if f x then r.return (Some x));
        None
      )
   ]}

   It is only because of a deficiency of ML types that [with_return] doesn't have type:

   {[ val with_return : 'a. (('a -> ('b. 'b)) -> 'a) -> 'a ]}

   but we can slightly increase the scope of 'b, without changing the meaning of the type
   and then we get

   {[
   type 'a return = { return : 'b . 'a -> 'b }
   val with_return : ('a return -> 'a) -> 'a
   ]}

   But the actual reason we chose to use a record type with polymorphic field is that
   otherwise we would have to clobber the namespace of functions with [return] and that is
   undesirable because [return] would get hidden as soon as we open any monad. We
   considered names different than [return] but everything seemed worse than just having
   [return] as a record field. We are clobbering the namespace of record fields but that
   is much more acceptable.
*)
type -'a return = 'a With_return.return = private {
  return : 'b. 'a -> 'b;
}

val with_return        : ('a return -> 'a)   -> 'a
val with_return_option : ('a return -> unit) -> 'a option

(** We disable [==] and [!=] and replace them with the longer and more mnemonic
    [phys_equal] because they too easily lead to mistakes (for example they don't even
    work right on Int64 or Float).  One can usually use the [equal] function for a
    specific type, or use (=) or (<>) for built in types like char, int, float.

    Note that 4.02 increased cases where objects are physically equal.
*)
val phys_equal : 'a -> 'a -> bool

(** [phys_same] is like [phys_equal], but with a more general type.  [phys_same] is useful
    when dealing with existential types, when one has a packed value and an unpacked value
    that one wants to check are physically equal.  One can't use [phys_equal] in such a
    situation because the types are different. *)
val phys_same : _ -> _ -> bool

val force : 'a Lazy.t -> 'a

(** See {! module : Staged } for documentation *)
val stage   : 'a -> 'a Staged.t
val unstage : 'a Staged.t -> 'a

(** Raised if malloc in C bindings fail (errno * size). *)
exception C_malloc_exn of int * int

