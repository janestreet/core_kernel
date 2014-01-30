(** Basic types and definitions required throughout the system. *)
open Sexplib

(* pzimmer: use Bug when the condition is checkable but requires you to look further
   away *)
exception Bug of string

(** Raised when finalization after an exception failed, too.
    The first exception argument is the one raised by the initial
    function, the second exception the one raised by the finalizer. *)
exception Finally of exn * exn

(** For marking a given value as unimplemented.  Typically combined with conditional
    compilation, where on some platforms the function is defined normally, and on some
    platforms it is defined as unimplemented.  The supplied string should be the name of
    the function that is unimplemented. *)
val unimplemented : string -> _ Or_error.t

(** Types for use as markers in phantom types.  One should not expose functions for
    converting between read_only/immutable/read_write because the private types expose the
    subtyping. Users would say "(db :> read_only Db.t)" to cast.  The difference between
    read-only and immutable is that someone else can change a read-only object, while
    immutable never changes. *)
type read_only                      with sexp, bin_io, compare
type immutable  = private read_only with sexp, bin_io, compare
type read_write = private read_only with sexp, bin_io, compare

(** [never_returns] should be used as the return type of functions that don't return and
    might block forever, rather than ['a] or [_].  This forces callers of such functions
    to have a call to [never_returns] at the call site, which makes it clear to readers
    what's going on. We do not intend to use this type for functions such as [failwithf]
    that always raise an exception. *)
type never_returns = Never_returns.never_returns with sexp_of
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
val (|>) : 'a -> ('a -> 'b) -> 'b
val ident : 'a -> 'a
val const : 'a -> _ -> 'a
val (==>) : bool -> bool -> bool

val failwiths    :                    string -> 'a -> ('a -> Sexp.t) -> _
val failwithp    : Lexing.position -> string -> 'a -> ('a -> Sexp.t) -> _

val failwithf    : ('r, unit, string, unit -> _) format4 -> 'r
val invalid_argf : ('r, unit, string, unit -> _) format4 -> 'r

(** [does_raise f] returns [true] iff [f ()] raises. It is intended to be used in unit
    tests and should probably not be used in regular code. *)
val does_raise : (unit -> _) -> bool

(** [Or_error.ok_exn] *)
val ok_exn : 'a Or_error.t -> 'a

(** [Or_error.error] *)
val error : string -> 'a -> ('a -> Sexp.t) -> _ Or_error.t

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
type 'a return = private {
  return : 'b. 'a -> 'b;
}

val with_return : ('a return -> 'a) -> 'a

(** We disable [==] and [!=] and replace them with the longer and more mnemonic
    [phys_equal] because they too easily lead to mistakes (for example they don't even
    work right on Int64 or Float).  One can usually use the [equal] function for a
    specific type, or use (=) or (<>) for built in types like char, int, float,
*)
val phys_equal : 'a -> 'a -> bool
val (==) : [ `Consider_using_phys_equal ] -> [ `Consider_using_phys_equal ] -> [ `Consider_using_phys_equal ]
val (!=) : [ `Consider_using_phys_equal ] -> [ `Consider_using_phys_equal ] -> [ `Consider_using_phys_equal ]

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

(** {6 Deprecated operations}

  The following section contains definitions that hide operations from the standard
  library that are considered problematic or confusing, or simply redundant.
*)

(* override Pervasives methods that need LargeFile support *)
val seek_out : [ `Deprecated_use_out_channel ] -> [ `Deprecated_use_out_channel ] -> [ `Deprecated_use_out_channel ]
val pos_out : [ `Deprecated_use_out_channel ] -> [ `Deprecated_use_out_channel ]
val out_channel_length : [ `Deprecated_use_out_channel ] -> [ `Deprecated_use_out_channel ]
val seek_in : [ `Deprecated_use_in_channel ] -> [ `Deprecated_use_in_channel ] -> [ `Deprecated_use_in_channel ]
val pos_in : [ `Deprecated_use_in_channel ] -> [ `Deprecated_use_in_channel ]
val in_channel_length : [ `Deprecated_use_in_channel ] -> [ `Deprecated_use_in_channel ]
val modf : [ `Deprecated_use_float_modf ] -> [ `Deprecated_use_float_modf ]
val truncate : [ `Deprecated_use_float_iround_towards_zero ] -> [ `Deprecated_use_float_iround_towards_zero ]

(** we have our own version of these two, the INRIA version doesn't release the runtime
    lock.  *)
val close_in : [ `Deprecated_use_in_channel ] -> [ `Deprecated_use_in_channel ]
val close_out : [ `Deprecated_use_out_channel ] -> [ `Deprecated_use_out_channel ]

val ( & )  : [ `Deprecated_use_two_ampersands ] -> [ `Deprecated_use_two_ampersands ] -> [ `Deprecated_use_two_ampersands ]
(* val ( or ) : [ `Deprecated_use_pipe_pipe ] *)
val max_int : [ `Deprecated_use_int_module ]
val min_int : [ `Deprecated_use_int_module ]

val ceil            : [ `Deprecated_use__Float__round_up ] -> [ `Deprecated_use__Float__round_up ]
val floor           : [ `Deprecated_use__Float__round_down ] -> [ `Deprecated_use__Float__round_down ]
val abs_float       : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val mod_float       : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val frexp           : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val ldexp           : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val float_of_int    : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val max_float       : [ `Deprecated_use_float_module ]
val min_float       : [ `Deprecated_use_float_module ]
val epsilon_float   : [ `Deprecated_use_float_module ]
val classify_float  : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val string_of_float : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val float_of_string : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
val infinity        : [ `Deprecated_use_float_module ]
val neg_infinity    : [ `Deprecated_use_float_module ]
val nan             : [ `Deprecated_use_float_module ]
val int_of_float    : [ `Deprecated_use_float_module ] -> [ `Deprecated_use_float_module ]
type fpclass = [ `Deprecated_use_float_module ]

