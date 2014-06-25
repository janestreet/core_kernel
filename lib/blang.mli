
(** A simple boolean domain-specific language *)

(** Blang provides infrastructure for writing simple boolean DSLs.
    All expressions in a Blang language evaluate to a bool.  The language
    is parameterized over another language of base propositions.

    The syntax is almost exactly the obvious s-expression syntax,
    except that:

    1. Base elements are not marked explicitly.  Thus, if your base
       language has elements FOO, BAR, etc., then you could write
       the following Blang s-expressions:

       {v
          FOO
          (and FOO BAR)
          (if FOO BAR BAZ)
       v}

       and so on.  Note that this gets in the way of using the blang
       "keywords" in your value language.

    2. And and Or take a variable number of arguments, so that one can
       (and probably should) write

       {v (and FOO BAR BAZ QUX) v}

       instead of

       {v (and FOO (and BAR (and BAZ QUX))) v}
*)
open Std_internal

type 'a t = private
  | True
  | False
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Not of 'a t
  | If of 'a t * 'a t * 'a t
  | Base of 'a
with bin_io, compare, sexp
(** Note that the sexps are not directly inferred from the type above -- there are lots of
    fancy shortcuts.  Also, the sexps for ['a] must not look anything like blang sexps.
    Otherwise [t_of_sexp] will fail. *)

(* smart constructors that simplify away constants whenever possible *)
val base     : 'a -> 'a t
val true_    : _ t
val false_   : _ t
val constant : bool -> _ t (* [function true -> true_ | false -> false_] *)
val not_     : 'a t -> 'a t
val and_     : 'a t list -> 'a t (* n-ary [And] *)
val or_      : 'a t list -> 'a t (* n-ary [Or] *)
val if_      : 'a t -> 'a t -> 'a t -> 'a t (* [if_ if then else] *)

(* [constant_value t = Some b] iff [t = constant b] *)
val constant_value : 'a t -> bool option

(** The following two functions are useful when one wants to pretend
    that ['a t] has constructors And and Or of type ['a t list -> 'a t].
    The pattern of use is

    {[
      match t with
      | ...
      | And (_, _) as t -> let ts = gather_conjuncts t in ...
      | Or (_, _) as t -> let ts = gather_disjuncts t in ...
      | ...
    ]}

    or, in case you also want to handle True (resp. False) as a special
    case of conjunction (disjunction)

    {[
      match t with
      | ...
      | True | And (_, _) as t -> let ts = gather_conjuncts t in ...
      | False | Or (_, _) as t -> let ts = gather_disjuncts t in ...
      | ...
    ]}
*)
(** [gather_conjuncts t] gathers up all toplevel conjuncts in [t].  For example,
    {ul {- [gather_conjuncts (and_ ts) = ts] }
        {- [gather_conjuncts (And (t1, t2)) = gather_conjuncts t1 @ gather_conjuncts t2] }
        {- [gather_conjuncts True = [] ] }
        {- [gather_conjuncts t = [t]] when [t] matches neither [And (_, _)] nor [True] } }
*)
val gather_conjuncts : 'a t -> 'a t list

(** [gather_disjuncts t] gathers up all toplevel disjuncts in [t].  For example,
    {ul {- [gather_disjuncts (or_ ts) = ts] }
        {- [gather_disjuncts (Or (t1, t2)) = gather_disjuncts t1 @ gather_disjuncts t2] }
        {- [gather_disjuncts False = [] ] }
        {- [gather_disjuncts t = [t]] when [t] matches neither [Or (_, _)] nor [False] } }
*)
val gather_disjuncts : 'a t -> 'a t list

include Container.S1 with type 'a t := 'a t

(** [Blang.t] sports a substitution monad:
    {ul {- [return v] is [Base v] (think of [v] as a variable) }
        {- [bind t f] replaces every [Base v] in [t] with [f v]
           (think of [v] as a variable and [f] as specifying the term to
           substitute for each variable) } }

    Note: [bind t f] does short-circuiting, so [f] may not be called on every variable in
    [t]. *)
include Monad with type 'a t := 'a t

(** [values t] forms the list containing every [v]
    for which [Base v] is a subexpression of [t] *)
val values : 'a t -> 'a list

(** [eval t f] evaluates the proposition [t] relative to an environment
    [f] that assigns truth values to base propositions. *)
val eval : 'a t -> ('a -> bool) -> bool

(** [specialize t f] partially evaluates [t] according to a
    perhaps-incomplete assignment [f] of the values of base propositions.
    The following laws (at least partially) characterize its behavior.

    - [specialize t (fun _ -> `Unknown) = t]

    - [specialize t (fun x -> `Known (f x)) = constant (eval t f)]

    - [List.for_all (values (specialize t g)) ~f:(fun x -> g x = `Unknown)]

    - {[
        if
          List.for_all (values t) ~f:(fun x ->
            match g x with
            | `Known b -> b = f x
            | `Unknown -> true)
        then
          eval t f = eval (specialize t g) f
      ]}
*)
val specialize : 'a t -> ('a -> [`Known of bool | `Unknown]) -> 'a t

val invariant : 'a t -> unit

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a
    with sexp, bin_io, compare
  end
end
