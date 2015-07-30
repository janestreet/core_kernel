
(** An ['a Quickcheck.Observer.t] represents observations that can be made to distinguish
    values of type ['a].  An observer maps values of type ['a] to disjoint subsets
    ("buckets") using a finite number of observations.

    Observers are used to construct distributions of random functions; see
    [Quickcheck.Generator.fn].

    One constructs an observer by breaking down an input into basic type constituents that
    can be individually observed.  Use built-in observers for basic types when possible.
    Use [either] or the [variant*] observers to distinguish clauses of variants.  Use the
    [tuple*] observers to get at individual fields of tuples or records.  When you have a
    custom type with no built-in observer, construct an observer for an equivalent type,
    then use [unmap].  Use [recursive] to build observers for recursive types.  See the
    below example for a binary search tree:

    {[
      type 'a bst = Leaf | Node of 'a bst * 'a * 'a bst

      let bst_obs key_obs =
        recursive (fun bst_of_key_obs ->
          unmap (either unit (tuple3 bst_of_key_obs key_obs bst_of_key_obs))
            ~f:(function
              | Leaf           -> First ()
              | Node (l, k, r) -> Second (l, k, r))
            ~f_sexp:(fun () -> Sexp.Atom "either_of_bst"))
    ]}
*)

open Std_internal

type 'a t   = 'a Raw_quickcheck_observer.t
type 'a gen = 'a Raw_quickcheck_generator.t

include module type of Raw_quickcheck_observer
  with type 'a t := 'a t

(** Observers for basic types. *)
val unit   : unit   t
val bool   : bool   t
val int    : int    t
val float  : float  t
val string : string t
val char   : char   t
val sexp   : Sexp.t t

(** [doubleton f ~f_sexp] maps values to two "buckets" (as described in [t] above),
    depending on whether they satisfy [f].  [f_sexp] should describe [f]. *)
val doubleton : ('a -> bool) -> f_sexp:(unit -> Sexp.t) -> 'a t

(** [enum n ~f] maps values to [n] buckets, where [f] produces the index for a bucket
    from [0] to [n-1] for each value. *)
val enum : int -> f:('a -> int) -> f_sexp:(unit -> Sexp.t) -> 'a t

(** [of_list list ~equal] maps values in [list] to separate buckets, and compares
    observed values to the elements of [list] using [equal]. *)
val of_list : 'a list -> equal:('a -> 'a -> bool) -> sexp_of_elt:('a -> Sexp.t) -> 'a t

val option : 'a t -> 'a option t
val list   : 'a t -> 'a list   t

(** Fixed point observer; use [recursive] to create observers for recursive types.  For
    example:

    {[
      let sexp_obs =
        recursive (fun sexp_t ->
          unmap (variant2 string (list sexp_t))
            ~f:(function
              | Sexp.Atom atom -> `A atom
              | Sexp.List list -> `B list)
            ~f_sexp:(fun () -> Sexp.Atom "variant_of_sexp"))
    ]}
*)
val recursive : ('a t -> 'a t) -> 'a t

val either : 'a t -> 'b t -> ('a, 'b) Either.t t

val variant3
  :  'a t -> 'b t -> 'c t
  -> [ `A of 'a | `B of 'b | `C of 'c ] t
val variant4
  :  'a t -> 'b t -> 'c t -> 'd t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t
val variant5
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t
val variant6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

(** [of_predicate t1 t2 ~f] combines [t1] and [t2], where [t1] observes values that
    satisfy [f] and [t2] observes values that do not satisfy [f]. *)
val of_predicate : 'a t -> 'a t -> f:('a -> bool) -> f_sexp:(unit -> Sexp.t) -> 'a t

(** [comparison ~compare ~eq ~lt ~gt] combines observers [lt] and [gt], where [lt]
    observes values less than [eq] according to [compare], and [gt] observes values
    greater than [eq] according to [compare]. *)
val comparison
  :  compare:('a -> 'a -> int)
  -> eq:'a
  -> lt:'a t
  -> gt:'a t
  -> compare_sexp:(unit -> Sexp.t)
  -> sexp_of_eq:('a -> Sexp.t)
  -> 'a t

(** [int_between ~lower_bound ~upper_bound] observes integers within the given bounds. *)
val int_between
  :  lower_bound:int Comparable.bound
  -> upper_bound:int Comparable.bound
  -> int t
