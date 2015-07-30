(** An ['a Quickcheck_generator.t] a generates values of type ['a] with a specific
    probability distribution.
*)

open Std_internal

type 'a t   = 'a Raw_quickcheck_generator.t
type 'a obs = 'a Raw_quickcheck_observer.t

include module type of Raw_quickcheck_generator
  with type 'a t := 'a t

(** Generators form a monad.  [t1 >>= fun x -> t2] replaces each value [x] in [t1] with
    the values in [t2]; each value's probability is the product of its probability in
    [t1] and [t2].

    This can be used to form distributions of related values.  For instance, the following
    expression creates a distribution of pairs [x,y] where [x <= y]:

    {[
      int
      >>= fun x ->
      int_between
        ~lower_bound:(Incl x)
        ~upper_bound:(Incl Int.max_value)
      >>| fun y ->
      x, y
    ]}
*)
include Monad.S with type 'a t := 'a t

val unit   : unit   t
val bool   : bool   t
val int    : int    t
val float  : float  t
val string : string t
val char   : char   t
val sexp   : Sexp.t t

val char_digit      : char t
val char_lowercase  : char t
val char_uppercase  : char t
val char_alpha      : char t
val char_alphanum   : char t
val char_print      : char t
val char_whitespace : char t

val string_of : char t -> string t

val singleton : 'a -> 'a t
val doubleton : 'a -> 'a -> 'a t

(** Produce any of the given values, weighted equally.

    [of_list [ v1 ; ... ; vN ] = union [ singleton v1 ; ... ; singleton vN ]] *)
val of_list : 'a list -> 'a t

(** Combine arbitary generators, weighted equally.

    [ union [ g1 ; ... ; gN ] = weighted_union [ (1.0, g1) ; ... ; (1.0, gN) ] ] *)
val union : 'a t list -> 'a t

(** Generator for the values from a potentially infinite sequence.  Chooses each value
    with probability [p], or continues with probability [1-p].  Defaults to [p=0.25]. *)
val of_sequence : ?p:Percent.t -> 'a Sequence.t -> 'a t

val tuple2 : 'a t -> 'b t -> ('a * 'b) t
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val tuple6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> ('a * 'b * 'c * 'd * 'e * 'f) t

val either : 'a t -> 'b t -> ('a, 'b) Either.t t

val variant2 : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t
val variant3 : 'a t -> 'b t -> 'c t -> [ `A of 'a | `B of 'b | `C of 'c ] t
val variant4
  :  'a t -> 'b t -> 'c t -> 'd t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t
val variant5
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t
val variant6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

val option : 'a t -> 'a option t

(** [size] produces a geometric distribution (think "radioactive decay") starting at 0
    and increasing with probability 0.25.  It produces natural numbers and is weighted
    toward low values, making it a good default for, e.g., data structure sizes. *)
val size : int t

(** Generators for functions; take observers for inputs and a generator for outputs.  The
    observer splits the space of possible inputs into a number of "buckets"; each randomly
    generated function applies the observer to an input, and produces a different output
    value for each bucket.

    The [?branching_factor] argument determines how many branches the function's decision
    tree has, and thus how many categories of inputs it can distinguish.  If absent,
    [branching_factor] defaults to a geometric distribution similar to [size] but capped
    by the maximum branching factor of the domain observers. *)
val fn
  :  ?branching_factor:int t
  -> 'a obs -> 'b t
  -> ('a -> 'b) t
val fn2
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c t
  -> ('a -> 'b -> 'c) t
val fn3
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd t
  -> ('a -> 'b -> 'c -> 'd) t
val fn4
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e t
  -> ('a -> 'b -> 'c -> 'd -> 'e) t
val fn5
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f t
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f) t
val fn6
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f obs -> 'g t
  -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) t

(** Generator for comparison functions; result is guaranteed to be a partial order. *)
val compare_fn
  :  ?branching_factor:int t
  -> 'a obs
  -> ('a -> 'a -> int) t

(** Generator for equality functions; result is guaranteed to be an equivalence
    relation. *)
val equal_fn
  :  ?branching_factor:int t
  -> 'a obs
  -> ('a -> 'a -> bool) t

(** Generators for functions, annotated with sexps that describe the functions.  Intended
    for debugging purposes.  These generators each produce a [fn_with_sexp], whose
    description can be extracted using [fn_sexp]. *)
type ('a, 'b) fn_with_sexp = (('a -> 'b) * (unit -> Sexp.t)) with sexp_of
val fn_sexp : (_, _) fn_with_sexp -> Sexp.t

val compare_fn_with_sexp
  :  ?branching_factor:int t
  -> 'a obs
  -> ('a, 'a -> int) fn_with_sexp t
val equal_fn_with_sexp
  :  ?branching_factor:int t
  -> 'a obs
  -> ('a, 'a -> bool) fn_with_sexp t
val fn_with_sexp
  :  ?branching_factor:int t
  -> 'a obs -> 'b t
  -> sexp_of_rng:('b -> Sexp.t)
  -> ('a, 'b) fn_with_sexp t
val fn2_with_sexp
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c t
  -> sexp_of_rng:('c -> Sexp.t)
  -> ('a, 'b -> 'c) fn_with_sexp t
val fn3_with_sexp
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd t
  -> sexp_of_rng:('d -> Sexp.t)
  -> ('a, 'b -> 'c -> 'd) fn_with_sexp t
val fn4_with_sexp
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e t
  -> sexp_of_rng:('e -> Sexp.t)
  -> ('a, 'b -> 'c -> 'd -> 'e) fn_with_sexp t
val fn5_with_sexp
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f t
  -> sexp_of_rng:('f -> Sexp.t)
  -> ('a, 'b -> 'c -> 'd -> 'e -> 'f) fn_with_sexp t
val fn6_with_sexp
  :  ?branching_factor:int t
  -> 'a obs -> 'b obs -> 'c obs -> 'd obs -> 'e obs -> 'f obs -> 'g t
  -> sexp_of_rng:('g -> Sexp.t)
  -> ('a, 'b -> 'c -> 'd -> 'e -> 'f -> 'g) fn_with_sexp t

(** [filter_map t ~f] produces [y] for every [x] in [t] such that [f x = Some y].
    [filter t ~f] produces every [x] in [t] such that [f x = true].

    Caveat: Use [filter] and [filter_map] sparingly.  Every time [f] rejects a value, it
    counts as a failed attempt to produce a value.  Too many failures can cause Quickcheck
    to take a long time to generate values, or fail a test if it fails more times than the
    maximum configured number of attempts. *)
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val filter     : 'a t -> f:('a -> bool     ) -> 'a t

(** Fixed-point generator.  For example, the following produces a naive generator for
    natural numbers:

    {[
      recursive (fun t ->
        union [ singleton 0 ; t >>| Int.succ ])
    ]}
*)
val recursive : ('a t -> 'a t) -> 'a t

(** Generator for lists of a given type.

    [list t] produces a generator for arbitrary lists of values from [t].

    Adding [~unique:true] guarantees that every value from [t] is included in each list at
    most once.

    - Adding [~length:(`Exactly n)] produces only lists of length [n].
    - Adding [~length:(`At_least n)] produces only lists of length [n] or greater.
    - Adding [~length:(`At_most n)] produces only lists of length [n] or less.
    - Adding [~length:(`Between_inclusive (m,n))] produces only lists of length [k] such
      that [m <= k] and [k <= n].

    Adding [~sorted:`Arbitrarily] produces lists that are sorted in a deterministic order
    based on the construction of [t], but not guaranteed to correspond to any specific
    comparison function.

    Adding [~sorted:(`By cmp)] produces lists that are sorted in ascending order by [cmp].

    The optional arguments can be combined; for example, the following expression creates
    lists of 10 to 20 integers each that are strictly sorted (no duplicates):

    {[
      list int ~unique:true ~sorted:(`By Int.compare) ~length:(`Between_inclusive (10,20))
    ]}

    Regardless of the options provided, the lists in the output of [list t] are generated
    uniquely, so long as the values in [t] are generated uniquely. *)
val list
  :  ?length : [ `Exactly           of int
               | `At_least          of int
               | `At_most           of int
               | `Between_inclusive of int * int ]
  -> ?unique : bool
  -> ?sorted : [ `Arbitrarily | `By of ('a -> 'a -> int) ]
  -> 'a t
  -> 'a list t

(** [permute list] generates all permutations of [list].  If [list] contains duplicate
    values, then [permute list] will produce duplicate lists. *)
val permute : 'a list -> 'a list t

(** [int_between ~lower_bound ~upper_bound] produces integers within the given bounds.
    The distribution is *NOT* uniform, even when both ends are bounded; it is weighted
    toward potential boundary conditions. *)
val int_between
  :  lower_bound : int Comparable.bound
  -> upper_bound : int Comparable.bound
  -> int t

type nan_dist = Without | With_single | With_all

(** [float_between ~nan ~lower_bound ~upper_bound] produces floating point
    numbers within the given bounds.

    If [nan=Without], no values are produced satisfying [Float.is_nan].  If
    [nan=With_single], only the single value [Float.nan] is produced satisfying
    [Float.is_nan].  If [nan=With_all], the distribution includes all possible floating
    point values satisfying [Float.is_nan].

    The default [float] distribution is equivalent to
    [float_between ~nan:With_single ~lower_bound:`Unbounded ~upper_bound:`Unbounded]. *)
val float_between
  :  nan         : nan_dist
  -> lower_bound : float Comparable.bound
  -> upper_bound : float Comparable.bound
  -> float t

(** [float_without_nan] includes finite and infinite values, but excludes NaN. *)
val float_without_nan : float t

(** [float_finite] includes finite values, but excludes infinite and NaN values. *)
val float_finite : float t
