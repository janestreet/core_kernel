(** See {!Quickcheck_observer} for an overview. *)

type 'a t

(** [branching_factor t] produces the number of nodes in the decision tree of [t], or one
    less than the number of buckets in [t].  This value is artificially capped at 2^15-1
    in order to avoid intractable function generators and overflow in arithmetic.  The
    result may be an overapproximation for observers constructed with [fn] or [of_fun]. *)
val branching_factor : _ t -> int

(** [observe t gen ~sexp_of_rng ~branching_factor] constructs a generator for a function
    type using [t] to observe the domain and [gen] to generate the range.  Each generated
    function also comes with a (lazy, unmemoized) sexp describing it.  The size of the
    function's decision tree is determined by [branching_factor] and the sexps of its
    return values are constructed by [sexp_of_rng].

    The functions in the resulting generator will all be intensionally unique: no two will
    make the same set of decisions in the same order.  However, as two such functions may
    make the same set of decisions in a different order, they will not be extensionally
    unique.  While it would be nice to have distributions of extensionally unique
    functions, implementing such a generator is quite difficult, and likely not worth the
    effort. *)
val observe
  :  'a t
  -> 'b Raw_quickcheck_generator.t
  -> sexp_of_rng:('b -> Sexplib.Sexp.t)
  -> branching_factor:int
  -> (('a -> 'b) * (unit -> Sexplib.Sexp.t)) Raw_quickcheck_generator.t

(** maps all values to a single bucket. *)
val singleton : unit -> _ t

(** [unmap t ~f ~f_sexp] applies [f] to values before observing them using [t]. *)
val unmap : 'a t -> f:('b -> 'a) -> f_sexp:(unit -> Sexplib.Sexp.t) -> 'b t

(** Nondeterministic observer.  Presents a weighted choice of multiple observers.  When
    [observe] builds a decision tree, it randomly chooses nodes from any of these
    observers with probability proportional to the given weights.  All weights must be
    finite and non-negative. *)
val weighted_union : (float * 'a t) list -> 'a t

val variant2 : 'a t -> 'b t -> [ `A of 'a | `B of 'b ] t

val tuple2 : 'a t -> 'b t -> ('a * 'b) t
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
val tuple6
  :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
  -> ('a * 'b * 'c * 'd * 'e * 'f) t

(** Observer for function type.  [fn ~p gen t ~sexp_of_dom] observes a function by
    generating random inputs from [gen], applying the function, and observing the output
    using [t].

    When [observe] builds a single random decision tree node from the result of [fn], it
    chooses between generating a new input and observing a previously generated input.  It
    does the former with probability [p] and the latter with probability [1. -. p].

    [p] defaults to 0.25, and must be between 0 and 1 (both inclusive). *)
val fn
  :  ?p:float
  -> 'a Raw_quickcheck_generator.t
  -> 'b t
  -> sexp_of_dom:('a -> Sexplib.Sexp.t)
  -> ('a -> 'b) t

(** [of_fun f] produces an observer that lazily applies [f].

    It is recommended that [f] should not do a lot of expensive work and should not be
    memoized.  Instead, spread out the work of generating an observer over many [of_fun]
    calls combined with, e.g., [variant] or [tuple].  This allows lazily generated
    observers to be garbage collected after each test and the relevant portions cheaply
    recomputed in subsequent tests, rather than accumulating without bound over time. *)
val of_fun : (unit -> 'a t) -> 'a t
