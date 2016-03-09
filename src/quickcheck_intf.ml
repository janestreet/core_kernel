module Sexp = Sexplib.Sexp

(** For Quickcheck overview see: http://docs/programming/unit-testing/quickcheck.html *)

module type Generator = sig

  (** An ['a t] a generates values of type ['a] with a specific probability
      distribution. *)
  type 'a t
  type 'a obs

  (** Generators form a monad.  [t1 >>= fun x -> t2] replaces each value [x] in [t1] with
      the values in [t2]; each value's probability is the product of its probability in
      [t1] and [t2].

      This can be used to form distributions of related values.  For instance, the
      following expression creates a distribution of pairs [x,y] where [x <= y]:

      {[
        Int.gen
        >>= fun x ->
        Int.gen_between
          ~lower_bound:(Incl x)
          ~upper_bound:(Incl Int.max_value)
        >>| fun y ->
        x, y
      ]}
  *)
  include Monad.S with type 'a t := 'a t

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
  val of_sequence : ?p:float -> 'a Sequence.t -> 'a t

  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val tuple6
    :  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
    -> ('a * 'b * 'c * 'd * 'e * 'f) t

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

  (** [size] produces a geometric distribution (think "radioactive decay") starting at 0
      and increasing with probability 0.75.  It produces natural numbers and is weighted
      toward low values, making it a good default for, e.g., data structure sizes. *)
  val size : int t

  (** Generators for functions; take observers for inputs and a generator for outputs.
      The observer splits the space of possible inputs into a number of "buckets"; each
      randomly generated function applies the observer to an input, and produces a
      different output value for each bucket.

      The [?branching_factor] argument determines how many branches the function's
      decision tree has, and thus how many categories of inputs it can distinguish.  If
      absent, [branching_factor] defaults to a geometric distribution similar to [size]
      but capped by the maximum branching factor of the domain observers. *)
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

  (** Generators for functions, annotated with sexps that describe the functions.
      Intended for debugging purposes.  These generators each produce a [fn_with_sexp],
      whose description can be extracted using [fn_sexp]. *)
  type ('a, 'b) fn_with_sexp = ('a -> 'b) * (unit -> Sexp.t) [@@deriving sexp_of]
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
      counts as a failed attempt to produce a value.  Too many failures can cause
      Quickcheck to take a long time to generate values, or fail a test if it fails more
      times than the maximum configured number of attempts. *)
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

  (** ['a Choice.t] represents the choice of a value from a generator.  It also
      encapsulates the history of which random decisions were made to reach that value,
      including all choices along the way that failed to produce a value. *)
  module Choice : sig

    type 'a gen = 'a t
    type 'a t

    (** [original_gen t] produces the original generator from which [t] was constructed,
        unmodified. *)
    val original_gen : 'a t -> 'a gen

    (** [updated_gen t ~keep] produces a generator representing a subset of the
        probability distribution from which [t] was constructed.

        The subset determined by [keep] refers to notions of "left" and "right" with
        respect to choices in a [gen].  For this purpose, a generator can be thought of as
        a potentially infinite sequence of weighted choices, and operations on generators
        preserve the order of this sequence.  The [keep] argument determines where to
        "cut" this sequence relative to the current choice.  This property can be used to
        construct generators that do not repeat choices, or do not repeat the same set of
        choices in a different order, which is useful, e.g., for constructing lists of
        unique values or unique sets of values.

        In all variants of [keep], any failed choices that were made in constructing [t]
        are discarded.

        Do not assume that a generator has any particular order: for example, do not count
        on an [int] generator being in strictly ascending order. *)
    val updated_gen
      :  'a t
      -> keep : [ `All_choices
                | `All_choices_except_this_choice
                | `Choices_to_the_left_of_this_choice_only
                | `Choices_to_the_right_of_this_choice_only
                | `This_choice_and_all_choices_to_the_left
                | `This_choice_and_all_choices_to_the_right
                ]
      -> 'a gen

    (** [value t] produces the value chosen from [original_gen t]. *)
    val value : 'a t -> 'a

    (** [attempts_used t] reports the number of attempts made to choose a value.  It is
        always at least 1, since there must have been one successful attempt; it may be
        higher to indicate failed attempts. *)
    val attempts_used : 'a t -> int

  end with type 'a gen := 'a t

  (** [bind_choice t f] is like [bind t f], except [f] is passed an ['a Choice.t] and can
      thus use subsets of [t] by using [Choice.gen] with the [~keep] option.

      [bind t f] is equal to [bind_choice t (fun c -> f (Choice.value c))], although
      [bind] is cheaper than [bind_choice]. *)
  val bind_choice : 'a t -> ('a Choice.t -> 'b t) -> 'b t

  (** Empty generator that is guaranteed to fail to produce a value. *)
  val failure : _ t

  (** [weighted_union alist] produces a generator that combines the distributions of each
      [t] in [alist] with the associated weights, which must be finite positive floating
      point values. *)
  val weighted_union : (float * 'a t) list -> 'a t

  (** [of_fun f] produces a generator that lazily applies [f].

      [f] *MUST* be deterministic or else random value generation will fail.  However, it
      is recommended that [f] not be memoized.  Instead, spread out the work of generating
      a whole distribution over many [of_fun] calls combined with [weighted_union].  This
      allows lazily generated generators to be garbage collected after each test and the
      relevant portions cheaply recomputed in subsequent tests, rather than accumulating
      without bound over time. *)
  val of_fun : (unit -> 'a t) -> 'a t

  (** [choose t ~random_float_between_zero_and_one] makes a choice in [t] at random, using
      [random_float_between_zero_and_one] to produce random floats between 0. (inclusive)
      and 1. (exclusive) for each weighted choice.  If [t] has been fully explored, it
      produces [`No_choices_remain].  Otherwise it produces [`Choice c] for some choice
      [c] in [t]. *)
  val choose
    :  'a t
    -> random_float_between_zero_and_one : (unit -> float)
    -> max_attempts                      : int
    -> [ `No_choices_remain
       | `Ran_out_of_attempts
       | `Choice of 'a Choice.t
       ]

  (** [inspect t] produces a concrete representation of the outermost constructor of [t].
      It is possible to explore [t] further with recursive calls to [inspect]; however, be
      aware that [t] may be infinite. *)
  val inspect
    :  'a t
    -> [ `Failure
       | `Singleton of 'a
       | `Weighted_union of (float * 'a t) list
       ]

end

module type Observer = sig


  (** An ['a Quickcheck.Observer.t] represents observations that can be made to
      distinguish values of type ['a].  An observer maps values of type ['a] to disjoint
      subsets ("buckets") using a finite number of observations.

      Observers are used to construct distributions of random functions; see
      [Quickcheck.Generator.fn].

      One constructs an observer by breaking down an input into basic type constituents
      that can be individually observed.  Use built-in observers for basic types when
      possible.  Use [either] or the [variant*] observers to distinguish clauses of
      variants.  Use the [tuple*] observers to get at individual fields of tuples or
      records.  When you have a custom type with no built-in observer, construct an
      observer for an equivalent type, then use [unmap].  Use [recursive] to build
      observers for recursive types.  See the below example for a binary search tree:

      {[
        type 'a bst = Leaf | Node of 'a bst * 'a * 'a bst

        let bst_obs key_obs =
          recursive (fun bst_of_key_obs ->
            unmap (Either.obs Unit.obs (tuple3 bst_of_key_obs key_obs bst_of_key_obs))
              ~f:(function
                | Leaf           -> First ()
                | Node (l, k, r) -> Second (l, k, r))
              ~f_sexp:(fun () -> Sexp.Atom "either_of_bst"))
      ]}
  *)
  type 'a t
  type 'a gen

  (** [doubleton f ~f_sexp] maps values to two "buckets" (as described in [t] above),
      depending on whether they satisfy [f].  [f_sexp] should describe [f]. *)
  val doubleton : ('a -> bool) -> f_sexp:(unit -> Sexp.t) -> 'a t

  (** [enum n ~f] maps values to [n] buckets, where [f] produces the index for a bucket
      from [0] to [n-1] for each value. *)
  val enum : int -> f:('a -> int) -> f_sexp:(unit -> Sexp.t) -> 'a t

  (** [of_list list ~equal] maps values in [list] to separate buckets, and compares
      observed values to the elements of [list] using [equal]. *)
  val of_list
    :  'a list
    -> equal:('a -> 'a -> bool)
    -> sexp_of_elt:('a -> Sexp.t)
    -> 'a t

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
  val of_predicate
    :  'a t
    -> 'a t
    -> f:('a -> bool)
    -> f_sexp:(unit -> Sexp.t)
    -> 'a t

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

  (** [branching_factor t] produces the number of nodes in the decision tree of [t], or
      one less than the number of buckets in [t].  This value is artificially capped at
      2^15-1 in order to avoid intractable function generators and overflow in arithmetic.
      The result may be an overapproximation for observers constructed with [fn] or
      [of_fun]. *)
  val branching_factor : _ t -> int

  (** [observe t gen ~sexp_of_rng ~branching_factor] constructs a generator for a function
      type using [t] to observe the domain and [gen] to generate the range.  Each
      generated function also comes with a (lazy, unmemoized) sexp describing it.  The
      size of the function's decision tree is determined by [branching_factor] and the
      sexps of its return values are constructed by [sexp_of_rng].

      The functions in the resulting generator will all be intensionally unique: no two
      will make the same set of decisions in the same order.  However, as two such
      functions may make the same set of decisions in a different order, they will not be
      extensionally unique.  While it would be nice to have distributions of extensionally
      unique functions, implementing such a generator is quite difficult, and likely not
      worth the effort. *)
  val observe
    :  'a t
    -> 'b gen
    -> sexp_of_rng:('b -> Sexp.t)
    -> branching_factor:int
    -> (('a -> 'b) * (unit -> Sexp.t)) gen

  (** maps all values to a single bucket. *)
  val singleton : unit -> _ t

  (** [unmap t ~f ~f_sexp] applies [f] to values before observing them using [t]. *)
  val unmap : 'a t -> f:('b -> 'a) -> f_sexp:(unit -> Sexp.t) -> 'b t

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
      chooses between generating a new input and observing a previously generated input.
      It does the former with probability [p] and the latter with probability [1. -. p].

      [p] defaults to 0.25, and must be between 0 and 1 (both inclusive). *)
  val fn
    :  ?p:float
    -> 'a gen
    -> 'b t
    -> sexp_of_dom:('a -> Sexp.t)
    -> ('a -> 'b) t

  (** [of_fun f] produces an observer that lazily applies [f].

      It is recommended that [f] should not do a lot of expensive work and should not be
      memoized.  Instead, spread out the work of generating an observer over many [of_fun]
      calls combined with, e.g., [variant] or [tuple].  This allows lazily generated
      observers to be garbage collected after each test and the relevant portions cheaply
      recomputed in subsequent tests, rather than accumulating without bound over time. *)
  val of_fun : (unit -> 'a t) -> 'a t

end

module type Shrinker = sig
  (** A ['a Quickcheck.Shrinker.t] takes a value of type ['a] and produces similar values
      that are smaller by some metric.

      The defined shrinkers generally try to make a single change for each value based on
      the assumption that the first resulting value that preserves the desired property
      will be used to create another sequence of shrunk values.

      Within [Quickcheck.test] the shrinker is used as described above.

      Shrinkers aim to aid understanding of what's causing an error by reducing the input
      down to just the elements making it fail.  The default shrinkers remove elements of
      compound structures, but leave atomic values alone.  For example, the default list
      shrinker tries removing elements from the list, but the default int shrinker does
      nothing.  This default strikes a balance between performance and precision.
      Individual tests can use different shrinking behavior as necessary.

      See lib/core/example/quickcheck/shrinker_example.ml for some example shrinkers.
  *)

  type 'a t

  val shrink : 'a t -> 'a -> 'a Sequence.t

  val create : ('a -> 'a Sequence.t) -> 'a t

  val empty : unit -> 'a t

  val map : 'a t -> f:('a -> 'b) -> f_inverse:('b -> 'a) -> 'b t

  val tuple2 : 'a t -> 'b t -> ('a * 'b) t
  val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
  val tuple5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
  val tuple6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val variant2
    :  'a t
    -> 'b t
    -> [ `A of 'a | `B of 'b ] t

  val variant3
    :  'a t
    -> 'b t
    -> 'c t
    -> [ `A of 'a | `B of 'b | `C of 'c ] t

  val variant4
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd ] t

  val variant5
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e ] t

  val variant6
    :  'a t
    -> 'b t
    -> 'c t
    -> 'd t
    -> 'e t
    -> 'f t
    -> [ `A of 'a | `B of 'b | `C of 'c | `D of 'd | `E of 'e | `F of 'f ] t

  (** [recursive] assists with shrinking structures recursively. Its advantage
      over directly using [rec] in the definition of the shrinker is that it
      causes lazy evaluation where possible. *)
  val recursive : ('a t -> 'a t) -> 'a t

end

module type Pre_int = sig

  type t [@@deriving sexp, compare]

  include Polymorphic_compare_intf.Infix with type t := t

  val num_bits : int

  val ( +  ) : t -> t -> t
  val ( -  ) : t -> t -> t
  val ( ~- ) : t -> t

  val zero      : t
  val one       : t
  val min_value : t
  val max_value : t

  val abs  : t -> t
  val succ : t -> t

  val bit_not     : t -> t
  val bit_and     : t -> t   -> t
  val shift_left  : t -> int -> t
  val shift_right : t -> int -> t

  val of_int_exn : int -> t
  val to_int_exn : t -> int
  val to_float : t -> float

end

module type S = sig
  type 'a gen
  type 'a obs
  type 'a shr
  type t
  val gen      : t gen
  val obs      : t obs
  val shrinker : t shr
end

module type S1 = sig
  type 'a gen
  type 'a obs
  type 'a shr
  type 'a t
  val gen      : 'a gen -> 'a t gen
  val obs      : 'a obs -> 'a t obs
  val shrinker : 'a shr -> 'a t shr
end

module type S2 = sig
  type 'a gen
  type 'a obs
  type 'a shr
  type ('a, 'b) t
  val gen      : 'a gen -> 'b gen -> ('a, 'b) t gen
  val obs      : 'a obs -> 'b obs -> ('a, 'b) t obs
  val shrinker : 'a shr -> 'b shr -> ('a, 'b) t shr
end

module type S_bounded = sig
  include S
  (** [gen_between] and [obs_between] produce generators and observers for values
      satisfying [lower_bound] and [upper_bound].  Both functions raise an exception if
      no values satisfy both [lower_bound] and [upper_bound]. *)
  val gen_between
    :  lower_bound : t Maybe_bound.t
    -> upper_bound : t Maybe_bound.t
    -> t gen
  val obs_between
    :  lower_bound : t Maybe_bound.t
    -> upper_bound : t Maybe_bound.t
    -> t obs
end

(** [seed] specifies how to initialize a pseudo-random number generator.  When multiple
    tests share a deterministic seed, they each get a separate copy of the random
    generator's state; random choices in one test do not affect those in another.  The
    nondeterministic seed causes a fresh random state to be generated nondeterministically
    for each test. *)
type seed =
  [ `Deterministic of string
  | `Nondeterministic
  ]

type shrink_attempts =
  [ `Exhaustive
  | `Limit of int
  ]

module type Quickcheck_config = sig

  (** [default_seed] is used initialize the pseudo-random generator that chooses random
      values from generators, in each test that is not provided its own seed. *)
  val default_seed : seed

  (** [default_trial_count] determines the number of trials per test, except in tests
      that explicitly override it. *)
  val default_trial_count : int

  (** [default_trial_count_for_test_no_duplicates] determines the number of trials when
      running [test_no_duplicates] without [~trials], either as a constant or as a factor
      of [default_trial_count]. *)
  val default_trial_count_for_test_no_duplicates
    : [ `Constant of int
      | `Scale_of_default_trial_count of float
      ]

  (** [default_attempts_per_trial] determines the maximum number of attempts to generate
      inputs for trials, as a multiplier for the number of trials, except in tests that
      explicitly override it. *)
  val default_attempts_per_trial : float

  (** [default_probability_threshold_to_remember_choice] determines the minimum
      probability, out of 1.0, at which [Quickcheck.iter] and derived functions will
      remember previous choices and avoid repeating them.  Below this threshold, it is
      assumed that the space needed to record the choice outweighs the negligible chance
      of repeating it. *)
  val default_probability_threshold_to_remember_choice : float

  (** [default_shrink_attempts] determines the number of attempts at shrinking
      when running [test] or [iter] with [~shrinker] and without
      [~shrink_attempts] *)
  val default_shrink_attempts : shrink_attempts

end

module type Quickcheck_configured = sig

  include Quickcheck_config

  type 'a gen
  type 'a shr

  (** [random_value ~seed gen] produces a single value chosen from [gen] using [seed]. *)
  val random_value
    :  ?seed : seed
    -> 'a gen
    -> 'a

  (** [iter ~seed ~trials ~attempts gen ~f] runs [f] on up to [trials] different values
      generated by [gen].  It stops successfully after [trials] successful trials or if
      [gen] runs out of values.  It raises an exception if [f] raises an exception or if
      it fails to produce [trials] inputs from [gen] after [attempts] attempts. *)
  val iter
    :  ?seed                                     : seed
    -> ?trials                                   : int
    -> ?attempts                                 : int
    -> ?probability_threshold_to_remember_choice : float
    -> 'a gen
    -> f:('a -> unit)
    -> unit

  (** [test ~seed ~trials ~attempts ~sexp_of ~examples gen ~f] is like [iter],
      with optional concrete [examples] that are tested before values from
      [gen], and additional information provided on failure.  If [f] raises an
      exception and [sexp_of] is provided, the exception is re-raised with
      a description of the random input that triggered the failure.  If [f]
      raises an exception and [shrinker] is provided, it will be used to
      attempt to shrink the value that caused the exception with re-raising
      behaving the same as for unshrunk inputs. *)
  val test
    :  ?seed                                     : seed
    -> ?trials                                   : int
    -> ?attempts                                 : int
    -> ?shrinker                                 : 'a shr
    -> ?shrink_attempts                          : shrink_attempts
    -> ?probability_threshold_to_remember_choice : float
    -> ?sexp_of                                  : ('a -> Sexp.t)
    -> ?examples                                 : 'a list
    -> 'a gen
    -> f:('a -> unit)
    -> unit

  (** [test_can_generate ~seed ~trials ~attempts ~sexp_of gen ~f] is useful for testing
      [gen] values, to make sure they can generate useful examples.  It tests
      [gen] by generating up to [trials] values and passing them to [f].  Once a value
      satisfies [f], the iteration stops.  If no values satisfy [f], [test_can_generate]
      raises an exception.  If [sexp_of] is provided, the exception includes all of the
      generated values. *)
  val test_can_generate
    :  ?seed                                     : seed
    -> ?trials                                   : int
    -> ?attempts                                 : int
    -> ?probability_threshold_to_remember_choice : float
    -> ?sexp_of                                  : ('a -> Sexp.t)
    -> 'a gen
    -> f:('a -> bool)
    -> unit

  (** [test_no_duplicates ~seed ~trials ~attempts ~sexp_of gen ~by] is useful for testing
      [gen] values, to make sure they do not create duplicate values.  It tests
      [gen] by generating up to [trials] values and comparing each pair of the generated
      values using [by].  If any of the pairs are identical, [test_no_duplicates] raises
      an exception.  If [sexp_of] is provided, the exception includes the identical
      values. *)
  val test_no_duplicates
    :  ?seed                                     : seed
    -> ?trials                                   : int
    -> ?attempts                                 : int
    -> ?probability_threshold_to_remember_choice : float
    -> ?sexp_of                                  : ('a -> Sexp.t)
    -> 'a gen
    -> by:[ `Equal of 'a -> 'a -> bool | `Compare of 'a -> 'a -> int ]
    -> unit

  (** [random_sequence ~seed gen] produces a sequence of values chosen from [gen]. *)
  val random_sequence
    :  ?seed                                     : seed
    -> ?probability_threshold_to_remember_choice : float
    -> 'a gen
    -> 'a Sequence.t

  (** [random_state_of_seed] constructs initial random states for a given seed.  This is
      intended for building extensions to this interface, rather than for use in
      individual tests. *)
  val random_state_of_seed : seed -> Core_random.State.t

end

module type Quickcheck = sig

  type 'a gen
  type 'a obs
  type 'a shr

  module Generator : Generator with type 'a t = 'a gen with type 'a obs := 'a obs
  module Observer  : Observer  with type 'a t = 'a obs with type 'a gen := 'a gen
  module Shrinker  : Shrinker  with type 'a t = 'a shr

  module Make_int (M : Pre_int) : S_bounded
    with type    t   :=    M.t
    with type 'a gen := 'a gen
    with type 'a obs := 'a obs
    with type 'a shr := 'a shr

  module For_int : S_bounded
    with type    t   :=    int
    with type 'a gen := 'a gen
    with type 'a obs := 'a obs
    with type 'a shr := 'a shr

  include Quickcheck_configured (** with a default config *)
    with type 'a gen := 'a gen
    with type 'a shr := 'a shr

  module Configure (Config : Quickcheck_config) : Quickcheck_configured
    with type 'a gen := 'a gen

end
