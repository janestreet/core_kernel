(** See {!Quickcheck_generator} for an overview. *)

type 'a t

(** [bind t f] is a monadic bind that replaces each value [x] that has probability [p] in
    [t] with the probability distribution [f x] weighted proportionally by [p].
    [singleton], below, corresponds to monadic return. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** ['a Choice.t] represents the choice of a value from a generator.  It also
    encapsulates the history of which random decisions were made to reach that value,
    including all choices along the way that failed to produce a value. *)
module Choice : sig

  type 'a gen = 'a t
  type 'a t

  (** [original_gen t] produces the original generator from which [t] was constructed,
      unmodified. *)
  val original_gen : 'a t -> 'a gen

  (** [updated_gen t ~keep] produces a generator representing a subset of the probability
      distribution from which [t] was constructed.

      The subset determined by [keep] refers to notions of "left" and "right" with respect
      to choices in a [gen].  For this purpose, a generator can be thought of as a
      potentially infinite sequence of weighted choices, and operations on generators
      preserve the order of this sequence.  The [keep] argument determines where to "cut"
      this sequence relative to the current choice.  This property can be used to
      construct generators that do not repeat choices, or do not repeat the same set of
      choices in a different order, which is useful, e.g., for constructing lists of
      unique values or unique sets of values.

      In all variants of [keep], any failed choices that were made in constructing [t] are
      discarded.

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

    [bind t f] is equal to [bind_choice t (fun c -> f (Choice.value c))], although [bind]
    is cheaper than [bind_choice]. *)
val bind_choice : 'a t -> ('a Choice.t -> 'b t) -> 'b t

(** Empty generator that is guaranteed to fail to produce a value. *)
val failure : _ t

(** Constant generator.  [singleton t] is equal to [return t]. *)
val singleton : 'a -> 'a t

(** [weighted_union alist] produces a generator that combines the distributions of each
    [t] in [alist] with the associated weights, which must be finite positive floating
    point values. *)
val weighted_union : (float * 'a t) list -> 'a t

(** [of_fun f] produces a generator that lazily applies [f].

    [f] *MUST* be deterministic or else random value generation will fail.  However, it is
    recommended that [f] not be memoized.  Instead, spread out the work of generating a
    whole distribution over many [of_fun] calls combined with [weighted_union].  This
    allows lazily generated generators to be garbage collected after each test and the
    relevant portions cheaply recomputed in subsequent tests, rather than accumulating
    without bound over time. *)
val of_fun : (unit -> 'a t) -> 'a t

(** [choose t ~random_float_between_zero_and_one] makes a choice in [t] at random, using
    [random_float_between_zero_and_one] to produce random floats between 0. (inclusive)
    and 1. (exclusive) for each weighted choice.  If [t] has been fully explored, it
    produces [`No_choices_remain].  Otherwise it produces [`Choice c] for some choice [c]
    in [t]. *)
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
