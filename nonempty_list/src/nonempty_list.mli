open Core

(** A ['a t] represents a non-empty list, as evidenced by the fact that there is no [[]]
    variant. The sexp representation is as a regular list (i.e., the same as the
    [Stable.V3] module below).

    For operations on a locally allocated ['a t], see [Local_nonempty_list].
*)
type 'a t = ( :: ) of 'a * 'a list
[@@deriving
  compare ~localize
  , equal ~localize
  , sexp
  , sexp_grammar
  , hash
  , quickcheck
  , typerep
  , bin_io
  , globalize]

include Comparator.Derived with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t
include Monad.S_local with type 'a t := 'a t
include Indexed_container.S1 with type 'a t := 'a t

val create : 'a -> 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val of_list : 'a list -> 'a t option
val of_list_error : 'a list -> 'a t Or_error.t
val of_list_exn : 'a list -> 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val nth : 'a t -> int -> 'a option
val nth_exn : 'a t -> int -> 'a
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
val reverse : 'a t -> 'a t
val append : 'a t -> 'a list -> 'a t
val unzip : ('a * 'b) t -> 'a t * 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t List.Or_unequal_lengths.t
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t List.Or_unequal_lengths.t
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val filter : 'a t -> f:('a -> bool) -> 'a list
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a list
val filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b list
val concat : 'a t t -> 'a t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val last : 'a t -> 'a
val drop_last : 'a t -> 'a list
val to_sequence : 'a t -> 'a Sequence.t
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val sort_and_group : 'a t -> compare:('a -> 'a -> int) -> 'a t t
val stable_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val stable_dedup : 'a t -> compare:('a -> 'a -> int) -> 'a t
val dedup_and_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val permute : ?random_state:Random.State.t -> 'a t -> 'a t
val random_element : ?random_state:Random.State.t -> 'a t -> 'a
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
val fold_nonempty : 'a t -> init:('a -> 'acc) -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_right : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
val folding_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'c t
val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t
val findi_exn : 'a t -> f:(int -> 'a -> bool) -> int * 'a

(** [min_elt'] and [max_elt'] differ from [min_elt] and [max_elt] (included in
    [Container.S1]) in that they don't return options. *)
val min_elt' : 'a t -> compare:('a -> 'a -> int) -> 'a

val max_elt' : 'a t -> compare:('a -> 'a -> int) -> 'a

(** Like [Map.add_multi], but comes with a guarantee that the range of the returned map is
    all nonempty lists.
*)
val map_add_multi : ('k, 'v t, 'cmp) Map.t -> key:'k -> data:'v -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_alist_multi], but comes with a guarantee that the range of the returned
    map is all nonempty lists.
*)
val map_of_alist_multi
  :  ('k * 'v) list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_sequence_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists.
*)
val map_of_sequence_multi
  :  ('k * 'v) Sequence.t
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_list_with_key_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists.
*)
val map_of_list_with_key_multi
  :  'v list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> get_key:('v -> 'k)
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Result.combine_errors] but for non-empty lists *)
val combine_errors : ('ok, 'err) Result.t t -> ('ok t, 'err t) Result.t

(** Like [Result.combine_errors_unit] but for non-empty lists *)
val combine_errors_unit : (unit, 'err) Result.t t -> (unit, 'err t) Result.t

(** Like [Or_error.combine_errors] but for non-empty lists *)
val combine_or_errors : 'a Or_error.t t -> 'a t Or_error.t

(** Like [Or_error.combine_errors_unit] but for non-empty lists *)
val combine_or_errors_unit : unit Or_error.t t -> unit Or_error.t

(** Like [Or_error.filter_ok_at_least_one] but for non-empty lists.

    In particular it:

    + Returns a ['a t Or_error.t], statically ensuring that in the [Ok l] case, [l] is
    nonempty.

    + Takes a ['a Or_error.t t], ensuring that in the [Error e] case, [e] is informative,
    rather than having been constructed from an empty list. *)
val filter_ok_at_least_one : 'a Or_error.t t -> 'a t Or_error.t

type 'a nonempty_list := 'a t

module Emptiness_witness : sig
  type empty = private Empty
  type nonempty = private Nonempty
end

module Part : sig
  type ('a, 'emptiness) t =
    | Empty : ('a, Emptiness_witness.empty) t
    | Nonempty : 'a nonempty_list -> ('a, Emptiness_witness.nonempty) t
  [@@deriving sexp_of]

  type 'a packed = T : ('a, 'emptiness) t -> 'a packed
  [@@deriving compare, equal, sexp_of]

  val of_nonempty_list : 'a nonempty_list -> ('a, Emptiness_witness.nonempty) t
  val to_nonempty_list : ('a, Emptiness_witness.nonempty) t -> 'a nonempty_list
  val packed_of_list : 'a list -> 'a packed
  val to_list : ('a, _) t -> 'a list
  val map : ('a, 'emptiness) t -> f:('a -> 'b) -> ('b, 'emptiness) t

  (** Append two parts, knowing that the left part is nonempty, which produces a nonempty
      part. *)
  val append1
    :  ('a, Emptiness_witness.nonempty) t
    -> ('a, _) t
    -> ('a, Emptiness_witness.nonempty) t

  (** Append two parts, knowing that the right part is nonempty, which produces a nonempty
      part. *)
  val append2
    :  ('a, _) t
    -> ('a, Emptiness_witness.nonempty) t
    -> ('a, Emptiness_witness.nonempty) t

  val append_packed : 'a packed -> 'a packed -> 'a packed
end

module Partition : sig
  (** A [Partition] represents a splitting of a nonempty list into two parts, a "left" and
      a "right" part.

      Unlike a [List], partitioning a nonempty list does not produce two nonempty lists,
      because one of the parts could be empty. However, we know for sure that at least one
      of the parts is nonempty, so we represent this information in a structured type
      indicating which parts are nonempty.

      The [partition] functions below have two variations, one which safely provides a
      [Partition], and another [partition'] variation which returns two possibly empty
      lists instead. The latter loses some type information but may be more convenient in
      some cases.
  *)

  module Emptiness : sig
    type ('left_emptiness, 'right_emptiness) t =
      | Left_nonempty : (Emptiness_witness.nonempty, Emptiness_witness.empty) t
      | Right_nonempty : (Emptiness_witness.empty, Emptiness_witness.nonempty) t
      | Both_nonempty : (Emptiness_witness.nonempty, Emptiness_witness.nonempty) t
    [@@deriving sexp_of]
  end

  type ('left, 'right, 'left_emptiness, 'right_emptiness) t =
    { left : ('left, 'left_emptiness) Part.t
    ; right : ('right, 'right_emptiness) Part.t
    ; emptiness : ('left_emptiness, 'right_emptiness) Emptiness.t
    }
  [@@deriving sexp_of]

  type ('left, 'right) packed =
    | T : ('left, 'right, 'left_emptiness, 'right_emptiness) t -> ('left, 'right) packed
  [@@deriving compare, equal, sexp_of]

  (** Returns the left part of the partition. *)
  val left : ('left, _, 'left_emptiness, _) t -> ('left, 'left_emptiness) Part.t

  (** Returns the right part of the partition. *)
  val right : (_, 'right, _, 'right_emptiness) t -> ('right, 'right_emptiness) Part.t

  (** Returns the emptiness of the partition. *)
  val emptiness
    :  (_, _, 'left_emptiness, 'right_emptiness) t
    -> ('left_emptiness, 'right_emptiness) Emptiness.t

  (** Combines the parts of the partition into a single nonempty list, with the left being
      ordered before the right. Both parts must have the same type. *)
  val combine : ('a, 'a, _, _) t -> 'a nonempty_list

  (** Combines the parts of the partition into a single nonempty list. This uses an
      [Either.t] to indicate which part an element came from and to allow the two parts to
      be of different types. *)
  val combine' : ('left, 'right, _, _) t -> ('left, 'right) Either.t nonempty_list

  (** Maps the left part of the partition, calling [f] on the part. *)
  val map_left
    :  ('left_1, 'right, 'left_emptiness, 'right_emptiness) t
    -> f:(('left_1, 'left_emptiness) Part.t -> ('left_2, 'left_emptiness) Part.t)
    -> ('left_2, 'right, 'left_emptiness, 'right_emptiness) t

  (** Maps the right part of the partition, calling [f] on the part. *)
  val map_right
    :  ('left, 'right_1, 'left_emptiness, 'right_emptiness) t
    -> f:(('right_1, 'right_emptiness) Part.t -> ('right_2, 'right_emptiness) Part.t)
    -> ('left, 'right_2, 'left_emptiness, 'right_emptiness) t

  (** Swaps the left and right parts of the partition. *)
  val swap
    :  ('left, 'right, 'left_emptiness, 'right_emptiness) t
    -> ('right, 'left, 'right_emptiness, 'left_emptiness) t
end

(** [partition_tf t ~f] returns a pair [t1, t2], where [t1] is all elements of [t] that
    satisfy [f], and [t2] is all elements of [t] that do not satisfy [f]. The "tf" suffix
    is mnemonic to remind readers that the result is (trues, falses).

    At least one of the two parts must be nonempty, which is represented by the type of
    [Partition.t]. *)
val partition_tf : 'a t -> f:('a -> bool) -> ('a, 'a) Partition.packed

(** Like [partition_tf], but returns the parts in two lists instead. *)
val partition_tf' : 'a t -> f:('a -> bool) -> 'a list * 'a list

(** [partition_map t ~f] partitions [t] according to [f].

    At least one of the two parts must be nonempty, which is represented by the type of
    [Partition.t]. *)
val partition_map
  :  'a t
  -> f:('a -> ('left, 'right) Either.t)
  -> ('left, 'right) Partition.packed

(** Like [partition_map], but returns the parts in two lists instead. *)
val partition_map'
  :  'a t
  -> f:('a -> ('left, 'right) Either.t)
  -> 'left list * 'right list

(** [partition_result t] returns a pair [t1, t2], where [t1] is the
    all [Ok] elements in [t] and [t2] is the list of all [Error]
    elements. The order of elements in the input list is preserved.

    At least one of the two parts must be nonempty, which is represented by the type of
    [Partition.t]. *)
val partition_result : ('ok, 'error) Result.t t -> ('ok, 'error) Partition.packed

(** Like [partition_result], but returns the parts in two lists instead. *)
val partition_result' : ('ok, 'error) Result.t t -> 'ok list * 'error list

(** validates a list, naming each element by its position in the list (where the first
    position is 1, not 0). *)
val validate_indexed : 'a Validate.check -> 'a t Validate.check

(** validates a list, naming each element using a user-defined function for computing the
    name. *)
val validate : name:('a -> string) -> 'a Validate.check -> 'a t Validate.check

(** Returns a flag that must be passed one or more times.
    See [Command.Param.one_or_more_as_pair]. *)
val flag : 'a Command.Param.Arg_type.t -> 'a t Command.Flag.t

(** Accepts comma-separated lists of arguments parsed by [t].
    See [Command.Param.Arg_type.comma_separated]. *)
val comma_separated_argtype
  :  ?key:'a t Univ_map.Multi.Key.t
  -> ?strip_whitespace:bool
  -> ?unique_values:bool
  -> 'a Command.Param.Arg_type.t
  -> 'a t Command.Param.Arg_type.t

(** This module provides 0-alloc versions of [to_list] and [of_list], via [some] and
    allowing you to [match%optional] on a list, respectively. *)
module Option : sig
  type 'a t = 'a list
  [@@deriving
    compare ~localize, equal ~localize, sexp, sexp_grammar, hash, quickcheck, typerep]

  (** Constructors analogous to [None] and [Some]. *)

  val none : _ t
  val some : 'a nonempty_list -> 'a t
  val is_none : _ t -> bool
  val is_some : _ t -> bool

  (** [value (some x) ~default = x] and [value none ~default = default]. *)
  val value : 'a t -> default:'a nonempty_list -> 'a nonempty_list

  (** [value_exn (some x) = x].  [value_exn none] raises.  Unlike [Option.value_exn],
      there is no [?message] argument, so that calls to [value_exn] that do not raise
      also do not have to allocate. *)
  val value_exn : 'a t -> 'a nonempty_list

  (** [unchecked_value (some x) = x].  [unchecked_value none] returns an unspecified
      value.  [unchecked_value t] is intended as an optimization of [value_exn t] when
      [is_some t] is known to be true. *)
  val unchecked_value : 'a t -> 'a nonempty_list

  val to_option : 'a t -> 'a nonempty_list option
  val of_option : 'a nonempty_list option -> 'a t

  module Optional_syntax :
    Optional_syntax.S1 with type 'a t := 'a t and type 'a value := 'a nonempty_list
end

(** a non-empty version of Reversed_list.t *)
module Reversed : sig
  type 'a t = ( :: ) of 'a * 'a Reversed_list.t

  val cons : 'a -> 'a t -> 'a t
  val to_rev_list : 'a t -> 'a Reversed_list.t
  val rev : 'a t -> 'a nonempty_list
  val rev_append : 'a t -> 'a list -> 'a nonempty_list
  val rev_map : 'a t -> f:('a -> 'b) -> 'b nonempty_list
  val rev_mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b nonempty_list

  (** Renders sexps without reversing the list. E.g. [1::2] is represented as [(1 2)]. *)
  module With_sexp_of : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end

  (** Renders sexps after reversing the list. E.g. [1::2] is represented as [(2 1)]. *)
  module With_rev_sexp_of : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end
end

val rev' : 'a t -> 'a Reversed.t
val rev_append : 'a Reversed_list.t -> 'a t -> 'a t

module Unstable : sig
  type nonrec 'a t = 'a t
  [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar]
end

module Stable : sig
  (** Represents a [t] as an ordinary list for sexp and bin_io conversions, e.g. [1::2]
      is represented as [(1 2)]. *)
  module V3 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , hash
      , stable_witness]
  end

  (** Represents a [t] as an ordinary list for sexp conversions, but uses a record [{hd :
      'a; tl ; 'a list}] for bin_io conversions. This module is provided for compatibility
      with existing protocols; there's no reason not to use the latest version if you're
      writing a new protocol. *)
  module V2 : sig
    type nonrec 'a t = 'a t
    [@@deriving bin_io, compare ~localize, equal ~localize, sexp, hash, stable_witness]
  end

  (** Represents a [t] as an ordinary list for sexps, but as a pair for bin_io conversions
      (i.e., a ['a t] is represented as the type ['a * 'a list]). This module is provided
      for compatibility with existing protocols; there's no reason not to use the latest
      version if you're writing a new protocol. *)
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving bin_io, compare ~localize, equal ~localize, sexp, stable_witness]
  end
end
