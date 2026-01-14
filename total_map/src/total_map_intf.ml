open! Core
open! Import

[%%template
[@@@modality.default (p, c) = ((nonportable, uncontended), (portable, contended))]

module type Key_plain = sig
  type t : value mod c p [@@deriving sexp_of, compare, enumerate]
end

module type Key = sig
  type t : value mod c p [@@deriving sexp, bin_io, compare, enumerate]
end

module type Key_plain_with_witnesses = sig
  include Key_plain [@modality p]
  include Comparator.S [@modality p] with type t := t
  include Enumeration.S with type t := t
end

module type Key_with_witnesses = sig
  include Key [@modality p]
  include Comparator.S [@modality p] with type t := t
  include Enumeration.S with type t := t
end

module type Key_with_stable_witness = sig
  include Key_with_witnesses [@modality p]

  include
    Stable_with_witness with type t := t and type comparator_witness := comparator_witness
end

module type S_plain = sig
  type ('key
       , 'a
       , 'cmp
       , 'enum)
       total_map :
       value mod contended portable with 'key with 'a with 'cmp @@ contended

  module Key : Key_plain [@modality p]

  type comparator_witness : value mod p
  type enumeration_witness

  type 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) total_map
  [@@deriving sexp_of, compare ~localize, equal ~localize]

  include sig
    [@@@mode.default p = (nonportable, p)]

    val quickcheck_generator
      :  'a Quickcheck.Generator.t @ p
      -> 'a t Quickcheck.Generator.t @ p

    val quickcheck_observer
      :  'a Quickcheck.Observer.t @ p
      -> 'a t Quickcheck.Observer.t @ p

    val quickcheck_shrinker
      :  'a Quickcheck.Shrinker.t @ p
      -> 'a t Quickcheck.Shrinker.t @ p
  end

  include Applicative with type 'a t := 'a t

  val create : local_ (Key.t -> 'a) -> 'a t
  val create_const : 'a -> 'a t
  val of_map_exn : (Key.t, 'a, comparator_witness) Map.t -> 'a t
  val of_alist_exn : (Key.t * 'a) list -> 'a t
  val of_alist_multi_exn : (Key.t * 'a) list -> 'a list t

  (** Note that [of_alist_multi keylist] will contain empty list items if a given [Key.t]
      is not present in [keylist] *)
  val of_alist_multi : (Key.t * 'a) list -> 'a list t
end

(** An alternative interface for [S_plain] which can be used with [include functor]. We
    keep the old interface to avoid breaking existing code. *)
module type For_include_functor_plain = sig
  module Total_map : S_plain [@modality p]
end

module type S = sig
  module Key : Key [@modality p]
  include S_plain [@modality p] with module Key := Key
  include Sexpable.S1 with type 'a t := 'a t
  include Binable.S1 with type 'a t := 'a t
end

(** An alternative interface for [S] which can be used with [include functor]. We keep the
    old interface to avoid breaking existing code. *)
module type For_include_functor = sig
  module Total_map : S [@modality p]
end

module type Stable_S = sig
  type ('key, 'a, 'cmp, 'enum) total_map

  module Key : Key [@modality p]

  type comparator_witness
  type enumeration_witness

  type 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) total_map
  [@@deriving bin_io, sexp, compare ~localize]
end

module type Stable_For_include_functor = sig
  module Total_map : Stable_S [@modality p]
end

module type Stable_S_with_stable_witness = sig
  type ('key, 'a, 'cmp, 'enum) total_map

  module Key : Key [@modality p]

  type comparator_witness
  type enumeration_witness

  type 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) total_map
  [@@deriving bin_io, sexp, compare ~localize, stable_witness]
end

module type Stable_For_include_functor_with_stable_witness = sig
  module Total_map : Stable_S_with_stable_witness [@modality p]
end]

module type Stable = sig
  type ('key, 'a, 'cmp, 'enum) t

  [%%template:
  [@@@modality.default p = (portable, nonportable)]

  module type S =
    Stable_S
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

  module type S_with_stable_witness =
    Stable_S_with_stable_witness
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

  module type For_include_functor =
    Stable_For_include_functor
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

  module type For_include_functor_with_stable_witness =
    Stable_For_include_functor_with_stable_witness
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t]

  module%template.portable
    [@modality p] Make_with_witnesses
      (Key : Key_with_witnesses
    [@modality p]) :
    S
    [@modality p]
    with module Key = Key
    with type comparator_witness = Key.comparator_witness
    with type enumeration_witness = Key.enumeration_witness

  module%template.portable
    [@modality p] Make_for_include_functor_with_witnesses
      (Key : Key_with_witnesses
    [@modality p]) :
    For_include_functor
    [@modality p]
    with module Total_map.Key = Key
    with type Total_map.comparator_witness = Key.comparator_witness
    with type Total_map.enumeration_witness = Key.enumeration_witness

  module%template.portable
    [@modality p] Make_with_stable_witness
      (Key : Key_with_stable_witness
    [@modality p]) :
    S_with_stable_witness
    [@modality p]
    with module Key = Key
    with type comparator_witness = Key.comparator_witness
    with type enumeration_witness = Key.enumeration_witness

  module%template.portable
    [@modality p] Make_for_include_functor_with_stable_witness
      (Key : Key_with_stable_witness
    [@modality p]) :
    For_include_functor_with_stable_witness
    [@modality p]
    with module Total_map.Key = Key
    with type Total_map.comparator_witness = Key.comparator_witness
    with type Total_map.enumeration_witness = Key.enumeration_witness
end

module type Total_map = sig @@ portable
  (** A map that includes an entry for every possible value of the key type.

      This is intended to be used on ['key] types where there is a full enumeration of the
      type. In the common use case, ['key] will be a simple variant type with
      [[@@deriving compare, enumerate]]. For example:

      {[
        module Arrow_key = struct
          module T = struct
            type t =
              | Up
              | Down
              | Left
              | Right
            [@@deriving sexp, bin_io, compare, enumerate]
          end

          include T
          module Total_map = Total_map.Make (T)
        end
      ]}

      In such a case, a [t] is semantically equivalent to a pure function from ['key] to
      ['value]. The differences are that it is serializable and that mapping or changing a
      [t] will produce a [t] using the same amount of space as the original.

      However, in theory you could also modify the comparison function and enumeration, so
      long as the enumeration contains at least one representative of each equivalence
      class determined by the comparison function. *)

  module Enumeration = Enumeration

  type ('key, 'a, 'cmp, 'enum) t = private ('key, 'a, 'cmp) Map.t

  val to_map : ('key, 'a, 'cmp, _) t -> ('key, 'a, 'cmp) Map.t

  (** Many of the functions below have types reflecting the fact that the maps are total
      (e.g., [find] does not return an option). The fact that they won't raise exceptions
      relies on the enumeration passed to [Make] being complete. *)

  val map : ('key, 'a, 'c, 'e) t -> f:('a -> 'b) -> ('key, 'b, 'c, 'e) t
  val mapi : ('key, 'a, 'c, 'e) t -> f:(key:'key -> data:'a -> 'b) -> ('key, 'b, 'c, 'e) t

  val map2
    :  ('key, 'a, 'cmp, 'enum) t
    -> ('key, 'b, 'cmp, 'enum) t
    -> f:('a -> 'b -> 'c)
    -> ('key, 'c, 'cmp, 'enum) t

  val mapi2
    :  ('key, 'a, 'cmp, 'enum) t
    -> ('key, 'b, 'cmp, 'enum) t
    -> f:('key -> 'a -> 'b -> 'c)
    -> ('key, 'c, 'cmp, 'enum) t

  val unzip
    :  ('key, 'a * 'b, 'cmp, 'enum) t
    -> ('key, 'a, 'cmp, 'enum) t * ('key, 'b, 'cmp, 'enum) t

  val iter_keys : ('key, _, _, _) t -> f:('key -> unit) -> unit
  val iter : (_, 'a, _, _) t -> f:('a -> unit) -> unit
  val iteri : ('key, 'a, _, _) t -> f:(key:'key -> data:'a -> unit) -> unit

  val iter2
    :  ('key, 'a, 'cmp, 'enum) t
    -> ('key, 'b, 'cmp, 'enum) t
    -> f:(key:'key -> 'a -> 'b -> unit)
    -> unit

  val fold
    :  ('key, 'a, _, _) t
    -> init:'acc
    -> f:(key:'key -> data:'a -> 'acc -> 'acc)
    -> 'acc

  val fold_right
    :  ('key, 'a, _, _) t
    -> init:'acc
    -> f:(key:'key -> data:'a -> 'acc -> 'acc)
    -> 'acc

  (** Folds over two maps side by side, like [iter2]. *)
  val fold2
    :  ('key, 'a, 'cmp, 'enum) t
    -> ('key, 'b, 'cmp, 'enum) t
    -> init:'acc
    -> f:(key:'key -> 'a -> 'b -> 'acc -> 'acc)
    -> 'acc

  val set : ('key, 'a, 'cmp, 'enum) t -> 'key -> 'a -> ('key, 'a, 'cmp, 'enum) t

  val to_alist
    :  ?key_order:[ `Increasing | `Decreasing ] (** default is [`Increasing] *)
    -> ('key, 'a, _, _) t
    -> ('key * 'a) list

  val find : ('key, 'a, _, _) t -> 'key -> 'a
  val change : ('key, 'a, 'c, 'e) t -> 'key -> f:('a -> 'a) -> ('key, 'a, 'c, 'e) t
  val combine_errors : ('key, 'a Or_error.t, 'c, 'e) t -> ('key, 'a, 'c, 'e) t Or_error.t
  val data : (_, 'a, _, _) t -> 'a list
  val for_all : (_, 'a, _, _) t -> f:('a -> bool) -> bool
  val for_alli : ('key, 'a, _, _) t -> f:(key:'key -> data:'a -> bool) -> bool

  val validate
    :  name:('key -> string)
    -> 'v Validate.check
    -> ('key, 'v, _, _) t Validate.check

  val validatei
    :  name:('key -> string)
    -> ('key * 'v) Validate.check
    -> ('key, 'v, _, _) t Validate.check

  (** Sequence a total map of computations in order of their keys resulting in computation
      of the total map of results. *)
  module Sequence (A : Applicative) : sig
    val sequence : ('key, 'a A.t, 'cmp, 'enum) t -> ('key, 'a, 'cmp, 'enum) t A.t
  end

  module Sequence2 (A : Applicative.S2) : sig
    val sequence
      :  ('key, ('a, 'b) A.t, 'cmp, 'enum) t
      -> (('key, 'a, 'cmp, 'enum) t, 'b) A.t
  end

  module Sequence3 (A : Applicative.S3) : sig
    val sequence
      :  ('key, ('a, 'b, 'c) A.t, 'cmp, 'enum) t
      -> (('key, 'a, 'cmp, 'enum) t, 'b, 'c) A.t
  end

  (** The only reason that the Applicative interface isn't included here is that we don't
      have an [Applicative.S4]. *)

  module type Key = Key
  module type Key_plain = Key_plain
  module type Key_with_witnesses = Key_with_witnesses
  module type Key_plain_with_witnesses = Key_plain_with_witnesses

  [%%template:
  [@@@modality.default p = (portable, nonportable)]

  module type S_plain =
    S_plain
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

  module type For_include_functor_plain =
    For_include_functor_plain
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

  module type S =
    S
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

  module type For_include_functor =
    For_include_functor
    [@modality p]
    with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t]

  module%template.portable [@modality p] Make_plain (Key : Key_plain [@modality p]) :
    S_plain [@modality p] with module Key = Key

  module%template.portable
    [@modality p] Make_for_include_functor_plain
      (Key : Key_plain
    [@modality p]) :
    For_include_functor_plain [@modality p] with module Total_map.Key = Key

  module%template.portable
    [@modality p] Make_plain_with_witnesses
      (Key : Key_plain_with_witnesses
    [@modality p]) :
    S_plain
    [@modality p]
    with module Key = Key
    with type comparator_witness = Key.comparator_witness
    with type enumeration_witness = Key.enumeration_witness

  module%template.portable
    [@modality p] Make_for_include_functor_plain_with_witnesses
      (Key : Key_plain_with_witnesses
    [@modality p]) :
    For_include_functor_plain
    [@modality p]
    with module Total_map.Key = Key
    with type Total_map.comparator_witness = Key.comparator_witness
    with type Total_map.enumeration_witness = Key.enumeration_witness

  module%template.portable [@modality p] Make (Key : Key [@modality p]) :
    S [@modality p] with module Key = Key

  module%template.portable
    [@modality p] Make_for_include_functor
      (Key : Key
    [@modality p]) : For_include_functor [@modality p] with module Total_map.Key = Key

  module%template.portable
    [@modality p] Make_with_witnesses
      (Key : Key_with_witnesses
    [@modality p]) :
    S
    [@modality p]
    with module Key = Key
    with type comparator_witness = Key.comparator_witness
    with type enumeration_witness = Key.enumeration_witness

  module%template.portable
    [@modality p] Make_for_include_functor_with_witnesses
      (Key : Key_with_witnesses
    [@modality p]) :
    For_include_functor
    [@modality p]
    with module Total_map.Key = Key
    with type Total_map.comparator_witness = Key.comparator_witness
    with type Total_map.enumeration_witness = Key.enumeration_witness

  (** The below functions allow writing things like
      [type t = int M(T).t [@@deriving sexp]]. Note that for sexp, bin_io, and compare
      alone this would not be useful, as [T.Total_map.t] already has those conversions.
      However, it could be useful to additionally derive [sexp_grammar]. (Sexp grammar
      functionality is upcoming.) *)
  module type M = sig
    type t

    module Total_map : S with type Key.t = t
  end

  module M (T : For_include_functor_plain) : sig
    type 'a t = 'a T.Total_map.t
  end

  val m__t_of_sexp
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> (Sexp.t -> 'v)
    -> Sexp.t
    -> ('k, 'v, 'cmp, 'enum) t

  val sexp_of_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> ('v -> Sexp.t)
    -> ('k, 'v, 'cmp, 'enum) t
    -> Sexp.t

  val m__t_sexp_grammar
    :  (module Map.M_sexp_grammar with type t = 'k)
    -> 'v Sexplib0.Sexp_grammar.t
    -> ('k, 'v, 'cmp, 'enum) t Sexplib0.Sexp_grammar.t

  val bin_shape_m__t : (module For_include_functor) -> Bin_shape.t -> Bin_shape.t

  val bin_size_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Bin_prot.Size.sizer
    -> ('k, 'v, 'cmp, 'enum) t Bin_prot.Size.sizer

  val bin_write_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Bin_prot.Write.writer
    -> ('k, 'v, 'cmp, 'enum) t Bin_prot.Write.writer

  val bin_read_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Bin_prot.Read.reader
    -> ('k, 'v, 'cmp, 'enum) t Bin_prot.Read.reader

  val __bin_read_m__t__
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Bin_prot.Read.reader
    -> ('k, 'v, 'cmp, 'enum) t Bin_prot.Read.vtag_reader

  val compare_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> ('v -> 'v -> int)
    -> ('k, 'v, 'cmp, 'enum) t
    -> ('k, 'v, 'cmp, 'enum) t
    -> int

  val quickcheck_generator_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Quickcheck.Generator.t
    -> ('k, 'v, 'cmp, 'enum) t Quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Quickcheck.Observer.t
    -> ('k, 'v, 'cmp, 'enum) t Quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module M
          with type t = 'k
           and type Total_map.enumeration_witness = 'enum
           and type Total_map.comparator_witness = 'cmp)
    -> 'v Quickcheck.Shrinker.t
    -> ('k, 'v, 'cmp, 'enum) t Quickcheck.Shrinker.t

  module Stable : sig
    module V1 : sig end
    [@@deprecated
      "[since 2025-01] Deserialization functions for [Total_map.Stable.V1] do not ensure \
       the provided map is total. Use [Total_map.Stable.V2] instead."]

    module V1_unsafe_deserialization :
      Stable with type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp, 'enum) t

    module V2 : Stable with type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp, 'enum) t
  end
end
