open! Core

module type S_common = sig
  type ('a, 'b) t

  val hash_fold_m__t
    :  (module Set.Hash_fold_m with type t = 'elt)
    -> Hash.state
    -> ('elt, _) t
    -> Hash.state

  val hash_m__t : (module Set.Hash_fold_m with type t = 'elt) -> ('elt, _) t -> int

  val%template compare_m__t
    :  (module Set.Compare_m)
    -> ('elt, 'cmp) t @ m
    -> ('elt, 'cmp) t @ m
    -> int
  [@@mode m = (local, global)]

  val%template equal_m__t
    :  (module Set.Equal_m)
    -> ('elt, 'cmp) t @ m
    -> ('elt, 'cmp) t @ m
    -> bool
  [@@mode m = (local, global)]
end

module type S_serializable = sig
  type ('a, 'b) t

  val sexp_of_m__t : (module Set.Sexp_of_m with type t = 'elt) -> ('elt, 'cmp) t -> Sexp.t

  val m__t_of_sexp
    :  (module Set.M_of_sexp with type t = 'elt and type comparator_witness = 'cmp)
    -> Sexp.t
    -> ('elt, 'cmp) t

  val m__t_sexp_grammar
    :  (module Set.M_sexp_grammar with type t = 'elt)
    -> ('elt, 'cmp) t Sexplib0.Sexp_grammar.t
    @@ portable

  val bin_shape_m__t : ('a, 'b) Set.Elt_bin_io.t -> Bin_prot.Shape.t
  val bin_size_m__t : ('a, 'b) Set.Elt_bin_io.t -> ('a, 'b) t Bin_prot.Size.sizer
  val bin_write_m__t : ('a, 'b) Set.Elt_bin_io.t -> ('a, 'b) t Bin_prot.Write.writer
  val bin_read_m__t : ('a, 'b) Set.Elt_bin_io.t -> ('a, 'b) t Bin_prot.Read.reader

  val __bin_read_m__t__
    :  ('a, 'b) Set.Elt_bin_io.t
    -> ('a, 'b) t Bin_prot.Read.vtag_reader
end

module type S_stable = sig
  type ('a, 'b) t

  val stable_witness_m__t
    :  (module Set.Stable.V1.Stable_witness_m
          with type t = 'elt
           and type comparator_witness = 'cmp)
    -> ('elt, 'cmp) t Stable_witness.t
end

module type S_unstable = sig
  type ('a, 'b) t

  val quickcheck_generator_m__t
    :  (module Set.Quickcheck_generator_m
          with type t = 'a
           and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Quickcheck.Generator.t

  val quickcheck_observer_m__t
    :  (module Set.Quickcheck_observer_m
          with type t = 'a
           and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Quickcheck.Observer.t

  val quickcheck_shrinker_m__t
    :  (module Set.Quickcheck_shrinker_m
          with type t = 'a
           and type comparator_witness = 'cmp)
    -> ('a, 'cmp) t Quickcheck.Shrinker.t
end

module type For_deriving = sig
  module type S_serializable = S_serializable
  module type S_stable = S_stable
  module type S_unstable = S_unstable
  module type S_common = S_common
end
