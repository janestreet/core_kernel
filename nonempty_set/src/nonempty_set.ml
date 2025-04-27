let of_set_exn set =
  if Core.Set.is_empty set
  then invalid_arg "[Nonempty_set.of_set_exn] called on empty set"
  else set
;;

module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type ('a, 'b) t = ('a, 'b) Set.V1.t

    module M (Elt : sig
        type t
        type comparator_witness
      end) : sig
      type nonrec t = (Elt.t, Elt.comparator_witness) t
    end =
      Set.V1.M (Elt)

    let compare_m__t = Set.V1.compare_m__t
    let m__t_sexp_grammar = Set.V1.m__t_sexp_grammar
    let m__t_of_sexp e s = Set.V1.m__t_of_sexp e s |> of_set_exn
    let sexp_of_m__t = Set.V1.sexp_of_m__t
    let bin_shape_m__t = Set.V1.bin_shape_m__t
    let bin_size_m__t = Set.V1.bin_size_m__t
    let bin_write_m__t = Set.V1.bin_write_m__t
    let bin_read_m__t e buf ~pos_ref = of_set_exn (Set.V1.bin_read_m__t e buf ~pos_ref)
    let __bin_read_m__t__ = Set.V1.__bin_read_m__t__
    let stable_witness_m__t = Set.V1.stable_witness_m__t

    let%expect_test _ =
      print_endline [%bin_digest: M(Int.V1).t];
      [%expect {| 3564446b0bfa871d8c3ebf31ab342fe7 |}]
    ;;
  end
end

open! Core
include Nonempty_set_intf

type ('a, 'b) t = ('a, 'b) Set.t

let add = Set.add
let to_set t = t
let of_set set = if Set.is_empty set then None else Some set
let to_list t = Set.to_list t
let of_list c l = if List.is_empty l then None else Some (Set.of_list c l)

let of_list_exn c l =
  if List.is_empty l
  then invalid_arg "[Nonempty_set.of_list_exn] called on empty list"
  else Set.of_list c l
;;

let of_nonempty_list c l = Set.of_list c (Nonempty_list.to_list l)
let singleton = Set.singleton
let fold t ~init ~f = Set.fold t ~init ~f
let iter t ~f = Set.iter t ~f
let reduce t ~map ~f = Set.to_sequence t |> Sequence.map ~f:map |> Sequence.reduce_exn ~f
let to_nonempty_list t = Nonempty_list.of_list_exn (to_list t)
let map = Set.map
let union = Set.union
let union_set = Set.union
let union_set_list t l = Set.union_list (Set.comparator_s t) (t :: l)
let union_list (t :: l : _ t Nonempty_list.t) = union_set_list t l
let diff = Set.diff
let mem = Set.mem
let length = Set.length
let of_set_add = Set.add
let to_set_remove = Set.remove
let remove t elt = of_set (Set.remove t elt)
let equal = Set.equal
let is_subset = Set.is_subset
let max_elt = Set.max_elt_exn
let min_elt = Set.min_elt_exn
let choose = Set.choose_exn

let%expect_test "min/max elt" =
  Quickcheck.test
    [%quickcheck.generator: Int.t Nonempty_list.t]
    ~sexp_of:[%sexp_of: Int.t Nonempty_list.t]
    ~f:(fun list ->
      let t = of_nonempty_list (module Int) list in
      [%test_result: int]
        ~expect:(Nonempty_list.max_elt' ~compare:Int.compare list)
        (max_elt t);
      [%test_result: int]
        ~expect:(Nonempty_list.min_elt' ~compare:Int.compare list)
        (min_elt t));
  [%expect {| |}]
;;

module M (Elt : sig
    type t
    type comparator_witness
  end) : sig
  type nonrec t = (Elt.t, Elt.comparator_witness) t
end =
  Set.M (Elt)

let hash_fold_m__t = Set.hash_fold_m__t
let hash_m__t = Set.hash_m__t
let equal_m__t = Set.equal_m__t
let compare_m__t = Set.compare_m__t
let m__t_sexp_grammar = Set.m__t_sexp_grammar
let m__t_of_sexp e s = Set.m__t_of_sexp e s |> of_set_exn
let sexp_of_m__t = Set.sexp_of_m__t
let bin_shape_m__t = Set.bin_shape_m__t
let bin_size_m__t = Set.bin_size_m__t
let bin_write_m__t = Set.bin_write_m__t
let bin_read_m__t e buf ~pos_ref = of_set_exn (Set.bin_read_m__t e buf ~pos_ref)
let __bin_read_m__t__ = Set.__bin_read_m__t__

let quickcheck_generator_m__t m =
  Quickcheck.Generator.filter_map ~f:of_set (Set.quickcheck_generator_m__t m)
;;

let quickcheck_observer_m__t m =
  Quickcheck.Observer.unmap ~f:to_set (Set.quickcheck_observer_m__t m)
;;

let quickcheck_shrinker_m__t m =
  Quickcheck.Shrinker.filter_map
    ~f:of_set
    ~f_inverse:to_set
    (Set.quickcheck_shrinker_m__t m)
;;
