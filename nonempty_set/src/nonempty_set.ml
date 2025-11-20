module Nonempty_set0 = struct
  open! Core

  type ('a, 'b) t = { nonempty : ('a, 'b) Set.t } [@@unboxed]

  let to_set t = t.nonempty
  let of_set set = if Set.is_empty set then Null else This { nonempty = set }

  let of_set_exn set =
    match of_set set with
    | This t -> t
    | Null -> invalid_arg "[Nonempty_set.of_set_exn] called on empty set"
  ;;
end

module Stable = struct
  open! Core.Core_stable

  module V1 = struct
    type ('a, 'b) t = ('a, 'b) Nonempty_set0.t = { nonempty : ('a, 'b) Set.V1.t }
    [@@unboxed]

    module M (Elt : sig
        type t
        type comparator_witness
      end) : sig
      type nonrec t = (Elt.t, Elt.comparator_witness) t
    end = struct
      type nonrec t = (Elt.t, Elt.comparator_witness) t
    end

    let hash_fold_m__t hasher state t = Set.V1.hash_fold_m__t hasher state t.nonempty
    let hash_m__t hasher t = Set.V1.hash_m__t hasher t.nonempty

    let%template compare_m__t m t1 t2 =
      (Set.V1.compare_m__t [@mode m]) m t1.nonempty t2.nonempty
    [@@mode m = (local, global)]
    ;;

    let%template equal_m__t m t1 t2 =
      (Set.V1.equal_m__t [@mode m]) m t1.nonempty t2.nonempty
    [@@mode m = (local, global)]
    ;;

    let m__t_sexp_grammar m : ('a, 'b) t Sexplib0.Sexp_grammar.t =
      Sexplib0.Sexp_grammar.coerce (Set.V1.m__t_sexp_grammar m)
    ;;

    let m__t_of_sexp e s = Set.V1.m__t_of_sexp e s |> Nonempty_set0.of_set_exn
    let sexp_of_m__t m t = Set.V1.sexp_of_m__t m t.nonempty
    let bin_shape_m__t = Set.V1.bin_shape_m__t
    let bin_size_m__t m t = Set.V1.bin_size_m__t m t.nonempty
    let bin_write_m__t m buf ~pos t = Set.V1.bin_write_m__t m buf ~pos t.nonempty

    let bin_read_m__t e buf ~pos_ref =
      Nonempty_set0.of_set_exn (Set.V1.bin_read_m__t e buf ~pos_ref)
    ;;

    let __bin_read_m__t__ m buf ~pos_ref i =
      Nonempty_set0.of_set_exn (Set.V1.__bin_read_m__t__ m buf ~pos_ref i)
    ;;

    let stable_witness_m__t elt =
      Stable_witness.of_serializable
        (Set.V1.stable_witness_m__t elt)
        Nonempty_set0.of_set_exn
        Nonempty_set0.to_set
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: M(Int.V1).t];
      [%expect {| 3564446b0bfa871d8c3ebf31ab342fe7 |}]
    ;;
  end
end

open! Core
include Nonempty_set_intf
include Nonempty_set0

let of_set_add set el : _ t =
  (* Safe: adding an element to any set (even empty) produces a nonempty set *)
  { nonempty = Set.add set el }
;;

let add t elem : _ t = of_set_add t.nonempty elem
let of_set_or_null : _ Set.t -> _ t or_null = Nonempty_set0.of_set
let of_set set : _ t option = Or_null.to_option (of_set_or_null set)
let to_list t = Set.to_list t.nonempty

let of_list_or_null c l : _ t or_null =
  if List.is_empty l
  then Null
  else
    (* Safe: We checked nonemptiness of [l] which implies the same for the set *)
    This { nonempty = Set.of_list c l }
;;

let of_list c l : _ t option = Or_null.to_option (of_list_or_null c l)

let of_list_exn c l : _ t =
  match of_list_or_null c l with
  | This t -> t
  | Null -> invalid_arg "[Nonempty_set.of_list_exn] called on empty list"
;;

let comparator_s (t : _ t) = Set.comparator_s t.nonempty

let of_nonempty_list c l : _ t =
  (* Safe: a nonempty list always produces a nonempty set *)
  { nonempty = Set.of_list c (Nonempty_list.to_list l) }
;;

let singleton cmp el : _ t =
  (* Safe: a singleton set is always nonempty *)
  { nonempty = Set.singleton cmp el }
;;

let fold t ~init ~f = Set.fold t.nonempty ~init ~f
let iter t ~f : unit = Set.iter t.nonempty ~f

let reduce t ~map ~f =
  (* Safe: Sequence.reduce_exn only raises on empty sequences, but t.nonempty is
     guaranteed to be nonempty, so the sequence will have at least one element *)
  Set.to_sequence t.nonempty |> Sequence.map ~f:map |> Sequence.reduce_exn ~f
;;

let to_nonempty_list t =
  (* Safe: of_list_exn only raises on empty lists, but to_list of a nonempty set produces
     a nonempty list *)
  Nonempty_list.of_list_exn (to_list t)
;;

let map cmp t ~f : _ t =
  (* Safe: mapping a nonempty set always produces at least one element in the result, even
     if multiple elements map to the same value (they just collapse to one) *)
  { nonempty = Set.map cmp t.nonempty ~f }
;;

let union_set (t : _ t) set : _ t =
  (* Safe: union with a nonempty set always produces a nonempty result *)
  { nonempty = Set.union t.nonempty set }
;;

let union (t1 : _ t) (t2 : _ t) : _ t =
  (* Safe: delegates to union_set which is safe *)
  union_set t1 t2.nonempty
;;

let union_set_list (t : _ t) l : _ t =
  (* Safe: union_list with a nonempty set in the list always produces nonempty result *)
  { nonempty = Set.union_list (comparator_s t) (to_set t :: l) }
;;

let union_list (t :: l : _ t Nonempty_list.t) : _ t =
  union_set_list t (List.map ~f:to_set l)
;;

let to_set_inter t1 t2 : _ Set.t = Set.inter t1.nonempty t2.nonempty
let inter t1 t2 : _ t option = of_set (to_set_inter t1 t2)
let inter_or_null t1 t2 : _ t or_null = of_set_or_null (to_set_inter t1 t2)
let diff t1 t2 = Set.diff t1.nonempty t2.nonempty
let mem t el : bool = Set.mem t.nonempty el
let length t : int = Set.length t.nonempty
let to_set_remove t elt : _ Set.t = Set.remove t.nonempty elt
let remove t elt : _ t option = of_set (to_set_remove t elt)
let remove_or_null t elt : _ t or_null = of_set_or_null (to_set_remove t elt)

let%template equal t1 t2 : bool = (Set.equal [@mode m]) t1.nonempty t2.nonempty
[@@mode m = (global, local)]
;;

let is_subset t ~of_ : bool = Set.is_subset t.nonempty ~of_:of_.nonempty

let max_elt t =
  (* Safe: Set.max_elt_exn only raises on empty sets, but t.nonempty is guaranteed
     nonempty *)
  Set.max_elt_exn t.nonempty
;;

let min_elt t =
  (* Safe: Set.min_elt_exn only raises on empty sets, but t.nonempty is guaranteed
     nonempty *)
  Set.min_elt_exn t.nonempty
;;

let choose t =
  (* Safe: Set.choose_exn only raises on empty sets, but t.nonempty is guaranteed nonempty *)
  Set.choose_exn t.nonempty
;;

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

module M = Stable.V1.M
include (Stable.V1 : For_deriving.S_serializable with type ('a, 'b) t := ('a, 'b) t)
include (Stable.V1 : For_deriving.S_common with type ('a, 'b) t := ('a, 'b) t)

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
