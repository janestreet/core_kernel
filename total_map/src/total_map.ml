include Total_map_intf

open struct
  open Core

  let validate_map_from_serialization
    (type t cmp)
    (module Key : Key_with_witnesses with type t = t and type comparator_witness = cmp)
    (map : (t, _, cmp) Map.t)
    =
    let all_set = Set.of_list (module Key) Key.all in
    let keys = Map.key_set map in
    let keys_minus_all = Set.diff keys all_set in
    let all_minus_keys = Set.diff all_set keys in
    Validate.maybe_raise
      (Validate.of_list
         [ (if Set.is_empty keys_minus_all
            then Validate.get_pass ()
            else
              Validate.fails
                "map from serialization has keys not provided in the enumeration"
                keys_minus_all
                [%sexp_of: Set.M(Key).t])
         ; (if Set.is_empty all_minus_keys
            then Validate.get_pass ()
            else
              Validate.fails
                "map from serialization doesn't have keys it should have"
                all_minus_keys
                [%sexp_of: Set.M(Key).t])
         ])
  ;;
end

module Stable = struct
  open Core.Core_stable
  module V1 = struct end

  module V1_unsafe_deserialization = struct
    type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp) Map.V1.t

    module type S =
      Stable_S with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

    module type S_with_stable_witness =
      Stable_S_with_stable_witness
      with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

    module type For_include_functor =
      Stable_For_include_functor
      with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

    module type For_include_functor_with_stable_witness =
      Stable_For_include_functor_with_stable_witness
      with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

    module Make_with_witnesses (Key : Key_with_witnesses) = struct
      module Key = struct
        include Key
        include Comparable.V1.Make (Key)
      end

      type comparator_witness = Key.comparator_witness
      type enumeration_witness = Key.enumeration_witness
      type nonrec 'a t = 'a Key.Map.t [@@deriving bin_io, sexp, compare]

      let globalize _ t = Base.Map.globalize0 t
    end

    module Make_for_include_functor_with_witnesses (Key : Key_with_witnesses) = struct
      module Total_map = Make_with_witnesses (Key)
    end

    module Make_with_stable_witness (Key : Key_with_stable_witness) = struct
      module Key = struct
        include Key
        include Comparable.V1.With_stable_witness.Make (Key)
      end

      type comparator_witness = Key.comparator_witness
      type enumeration_witness = Key.enumeration_witness
      type nonrec 'a t = 'a Key.Map.t [@@deriving bin_io, sexp, compare, stable_witness]

      let globalize _ t = Base.Map.globalize0 t
    end

    module Make_for_include_functor_with_stable_witness (Key : Key_with_stable_witness) =
    struct
      module Total_map = Make_with_stable_witness (Key)
    end
  end

  module V2 = struct
    type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp) Map.V1.t

    module type S =
      Stable_S with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

    module type S_with_stable_witness =
      Stable_S_with_stable_witness
      with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

    module type For_include_functor =
      Stable_For_include_functor
      with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

    module type For_include_functor_with_stable_witness =
      Stable_For_include_functor_with_stable_witness
      with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

    module Make_common
        (Key : sig
           include Key_with_witnesses

           include
             Comparable.V1.S
             with type comparable := t
              and type comparator_witness := comparator_witness
         end)
        (M : sig
           type 'a t = 'a Key.Map.t
         end) =
    struct
      open M

      type comparator_witness = Key.comparator_witness
      type enumeration_witness = Key.enumeration_witness

      include
        Sexpable.Of_sexpable1.V1
          (Key.Map)
          (struct
            type nonrec 'a t = 'a t

            let to_sexpable t = t

            let of_sexpable map =
              validate_map_from_serialization
                (Base.Portability_hacks.magic_uncontended__promise_deeply_immutable_module
                   (module Key : Key_with_witnesses
                     with type t = Key.t
                      and type comparator_witness = Key.comparator_witness))
                map;
              map
            ;;
          end)

      include
        Binable.Of_binable1.V2
          (Key.Map)
          (struct
            type nonrec 'a t = 'a t

            let to_binable t = t

            let of_binable map =
              validate_map_from_serialization
                (Base.Portability_hacks.magic_uncontended__promise_deeply_immutable_module
                   (module Key : Key_with_witnesses
                     with type t = Key.t
                      and type comparator_witness = Key.comparator_witness))
                map;
              map
            ;;

            let caller_identity =
              Bin_shape.Uuid.of_string "9cb8901d-3d76-43b9-6f50-7b2a92d415f4"
            ;;
          end)
    end

    module Make_with_witnesses (Key : Key_with_witnesses) = struct
      module Key = struct
        include Key
        include Comparable.V1.Make (Key)
      end

      module T = struct
        type nonrec 'a t = 'a Key.Map.t [@@deriving compare]

        let globalize _ t = Base.Map.globalize0 t
      end

      include T
      include Make_common (Key) (T)
    end

    module Make_for_include_functor_with_witnesses (Key : Key_with_witnesses) = struct
      module Total_map = Make_with_witnesses (Key)
    end

    module Make_with_stable_witness (Key : Key_with_stable_witness) = struct
      module Key = struct
        include Key
        include Comparable.V1.With_stable_witness.Make (Key)
      end

      module T = struct
        type nonrec 'a t = 'a Key.Map.t [@@deriving compare, stable_witness]

        let globalize _ t = Base.Map.globalize0 t
      end

      include T
      include Make_common (Key) (T)
    end

    module Make_for_include_functor_with_stable_witness (Key : Key_with_stable_witness) =
    struct
      module Total_map = Make_with_stable_witness (Key)
    end
  end
end

open! Core
open! Import
module Enumeration = Enumeration

type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp, 'enum) Stable.V2.t

module type S_plain =
  S_plain with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

module type For_include_functor_plain =
  For_include_functor_plain
  with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

module type S = S with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

module type For_include_functor =
  For_include_functor
  with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t

let to_map t = t

let key_not_in_enumeration t key =
  failwiths
    "Key was not provided in the enumeration given to [Total_map.Make]"
    key
    (Comparator.sexp_of_t (Map.comparator t))
;;

let change t k ~f =
  Map.update t k ~f:(function
    | Some x -> f x
    | None -> key_not_in_enumeration t k)
;;

let find t k =
  try Map.find_exn t k with
  | _ -> key_not_in_enumeration t k
;;

let pair t1 t2 key = function
  | `Left _ -> key_not_in_enumeration t2 key
  | `Right _ -> key_not_in_enumeration t1 key
  | `Both (v1, v2) -> v1, v2
;;

let iter2 t1 t2 ~f =
  Map.iter2 t1 t2 ~f:(fun ~key ~data ->
    let v1, v2 = pair t1 t2 key data in
    f ~key v1 v2)
;;

let fold2 t1 t2 ~init ~f =
  Map.fold2 t1 t2 ~init ~f:(fun ~key ~data acc ->
    let v1, v2 = pair t1 t2 key data in
    f ~key v1 v2 acc)
;;

let map2 t1 t2 ~f =
  Map.merge t1 t2 ~f:(fun ~key v ->
    let v1, v2 = pair t1 t2 key v in
    Some (f v1 v2))
;;

let mapi2 t1 t2 ~f =
  Map.merge t1 t2 ~f:(fun ~key v ->
    let v1, v2 = pair t1 t2 key v in
    Some (f key v1 v2))
;;

let unzip t = Map.unzip t
let set t key data = Map.set t ~key ~data

module Sequence3 (A : Applicative.S3) = struct
  let sequence t =
    List.fold
      (Map.to_alist t)
      ~init:(A.return (Map.Using_comparator.empty ~comparator:(Map.comparator t)))
      ~f:(fun acc (key, data) ->
        A.map2 acc data ~f:(fun acc data -> Map.set acc ~key ~data))
  ;;
end

module Sequence2 (A : Applicative.S2) = Sequence3 (struct
    include A

    type ('a, 'b, _) t = ('a, 'b) A.t
  end)

module Sequence (A : Applicative) = Sequence2 (struct
    include A

    type ('a, _) t = 'a A.t
  end)

module type M = sig
  type t

  module Total_map : S with type Key.t = t
end

module type M_with_hash = sig
  include M

  val hash_fold_t : Hash.state -> t -> Hash.state
end

module M (T : For_include_functor_plain) = struct
  type 'a t = 'a T.Total_map.t
end

let m__t_of_sexp
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (a_of_sexp : Sexp.t -> a)
  : Sexp.t -> (k, a, cmp, enum) t
  =
  K.Total_map.t_of_sexp a_of_sexp
;;

let sexp_of_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (sexp_of_a : a -> Sexp.t)
  : (k, a, cmp, enum) t -> Sexp.t
  =
  K.Total_map.sexp_of_t sexp_of_a
;;

let m__t_sexp_grammar = Map.Stable.V1.m__t_sexp_grammar

let bin_shape_m__t (module K : For_include_functor) (bin_shape_a : Bin_shape.t)
  : Bin_shape.t
  =
  K.Total_map.bin_shape_t bin_shape_a
;;

let bin_size_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (bin_size_a : a Bin_prot.Size.sizer)
  : (k, a, cmp, enum) t Bin_prot.Size.sizer
  =
  K.Total_map.bin_size_t bin_size_a
;;

let bin_write_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (bin_write_a : a Bin_prot.Write.writer)
  : (k, a, cmp, enum) t Bin_prot.Write.writer
  =
  K.Total_map.bin_write_t bin_write_a
;;

let bin_read_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (bin_read_a : a Bin_prot.Read.reader)
  : (k, a, cmp, enum) t Bin_prot.Read.reader
  =
  K.Total_map.bin_read_t bin_read_a
;;

let __bin_read_m__t__
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (bin_read_a : a Bin_prot.Read.reader)
  : (k, a, cmp, enum) t Bin_prot.Read.vtag_reader
  =
  K.Total_map.__bin_read_t__ bin_read_a
;;

let compare_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (a_compare : a -> a -> int)
  : (k, a, cmp, enum) t -> (k, a, cmp, enum) t -> int
  =
  K.Total_map.compare a_compare
;;

let equal_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (a_equal : a -> a -> bool)
  : (k, a, cmp, enum) t -> (k, a, cmp, enum) t -> bool
  =
  K.Total_map.equal a_equal
;;

let hash_fold_m__t
  (type k cmp enum a)
  (module K : M_with_hash
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  (hash_fold_a : Hash.state -> a -> Hash.state)
  : Hash.state -> (k, a, cmp, enum) t -> Hash.state
  =
  Map.hash_fold_m__t (module K) hash_fold_a
;;

let globalize_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  globalize_a
  : (k, a, cmp, enum) t -> (k, a, cmp, enum) t
  =
  K.Total_map.globalize globalize_a
;;

let quickcheck_generator_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  a_generator
  : (k, a, cmp, enum) t Quickcheck.Generator.t
  =
  K.Total_map.quickcheck_generator a_generator
;;

let quickcheck_observer_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  a_observer
  : (k, a, cmp, enum) t Quickcheck.Observer.t
  =
  K.Total_map.quickcheck_observer a_observer
;;

let quickcheck_shrinker_m__t
  (type k cmp enum a)
  (module K : M
    with type t = k
     and type Total_map.enumeration_witness = enum
     and type Total_map.comparator_witness = cmp)
  a_shrinker
  : (k, a, cmp, enum) t Quickcheck.Shrinker.t
  =
  K.Total_map.quickcheck_shrinker a_shrinker
;;

include struct
  open Map

  let combine_errors = combine_errors
  let data = data
  let for_all = for_all
  let for_alli = for_alli
  let iter = iter
  let iter_keys = iter_keys
  let iteri = iteri
  let map = map
  let mapi = mapi
  let fold = fold
  let fold_right = fold_right
  let to_alist = to_alist
  let validate = validate
  let validatei = validatei
end

module Make_plain_with_witnesses (Key : Key_plain_with_witnesses) = struct
  module Key = struct
    include Key
    include Comparable.Make_plain_using_comparator (Key)
  end

  type comparator_witness = Key.comparator_witness
  type enumeration_witness = Key.enumeration_witness
  type 'a t = 'a Key.Map.t [@@deriving sexp_of, compare, equal]

  let globalize _ t = Base.Map.globalize0 t

  include struct
    let quickcheck_generator a_generator =
      let data =
        Quickcheck.Generator.list_with_length (List.length Key.all) a_generator
      in
      Quickcheck.Generator.map data ~f:(fun data ->
        List.zip_exn Key.all data |> Key.Map.of_alist_exn)
    ;;

    (* Dummy values; maybe we should make them do something someday? *)
    let quickcheck_shrinker _a_shrinker = Quickcheck.Shrinker.empty ()
    let quickcheck_observer _a_observer = Quickcheck.Observer.singleton ()
  end

  let create f =
    List.fold
      Key.all
      ~init:(Portability_hacks.magic_uncontended__promise_deeply_immutable Key.Map.empty)
      ~f:(fun t key -> Map.set t ~key ~data:(f key))
    [@nontail]
  ;;

  let create_const x = create (fun _ -> x)

  let named_key_set : _ Set.Named.t =
    { set = Key.Set.of_list Key.all; name = "[Key.all]" }
  ;;

  let map_has_expected_keys map expected_set =
    (* There's no direct way to compare the set of keys in a map to a given set, so we use
       [Map.Tree.Enum] and [Set.Tree.Enum] to step through the set and the map in
       lockstep. This is ~10x faster, and allocates ~10x less. *)
    let compare_key = Map.comparator map |> Core.Comparator.compare in
    let rec keys_match map_enum set_enum =
      match map_enum, set_enum with
      | Null, Null -> true
      | Null, This _ | This _, Null -> false
      | This map_enum, This set_enum ->
        compare_key (Base.Map.Tree.Enum.key map_enum) (Base.Set.Tree.Enum.elt set_enum)
        = 0
        && (keys_match [@tailcall])
             (Base.Map.Tree.Enum.next map_enum)
             (Base.Set.Tree.Enum.next set_enum)
    in
    keys_match
      (Base.Map.Tree.Enum.of_tree (Map.to_tree map))
      (Base.Set.Tree.Enum.of_set (Set.to_tree expected_set))
  ;;

  let of_map_exn map =
    if not (map_has_expected_keys map named_key_set.set)
    then
      (* If the fast path fails, construct the key set to give a good error message. *)
      Set.Named.equal named_key_set { set = Map.key_set map; name = "[Map.key_set map]" }
      |> ok_exn;
    map
  ;;

  let of_alist_exn alist = of_map_exn (Key.Map.of_alist_exn alist)
  let of_alist_multi_exn alist = of_map_exn (Key.Map.of_alist_multi alist)

  let of_alist_multi alist =
    let map = Key.Map.of_alist_multi alist in
    if map_has_expected_keys map named_key_set.set
    then map
    else
      create (fun key ->
        match Map.find map key with
        | Some value -> value
        | None -> [])
  ;;

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return t = create_const t
      let apply t1 t2 = map2 t1 t2 ~f:(fun f x -> f x)
      let map = `Custom map
    end)
end

module Make_for_include_functor_plain_with_witnesses (Key : Key_plain_with_witnesses) =
struct
  module Total_map = Make_plain_with_witnesses (Key)
end

module Make_with_witnesses (Key : Key_with_witnesses) = struct
  module Key = struct
    include Key
    include Comparable.Make_binable_using_comparator (Key)
  end

  type 'a t = 'a Key.Map.t [@@deriving sexp, bin_io, compare, equal]

  include (
    Make_plain_with_witnesses
      (Key) :
      sig
        include
          S_plain
          with type comparator_witness = Key.comparator_witness
          with type enumeration_witness = Key.enumeration_witness
          with module Key := Key
          with type 'a t := 'a t
      end)

  let t_of_sexp a_of_sexp sexp =
    let t = t_of_sexp a_of_sexp sexp in
    validate_map_from_serialization (module Key) t;
    t
  ;;

  include Bin_prot.Utils.Make_binable1_without_uuid [@alert "-legacy"] (struct
      type nonrec 'a t = 'a t

      module Binable = Key.Map

      let to_binable x = x

      let of_binable x =
        validate_map_from_serialization
          (Portability_hacks.magic_uncontended__promise_deeply_immutable_module
             (module Key : Key_with_witnesses
               with type t = Key.t
                and type comparator_witness = Key.comparator_witness))
          x;
        x
      ;;
    end)
end

module Make_for_include_functor_with_witnesses (Key : Key_with_witnesses) = struct
  module Total_map = Make_with_witnesses (Key)
end

module Make_plain (Key : Key_plain) = Make_plain_with_witnesses (struct
    include Key
    include Comparable.Make_plain (Key)
    include Enumeration.Make (Key)
  end)

module Make_for_include_functor_plain (Key : Key_plain) = struct
  module Total_map = Make_plain (Key)
end

module Make (Key : Key) = Make_with_witnesses (struct
    include Key
    include Comparable.Make_binable (Key)
    include Enumeration.Make (Key)
  end)

module Make_for_include_functor (Key : Key) = struct
  module Total_map = Make (Key)
end
