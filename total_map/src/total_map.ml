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

    [%%template
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
      [@modality p]) =
    struct
      module Key = struct
        include Key

        include%template Comparable.V1.Make [@modality p] (Key)
      end

      type comparator_witness = Key.comparator_witness
      type enumeration_witness = Key.enumeration_witness
      type nonrec 'a t = 'a Key.Map.t [@@deriving bin_io, sexp, compare ~localize]
    end

    module%template.portable
      [@modality p] Make_for_include_functor_with_witnesses
        (Key : Key_with_witnesses
      [@modality p]) =
    struct
      module Total_map = Make_with_witnesses [@modality p] (Key)
    end

    module%template.portable
      [@modality p] Make_with_stable_witness
        (Key : Key_with_stable_witness
      [@modality p]) =
    struct
      module Key = struct
        include Key
        include Comparable.V1.With_stable_witness.Make [@modality p] (Key)
      end

      type comparator_witness = Key.comparator_witness
      type enumeration_witness = Key.enumeration_witness

      type nonrec 'a t = 'a Key.Map.t
      [@@deriving bin_io, sexp, compare ~localize, stable_witness]
    end

    module%template.portable
      [@modality p] Make_for_include_functor_with_stable_witness
        (Key : Key_with_stable_witness
      [@modality p]) =
    struct
      module Total_map = Make_with_stable_witness [@modality p] (Key)
    end
  end

  module V2 = struct
    type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp) Map.V1.t

    [%%template
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
      [@modality p] Make_common
        (Key : sig
           include Key_with_witnesses [@modality p]

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
        Sexpable.Of_sexpable1.V1 [@modality p]
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
        Binable.Of_binable1.V2 [@modality p]
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

    module%template.portable
      [@modality p] Make_with_witnesses
        (Key : Key_with_witnesses
      [@modality p]) =
    struct
      module Key = struct
        include Key
        include Comparable.V1.Make [@modality p] (Key)
      end

      module T = struct
        type nonrec 'a t = 'a Key.Map.t [@@deriving compare ~localize]
      end

      include T
      include Make_common [@modality p] (Key) (T)
    end

    module%template.portable
      [@modality p] Make_for_include_functor_with_witnesses
        (Key : Key_with_witnesses
      [@modality p]) =
    struct
      module Total_map = Make_with_witnesses [@modality p] (Key)
    end

    module%template.portable
      [@modality p] Make_with_stable_witness
        (Key : Key_with_stable_witness
      [@modality p]) =
    struct
      module Key = struct
        include Key
        include Comparable.V1.With_stable_witness.Make [@modality p] (Key)
      end

      module T = struct
        type nonrec 'a t = 'a Key.Map.t [@@deriving compare ~localize, stable_witness]
      end

      include T
      include Make_common [@modality p] (Key) (T)
    end

    module%template.portable
      [@modality p] Make_for_include_functor_with_stable_witness
        (Key : Key_with_stable_witness
      [@modality p]) =
    struct
      module Total_map = Make_with_stable_witness [@modality p] (Key)
    end
  end
end

open! Core
open! Import
module Enumeration = Enumeration

type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp, 'enum) Stable.V2.t

[%%template
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
  S [@modality p] with type ('key, 'a, 'cmp, 'enum) total_map := ('key, 'a, 'cmp, 'enum) t

module type For_include_functor =
  For_include_functor
  [@modality p]
  with type ('key, 'a, 'cmp, 'enum) Total_map.total_map := ('key, 'a, 'cmp, 'enum) t]

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

module%template.portable
  [@modality p] Make_plain_with_witnesses
    (Key : Key_plain_with_witnesses
  [@modality p]) =
struct
  module Key = struct
    include Key
    include Comparable.Make_plain_using_comparator [@modality p] (Key)
  end

  type comparator_witness = Key.comparator_witness
  type enumeration_witness = Key.enumeration_witness
  type 'a t = 'a Key.Map.t [@@deriving sexp_of, compare ~localize, equal ~localize]

  include struct
    [@@@mode.default p = (nonportable, p)]

    let quickcheck_generator a_generator =
      let data =
        (Quickcheck.Generator.list_with_length [@mode p])
          (List.length Key.all)
          a_generator
      in
      (Quickcheck.Generator.map [@mode p]) data ~f:(fun data ->
        List.zip_exn Key.all data |> Key.Map.of_alist_exn)
    ;;

    (* Dummy values; maybe we should make them do something someday? *)
    let quickcheck_shrinker _a_shrinker = Quickcheck.Shrinker.empty ()
    let quickcheck_observer _a_observer = Quickcheck.Observer.singleton ()
  end

  let create (local_ f) =
    List.fold
      Key.all
      ~init:(Portability_hacks.magic_uncontended__promise_deeply_immutable Key.Map.empty)
      ~f:(local_ fun t key -> Map.set t ~key ~data:(f key))
    [@nontail]
  ;;

  let create_const x = create (fun _ -> x)

  let named_key_set : _ Set.Named.t =
    { set = Key.Set.of_list Key.all; name = "[Key.all]" }
  ;;

  let of_map map ~if_missing =
    create (fun key ->
      match Map.find map key with
      | Some value -> value
      | None -> if_missing ())
  ;;

  let of_map_exn map =
    Set.Named.equal named_key_set { set = Map.key_set map; name = "[Map.key_set map]" }
    |> ok_exn;
    of_map map ~if_missing:(fun () ->
      raise_s
        [%message
          "impossible: all keys must be present in the map as verified by the key set"])
  ;;

  let of_alist_exn alist = of_map_exn (Key.Map.of_alist_exn alist)
  let of_alist_multi_exn alist = of_map_exn (Key.Map.of_alist_multi alist)

  let of_alist_multi alist =
    of_map (Key.Map.of_alist_multi alist) ~if_missing:(fun () -> [])
  ;;

  include Applicative.Make [@modality p] (struct
      type nonrec 'a t = 'a t

      let return t = create_const t
      let apply t1 t2 = map2 t1 t2 ~f:(fun f x -> f x)
      let map = `Custom map
    end)
end

module%template.portable
  [@modality p] Make_for_include_functor_plain_with_witnesses
    (Key : Key_plain_with_witnesses
  [@modality p]) =
struct
  module Total_map = Make_plain_with_witnesses [@modality p] (Key)
end

module%template.portable
  [@modality p] Make_with_witnesses
    (Key : Key_with_witnesses
  [@modality p]) =
struct
  module Key = struct
    include Key
    include Comparable.Make_binable_using_comparator [@modality p] (Key)
  end

  type 'a t = 'a Key.Map.t [@@deriving sexp, bin_io, compare ~localize, equal ~localize]

  include (
    Make_plain_with_witnesses [@modality p]
      (Key) :
      sig
      @@ p
        include
          S_plain
          [@modality p]
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

  include
    Bin_prot.Utils.Make_binable1_without_uuid [@modality p] [@alert "-legacy"] (struct
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

module%template.portable
  [@modality p] Make_for_include_functor_with_witnesses
    (Key : Key_with_witnesses
  [@modality p]) =
struct
  module Total_map = Make_with_witnesses [@modality p] (Key)
end

module%template.portable [@modality p] Make_plain (Key : Key_plain [@modality p]) =
Make_plain_with_witnesses [@modality p] (struct
    include Key
    include Comparable.Make_plain [@modality p] (Key)
    include Enumeration.Make [@modality p] (Key)
  end)

module%template.portable
  [@modality p] Make_for_include_functor_plain
    (Key : Key_plain
  [@modality p]) =
struct
  module Total_map = Make_plain [@modality p] (Key)
end

module%template.portable [@modality p] Make (Key : Key [@modality p]) =
Make_with_witnesses [@modality p] (struct
    include Key
    include Comparable.Make_binable [@modality p] (Key)
    include Enumeration.Make [@modality p] (Key)
  end)

module%template.portable [@modality p] Make_for_include_functor (Key : Key [@modality p]) =
struct
  module Total_map = Make [@modality p] (Key)
end
