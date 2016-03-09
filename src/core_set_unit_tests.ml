module Caml_set = Set
open Std
open Core_set_intf

module Unit_tests
  (Elt : sig
    type 'a t [@@deriving sexp]

    val of_int : int -> int t
    val to_int : int t -> int
  end)
  (Set : sig
    type ('a, 'b) t_
    type ('a, 'b) set
    type ('a, 'b) tree

    type ('a, 'b, 'c) create_options

    include Creators_generic
      with type ('a, 'b) t           := ('a, 'b) t_
      with type ('a, 'b) set         := ('a, 'b) set
      with type ('a, 'b) tree        := ('a, 'b) tree
      with type 'a elt               := 'a Elt.t
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) create_options

    val simplify_creator : (int, Int.comparator_witness, 'c) create_options -> 'c

    type ('a, 'b, 'c) access_options

    include Accessors_generic
      with type ('a, 'b) t           := ('a, 'b) t_
      with type ('a, 'b) tree        := ('a, 'b) tree
      with type 'a elt               := 'a Elt.t
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) access_options
      with type 'cmp cmp             := 'cmp cmp

    val simplify_accessor : (int, Int.comparator_witness, 'c) access_options -> 'c

    val kind : [ `Set | `Tree ]
  end)

  : Creators_and_accessors_generic = struct

  module Set = struct
    include Set
    let add            = simplify_accessor add
    let remove         = simplify_accessor remove
    let mem            = simplify_accessor mem
    (* let filter         = simplify_accessor filter *)
    let compare_direct = simplify_accessor compare_direct
    let equal          = simplify_accessor equal
    let inter          = simplify_accessor inter
    let union          = simplify_accessor union
    let subset         = simplify_accessor subset
    let iter2          = simplify_accessor iter2
    let invariants     = simplify_accessor invariants
    let to_map         = simplify_accessor to_map
    let shrinker       = simplify_accessor shrinker
    let to_list    = to_list
    let to_array   = to_array
    let to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x =
      simplify_accessor to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x
    let merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x y =
      simplify_accessor merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x y

    let empty ()       = simplify_creator empty
    let singleton      = simplify_creator singleton
    let of_list        = simplify_creator of_list
    let of_sorted_array = simplify_creator of_sorted_array
    let of_sorted_array_unchecked = simplify_creator of_sorted_array_unchecked
    (* let of_tree        = simplify_creator of_tree *)
    let gen            = simplify_creator gen
    let symmetric_diff = simplify_accessor symmetric_diff
    let split          = simplify_accessor split
    let diff           = simplify_accessor diff

    let sexp_of_t_ t = [%sexp_of: int Elt.t list] (to_list t)
  end

  type ('a, 'b) t = Unit_test_follows
  type ('a, 'b) tree = ('a, 'b) t
  type ('a, 'b) set = ('a, 'b) t
  type 'a elt
  type ('a, 'b, 'c) options = ('a, 'b, 'c) Without_comparator.t
  type 'a cmp

  module Elt = struct
    open Elt
    let of_int = of_int
    let to_int = to_int

    let gen =
      let open Quickcheck.Generator in
      Int.gen >>| of_int

    let obs =
      Quickcheck.Observer.unmap Int.obs
        ~f:to_int
        ~f_sexp:(fun () -> Atom "Elt.to_int")

    module T = struct
      type t = int Elt.t [@@deriving sexp]
      let compare t t' = Pervasives.compare (to_int t) (to_int t')
      let equal t t' = compare t t' = 0
    end
    include T

    let samples = List.dedup ~compare (List.init 10 ~f:(fun i -> of_int (i + 1)))
    let absent = of_int 0
    let present = of_int 1
    let () = assert(List.mem ~equal samples present)
    let () = assert(not (List.mem ~equal samples absent))
  end

  let set_empty = Set.empty ()
  let set_nonempty = Set.of_list Elt.samples

  let gen_set =
    let open Quickcheck.Generator in
    List.gen Elt.gen >>| Set.of_list

  let add _     = assert false
  let of_list _ = assert false
  let mem _     = assert false

  let%test _ = List.for_all Elt.samples ~f:(fun e -> Set.mem set_nonempty e)

  let is_empty _ = assert false
  let%test _ = Set.is_empty set_empty
  let%test _ = not (Set.is_empty set_nonempty)

  let%test _ =
    let set' = List.fold Elt.samples ~init:(Set.empty ()) ~f:Set.add in
    Set.equal set_nonempty set'
  ;;

  let inter _ = assert false
  let%test _ = Set.is_empty (Set.inter set_empty set_nonempty)
  let%test _ = Set.is_empty (Set.inter set_nonempty set_empty)
  let%test _ =
    let single = Set.singleton Elt.absent in
    Set.equal single (Set.inter single (Set.add set_nonempty Elt.absent))
  ;;
  let%test _ = Set.equal set_nonempty (Set.inter set_nonempty set_nonempty)

  let subset _ = assert false
  let%test _ = Set.subset set_empty set_nonempty
  let%test _ = not (Set.subset set_nonempty set_empty)
  let%test _ = Set.subset set_nonempty set_nonempty
  let%test _ = Set.subset set_empty set_empty
  let%test _ = not (Set.subset set_nonempty (Set.singleton Elt.present))

  let to_list _ = assert false
  let%test _ =
    let elts = Set.to_list set_nonempty in
    List.for_all elts ~f:(fun elt ->
      Set.mem set_nonempty elt)
  ;;

  let rec is_list_ordered_ascending xs =
    match xs with
    | [] | [_] -> true
    | a :: b :: xs' ->
      Elt.compare a b < 0 && is_list_ordered_ascending (b :: xs')
  ;;

  let%test _ =
    is_list_ordered_ascending (Set.to_list set_nonempty)
  ;;

  let to_array _ = assert false
  let%test _ =
    let a = Set.to_array set_nonempty in
    List.equal (Array.to_list a) (Set.to_list set_nonempty) ~equal:Elt.equal
  ;;

  let to_sequence ?order:_ ?greater_or_equal_to:_ ?less_or_equal_to:_ _ : _ Sequence.t = assert false

  let%test_module "to_sequence" = (module struct

    let (<=>) observed expected =
      [%test_eq: Elt.t list] (Sequence.to_list observed) expected
    ;;

    let m = set_nonempty

    (* Calibration: make sure [m] contains elements less than 4, greater than 8, and
       between these two. Otherwise the tests aren't testing what we want. *)
    let%test _ =
      let l = Set.to_list m in
      List.exists l ~f:(fun x -> x < Elt.of_int 4)
      &&
      List.exists l ~f:(fun x -> x >= Elt.of_int 4 && x <= Elt.of_int 8)
      &&
      List.exists l ~f:(fun x -> x > Elt.of_int 8)
    ;;

    let%test_unit _ = Set.to_sequence ~order:`Increasing m <=> Set.to_list m

    let%test_unit _ = Set.to_sequence ~order:`Decreasing m <=> List.rev (Set.to_list m)

    let%test_unit _ =
      Set.to_sequence ~order:`Increasing ~greater_or_equal_to:(Elt.of_int 4) m
      <=>
      List.filter ~f:(fun x -> x >= Elt.of_int 4) (Set.to_list m)

    let%test_unit _ =
      Set.to_sequence m
        ~order:`Increasing
        ~greater_or_equal_to:(Elt.of_int 4)
        ~less_or_equal_to:(Elt.of_int 8)
      <=>
      List.filter ~f:(fun x -> x >= Elt.of_int 4 && x <= Elt.of_int 8) (Set.to_list m)

    let%test_unit _ =
      Set.to_sequence ~order:`Decreasing ~less_or_equal_to:(Elt.of_int 4) m
      <=>
      List.filter ~f:(fun x -> x <= Elt.of_int 4) (List.rev (Set.to_list m))

    let%test_unit _ =
      Set.to_sequence m
        ~order:`Decreasing
        ~less_or_equal_to:(Elt.of_int 8)
        ~greater_or_equal_to:(Elt.of_int 4)
      <=>
      List.filter ~f:(fun x -> x <= Elt.of_int 8 && x >= Elt.of_int 4)
        (List.rev (Set.to_list m))

    let%test_unit _ = Set.to_sequence ~order:`Increasing (Set.empty ()) <=> []

    let%test_unit _ = Set.to_sequence ~order:`Decreasing (Set.empty ()) <=> []

    let%test_unit _ =
      Set.to_sequence ~order:`Increasing ~greater_or_equal_to:(Elt.of_int 11) m <=> []

    let%test_unit _ =
      Set.to_sequence ~order:`Decreasing ~less_or_equal_to:(Elt.of_int ~-1) m <=> []

  end)

  let merge_to_sequence ?order:_ ?greater_or_equal_to:_ ?less_or_equal_to:_ _ _ : _ Sequence.t = assert false
  let%test_module "merge_to_sequence Quickcheck" =
    (module struct

      open Quickcheck

      module Merge_to_sequence_args = struct
        type t = {
          order : [ `Increasing | `Decreasing ] option;
          greater_or_equal_to : Elt.t option;
          less_or_equal_to : Elt.t option;
          x : (int, Int.comparator_witness) Set.t_;
          y : (int, Int.comparator_witness) Set.t_;
        }

        let gen =
          let open Generator.Monad_infix in
          let int_set_gen =
            Generator.size
            >>= fun size ->
            List.gen' ~length:(`Exactly size) ~unique:true Int.gen
            >>| fun ints ->
            Set.of_list (List.map ints ~f:Elt.of_int)
          in
          Generator.tuple5
            (Option.gen (Generator.of_list [ `Increasing; `Decreasing; ]))
            (Option.gen (Int.gen >>| Elt.of_int))
            (Option.gen (Int.gen >>| Elt.of_int))
            int_set_gen
            int_set_gen
          >>| fun (order, greater_or_equal_to, less_or_equal_to, x, y) ->
          { order; greater_or_equal_to; less_or_equal_to; x; y; }
        ;;
      end

      let%test_unit "merge_to_sequence = symmetric diff + inter" =
        Quickcheck.test Merge_to_sequence_args.gen ~f:(fun {
          order; greater_or_equal_to; less_or_equal_to; x; y ;
        } ->
          let open Merge_to_sequence_element in
          let value = function
            | Left x | Right x | Both (x, _) -> Elt.to_int x
          in
          let expect = List.concat [
            List.map ~f:(fun x -> Both (x, x)) (Set.to_list (Set.inter x y));
            List.map ~f:(fun x -> Left x) (Set.to_list (Set.diff x y));
            List.map ~f:(fun x -> Right x) (Set.to_list (Set.diff y x));
          ] in
          let expect =
            Option.fold greater_or_equal_to ~init:expect ~f:(fun elts min ->
              let min = Elt.to_int min in
              List.filter elts ~f:(fun x -> Int.(>=) (value x) min))
          in
          let expect =
            Option.fold less_or_equal_to ~init:expect ~f:(fun elts max ->
              let max = Elt.to_int max in
              List.filter elts ~f:(fun x -> Int.(<=) (value x) max))
          in
          let expect =
            match order with
            | None | Some `Increasing ->
              List.sort expect ~cmp:(fun a b -> Int.compare (value a) (value b))
            | Some `Decreasing ->
              List.sort expect ~cmp:(fun a b -> Int.compare (value b) (value a))
          in
          [%test_result: Elt.t Merge_to_sequence_element.t list]
            (Sequence.to_list
               (Set.merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x y))
            ~expect)
      ;;

    end)

  let of_sorted_array _ = assert false
  let of_sorted_array_unchecked _ = assert false

  let%test _ = Set.of_sorted_array [||] |! Result.is_ok
  let%test _ = Set.of_sorted_array [|Elt.of_int 0|] |! Result.is_ok
  let%test _ = Set.of_sorted_array [|Elt.of_int 0; Elt.of_int 0|] |! Result.is_error
  let%test _ = Set.of_sorted_array [|Elt.of_int 1
                             ; Elt.of_int 0
                             ; Elt.of_int 1|] |! Result.is_error

  let%test _ =
    let list = List.init 100 ~f:Elt.of_int in
    let array = Array.of_list list in
    let rev_array = Array.of_list (List.rev list) in
    Set.equal (Set.of_list list) (Set.of_sorted_array_unchecked array)
    && Set.equal (Set.of_list list) (Set.of_sorted_array_unchecked rev_array)
  ;;

  let invariants _ = assert false

  let%test_unit _ =
    for n = 0 to 100 do
      let list = List.init n ~f:Elt.of_int in
      assert (List.permute list |! Set.of_list |! Set.invariants);
      assert (Array.of_list list |! Set.of_sorted_array_unchecked |! Set.invariants);
      assert (List.rev list |! Array.of_list |! Set.of_sorted_array_unchecked |! Set.invariants);
    done
  ;;

  let iter2 _ = assert false

  let%test_unit _ =
    let test l1 l2 expected =
      let result = ref [] in
      let set_of_list l = Set.of_list (List.map l ~f:Elt.of_int) in
      Set.iter2 (set_of_list l1) (set_of_list l2) ~f:(fun a -> result := a :: !result);
      let result =
        List.rev_map !result ~f:(function
        | `Left a -> `Left (Elt.to_int a)
        | `Right a -> `Right (Elt.to_int a)
        | `Both (a, b) -> `Both (Elt.to_int a, Elt.to_int b)
        )
      in
      assert (result = expected)
    in
    test [] [] [];
    test [0] [] [`Left 0];
    test [] [0] [`Right 0];
    test
      [0; 1; 3; 4]
      [3; 4; 5; 6]
      [`Left 0; `Left 1;
       `Both (3, 3); `Both (4, 4);
       `Right 5; `Right 6
      ]
  ;;

  (* Ensure polymorphic equality raises for sets. *)
  let%test_unit _ =
    match Set.kind with
    | `Tree -> ()
    | `Set ->
      let ts = [ Set.empty (); Set.of_list [ Elt.of_int 13 ] ] in
      List.iter ts ~f:(fun t1 ->
        List.iter ts ~f:(fun t2 ->
          assert (Result.is_error (Result.try_with (fun () -> Poly.equal t1 t2)))))
  ;;

  let to_map _ = assert false

  let%test_unit _ = assert (Map.is_empty (Set.to_map set_empty ~f:Elt.to_int))

  let%test_unit _ =
    let s = set_nonempty in
    let m = Set.to_map s ~f:Elt.to_int in
    assert (Set.length s = Map.length m);
    Map.iteri m ~f:(fun ~key ~data -> assert (Elt.to_int key = data));
    assert (Set.to_list s = Map.keys m)
  ;;

  let of_map_keys _ = assert false

  let%test_unit _ =
    assert (Set.is_empty (Set.of_map_keys (Set.to_map set_empty ~f:Elt.to_int)))
  let%test_unit _ =
    assert
      (Set.equal (Set.of_map_keys (Set.to_map set_nonempty ~f:Elt.to_int)) set_nonempty)

  let symmetric_diff _ = assert false

  let%test_unit "symmetric diff quick" =
    let symmetric_diff_set s1 s2 =
      Set.symmetric_diff s1 s2
      |> Sequence.to_list
      |> List.map ~f:(function | First elt | Second elt -> elt)
      |> Set.of_list
    in
    (* Textbook definition of symmetric difference: *)
    let symmetric_diff_spec s1 s2 = Set.diff (Set.union s1 s2) (Set.inter s1 s2) in
    Quickcheck.test
      ~seed:(`Deterministic "core set symmetric diff")
      (Quickcheck.Generator.tuple2 gen_set gen_set)
      ~sexp_of:[%sexp_of: Set.t_ * Set.t_]
      ~f:(fun (s1, s2) ->
        let expect = symmetric_diff_spec s1 s2 in
        let actual = symmetric_diff_set s1 s2 in
        assert (Set.equal actual expect))

  let%test _ =
    let m1 = set_nonempty in
    Sequence.to_list (Set.symmetric_diff m1 m1) = []
  ;;

  let%test _ =
    let elt = Elt.of_int 7 in
    let m1 = Set.empty () in
    let m1 = Set.add m1 (Elt.of_int 1) in
    let m2 = Set.add m1 elt in
    Sequence.to_list (Set.symmetric_diff m1 m2) = [Second elt]
  ;;

  let%test _ =
    let m1 = set_nonempty in
    let m2 =
      List.fold (Set.to_list m1) ~init:(Set.empty ()) ~f:(fun m k ->
        Set.add m k)
    in
    Sequence.to_list (Set.symmetric_diff m1 m2) = []
  ;;

  let%test _ =
    let elt = Elt.of_int 20 in
    let m1 = set_nonempty in
    let m2 = Set.add m1 elt in
    Sequence.to_list (Set.symmetric_diff m1 m2) = [Second elt]
  ;;

  let%test _ =
    let elt = Elt.of_int 5 in
    let m1 = set_nonempty in
    let m2 = Set.remove m1 elt in
    Sequence.to_list (Set.symmetric_diff m1 m2) = [First elt]
  ;;

  let set_of_int_list l =
    Set.of_list (List.map ~f:Elt.of_int l)

  let%test _ =
    let map1 = Set.empty () in
    let map2 =
      set_of_int_list
        [ 1
        ; 2
        ; 3
        ; 4
        ; 5
        ]
    in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 5
  ;;

  let%test _ =
    let map1 =
      set_of_int_list
        [ 1
        ; 2
        ; 3
        ; 4
        ; 5
        ]
    in
    let map2 = Set.empty () in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 5
  ;;

  let%test _ =
    let map1 =
      set_of_int_list
        [ 1
        ; 2
        ]
    in
    let map2 =
      List.fold
        [ 3
        ; 4
        ; 5 ]
        ~init:map1
        ~f:(fun acc elt -> Set.add acc (Elt.of_int elt))
    in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 3
  ;;

  let%test _ =
    let map2 =
      set_of_int_list
        [ 1; 2 ]
    in
    let map1 =
      List.fold [ 3; 4; 5 ]
        ~init:map2
        ~f:(fun acc elt -> Set.add acc (Elt.of_int elt))
    in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 3
  ;;

  let split _ = assert false

  module Simple_int_set = struct
    type t = int list [@@deriving compare, sexp]

    let init i = List.init i ~f:Fn.id

    let split t i =
      let l = List.filter t ~f:(Int.(>) i) in
      let r = List.filter t ~f:(Int.(<) i) in
      let x =
        if List.mem t i ~equal:(Int.(=))
        then Some (Elt.of_int i)
        else None
      in
      (l, x, r)
    ;;

    let to_set = set_of_int_list
  end

  let%test_unit _ =
    let n = 16 in
    let t = set_of_int_list (List.init n ~f:Fn.id) in
    let t' = Simple_int_set.init n in
    let check i =
      let l, x, r = Set.split t (Elt.of_int i) in
      let l', x', r' = Simple_int_set.split t' i in
      assert (Set.equal l (Simple_int_set.to_set l'));
      [%test_eq: Elt.t option] x x';
      assert (Set.equal r (Simple_int_set.to_set r'));
    in
    for i = 0 to n - 1 do check i done
  ;;

  let gen _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck
      let sexp_of t = to_list t |> [%sexp_of: Elt.t list]
      let by = `Compare Set.compare_direct
      let gen = Set.gen Elt.gen
      let can_generate f = test_can_generate gen ~sexp_of ~f

      let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
      let%test_unit _ = can_generate (fun t -> Set.is_empty t)
      let%test_unit _ = can_generate (fun t -> Set.length t = 1)
      let%test_unit _ = can_generate (fun t -> Set.length t = 2)
      let%test_unit _ = can_generate (fun t -> Set.length t >= 3)
      let%test_unit _ = can_generate (fun t -> Set.mem t (Elt.of_int 0))
      let%test_unit _ = can_generate (fun t -> Set.mem t (Elt.of_int (-1)))
    end)

  let obs _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck
      let sexp_of = Generator.fn_sexp
      let by = `Compare [%compare: Sexp.t]
      let gen =
        Generator.fn_with_sexp (Set.obs Elt.obs) Int.gen
          ~sexp_of_rng:[%sexp_of: int]
      let can_generate f = test_can_generate gen ~sexp_of ~f

      let%test_unit _ =
        test_no_duplicates (Generator.map gen ~f:sexp_of) ~sexp_of:Fn.id ~by

      let%test_unit _ = can_generate (fun (f, _) ->
        f (Set.singleton (Elt.of_int 0)) <> f (Set.empty ()))
      let%test_unit _ = can_generate (fun (f, _) ->
        f (Set.singleton (Elt.of_int 0)) <> f (Set.singleton (Elt.of_int 1)))
      let%test_unit _ = can_generate (fun (f, _) ->
        f (Set.singleton (Elt.of_int 0)) <> f (Set.of_list [Elt.of_int 0; Elt.of_int 1]))
    end)

  let shrinker _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      let elt_test_shrinker =
        Shrinker.create (fun t ->
          let n = Elt.to_int t in
          if n <= 0
          then Sequence.empty
          else Sequence.singleton (Elt.of_int (n - 1)))

      let shrinker = Set.shrinker elt_test_shrinker

      let normalize_list list =
        List.sort list ~cmp:[%compare: int]

      let normalize_lists lists =
        List.map lists ~f:normalize_list
        |> List.sort ~cmp:[%compare: int list]

      let set_of_list list =
        List.map list ~f:Elt.of_int
        |> Set.of_list

      let list_of_set set =
        Set.to_list set
        |> List.map ~f:Elt.to_int

      let test list shrunk_lists =
        [%test_result: int list list]
          (set_of_list list
           |> Shrinker.shrink shrinker
           |> Sequence.to_list
           |> List.map ~f:list_of_set
           |> normalize_lists)
          ~expect:(normalize_lists shrunk_lists)

      let%test_unit _ = test [] []
      let%test_unit _ = test [0] [[]]
      let%test_unit _ = test [1] [[];[0]]
      let%test_unit _ = test [0;1] [[0];[1];[0]]
    end)


  let to_tree _           = assert false
  let remove_index _      = assert false
  let find_index _        = assert false
  let find_exn _          = assert false
  let group_by _          = assert false
  let choose_exn _        = assert false
  let choose _            = assert false
  let max_elt_exn _       = assert false
  let max_elt _           = assert false
  let min_elt_exn _       = assert false
  let min_elt _           = assert false
  let elements _          = assert false
  let partition_tf _      = assert false
  let filter _            = assert false
  let fold_right _        = assert false
  let fold_until _        = assert false
  let equal _             = assert false
  let compare_direct _    = assert false
  let diff _              = assert false
  let union _             = assert false
  let remove _            = assert false
  let find_map _          = assert false
  let find _              = assert false
  let count _             = assert false
  let sum _               = assert false
  let for_all _           = assert false
  let exists _            = assert false
  let fold _              = assert false
  let iter _              = assert false
  let length _            = assert false
  let of_tree _           = assert false
  let filter_map _        = assert false
  let map _               = assert false
  let stable_dedup_list _ = assert false
  let of_array _          = assert false
  let union_list _        = assert false
  let singleton _         = assert false
  let empty               = Unit_test_follows

end

module Elt_int = struct
  type 'a t = int [@@deriving sexp]
  let of_int = Fn.id
  let to_int = Fn.id
end

module Elt_poly = struct
  type 'a t = 'a [@@deriving sexp]
  let of_int = Fn.id
  let to_int = Fn.id
end

module Create_options_with_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) With_comparator.t
  let simplify_creator f = f ~comparator:Core_int.comparator
end

module Create_options_without_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) Without_comparator.t
  let simplify_creator = Fn.id
end

module Access_options_without_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) Without_comparator.t
  let simplify_accessor = Fn.id
end

module Access_options_with_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) With_comparator.t
  let simplify_accessor f = f ~comparator:Core_int.comparator
end

let%test_module "Set" = (module Unit_tests (Elt_poly) (struct
  include Set
  type ('a, 'b) t_   = ('a, 'b) t
  type ('a, 'b) set  = ('a, 'b) t
  type ('a, 'b) tree = ('a, 'b) Tree.t
  type 'a cmp        = 'a
  include Create_options_with_comparator
  include Access_options_without_comparator
  let kind = `Set
end))

let%test_module "Set.Poly" = (module Unit_tests (Elt_poly) (struct
  include Set.Poly
  type ('a, 'b) set  = ('a, 'b) Set.t
  type ('a, 'b) t_   = 'a t
  type ('a, 'b) tree = 'a Tree.t
  type 'a cmp        = Comparator.Poly.comparator_witness
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Set
end))

let%test_module "Int.Set" = (module Unit_tests (Elt_int) (struct
  include Int.Set
  type ('a, 'b) set  = ('a, 'b) Set.t
  type ('a, 'b) t_   = t
  type ('a, 'b) tree = Tree.t
  type 'a cmp        = Int.comparator_witness
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Set
end))

let%test_module "Set.Tree" = (module Unit_tests (Elt_poly) (struct
  include Set.Tree
  type ('a, 'b) set  = ('a, 'b) Set.Tree.t
  type ('a, 'b) t_   = ('a, 'b) t
  type ('a, 'b) tree = ('a, 'b) t
  type 'a cmp        = 'a
  include Create_options_with_comparator
  include Access_options_with_comparator
  let kind = `Tree
end))

let%test_module "Set.Poly.Tree" = (module Unit_tests (Elt_poly) (struct
  include Set.Poly.Tree
  type ('a, 'b) set  = 'a Set.Poly.Tree.t
  type ('a, 'b) t_   = 'a t
  type ('a, 'b) tree = 'a t
  type 'a cmp        = Comparator.Poly.comparator_witness
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Tree
end))

let%test_module "Int.Set.Tree" = (module Unit_tests (Elt_int) (struct
  include Int.Set.Tree
  type ('a, 'b) set  = ('a, 'b) Set.Tree.t
  type ('a, 'b) t_   = t
  type ('a, 'b) tree = t
  type 'a cmp        = Int.comparator_witness
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Tree
end))
