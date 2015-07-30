open Std_internal

TEST_MODULE "random" = struct

  module G = Quickcheck.Generator
  module O = Quickcheck.Observer

  module type T = sig

    type t with sexp_of

    val t_gen : t G.t
    val t_obs : t O.t

    include Comparable.S with type t := t

    val module_name : string

  end

  module type Math = sig

    type t

    include T with type t := t
    include Commutative_group.S with type t := t

  end

  module Make (T : T) (Math : Math) = struct

    open T
    open G.Monad_infix

    module Q = Quickcheck.Configure (struct
        include Quickcheck
        let default_seed =
          `Deterministic
            (sprintf "%s values with %s operators."
               (String.capitalize T.module_name)
               (String.capitalize Math.module_name))
      end)
    ;;

    TEST_UNIT "mem true" =
      Q.test ~sexp_of:<:sexp_of< t * t list >>
        (G.list    t_gen >>= fun list        ->
         G.of_list list  >>| fun elt_of_list ->
         elt_of_list, list)
        ~f:(fun (elt_of_list, list) ->
          <:test_result< bool >>
            (List.mem list elt_of_list ~equal)
            ~expect:true)

    TEST_UNIT "mem false" =
      Q.test ~sexp_of:<:sexp_of< t * t list >>
        (G.bind_choice t_gen (fun choice ->
           let x = G.Choice.value choice in
           let not_x_gen =
             G.Choice.updated_gen choice
               ~keep:`All_choices_except_this_choice
           in
           G.list not_x_gen >>| fun list_of_not_x ->
           x, list_of_not_x))
        ~f:(fun (x, list_of_not_x) ->
          <:test_result< bool >>
            (List.mem list_of_not_x x ~equal)
            ~expect:false)

    TEST_UNIT "len" =
      Q.test ~sexp_of:<:sexp_of< int * t list >>
        (G.size                              >>= fun len  ->
         G.list t_gen ~length:(`Exactly len) >>| fun list ->
         len, list)
        ~f:(fun (len, list) ->
          <:test_result< int >>
            (List.length list)
            ~expect:len)

    TEST_UNIT "is_empty true" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.singleton [])
        ~f:(fun empty ->
          <:test_result< bool >>
            (List.is_empty empty)
            ~expect:true)

    TEST_UNIT "is_empty false" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.tuple2 t_gen (G.list t_gen) >>| fun (x,list) ->
         x::list)
        ~f:(fun non_empty ->
          <:test_result< bool >>
            (List.is_empty non_empty)
            ~expect:false)

    TEST_UNIT "iter" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          let q = Queue.create () in
          List.iter list ~f:(Queue.enqueue q);
          <:test_result< t list >>
            (Queue.to_list q)
            ~expect:list)

    TEST_UNIT "sum vs fold" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> Math.t) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs Math.t_gen))
        ~f:(fun (list, f) ->
          <:test_eq< Math.t >>
            (List.fold list ~init:Math.zero ~f:(fun m x ->
               Math.(+) m (f x)))
            (List.sum (module Math) list ~f))

    TEST_UNIT "for_all vs exists" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< bool >>
            (List.for_all list ~f)
            (not (List.exists list ~f:(Fn.non f))))

    TEST_UNIT "exists vs mem" =
      Q.test ~sexp_of:<:sexp_of< t * t list >>
        (G.tuple2 t_gen (G.list t_gen))
        ~f:(fun (x, list) ->
          <:test_eq< bool >>
            (List.exists list ~f:(fun y -> equal x y))
            (List.mem    list x ~equal))

    TEST_UNIT "exists vs find" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< bool >>
            (List.exists list ~f)
            (Option.is_some (List.find list ~f)))

    TEST_UNIT "count vs length/filter" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< int >>
            (List.count list ~f)
            (List.length (List.filter list ~f)))

    TEST_UNIT "find vs find_map" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< t option >>
            (List.find     list ~f)
            (List.find_map list ~f:(fun x -> if f x then Some x else None)))

    TEST_UNIT "to_list" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_result< t list >>
            (List.to_list list)
            ~expect:list)

    TEST_UNIT "to_array + Array.to_list" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_result< t list >>
            (Array.to_list (List.to_array list))
            ~expect:list)

    TEST_UNIT "max_elt vs min_elt" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t -> int) >>
        (G.tuple2 (G.list t_gen) (G.compare_fn t_obs))
        ~f:(fun (list, cmp) ->
          <:test_eq< t option >>
            (List.min_elt list ~cmp)
            (List.max_elt list ~cmp:(fun x y -> cmp y x)))

    TEST_UNIT "return" =
      Q.test ~sexp_of:<:sexp_of< t >>
        t_gen
        ~f:(fun x ->
          <:test_result< t list >>
            (List.return x)
            ~expect:[x])

    TEST_UNIT "map vs bind" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs t_gen))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.map list ~f)
            (List.bind list (fun x -> [ f x ])))

    TEST_UNIT "monad left identity" =
      Q.test ~sexp_of:<:sexp_of< t * (t -> t list) >>
        (G.tuple2 t_gen (G.fn t_obs (G.list t_gen)))
        ~f:(fun (x, f) ->
          <:test_eq< t list >>
            (List.bind (List.return x) f)
            (f x))

    TEST_UNIT "monad right identity" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_result< t list >>
            (List.bind list List.return)
            ~expect:list)

    TEST_UNIT "monad associativity" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t list) * (t -> t list) >>
        (G.tuple3
           (G.list t_gen)
           (G.fn t_obs (G.list t_gen))
           (G.fn t_obs (G.list t_gen)))
        ~f:(fun (list, f, g) ->
          <:test_eq< t list >>
            (List.bind (List.bind list f) g)
            (List.bind list (fun x -> List.bind (f x) g)))

    TEST_UNIT "join" =
      Q.test ~sexp_of:<:sexp_of< t list list >>
        (G.list (G.list t_gen))
        ~f:(fun list ->
          <:test_eq< t list >>
            (List.join list)
            (List.bind list Fn.id))

    TEST_UNIT "ignore" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_eq< unit list >>
            (List.ignore_m list)
            (List.map list ~f:ignore))

    TEST_UNIT "of_list + to_list" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_result< t list >>
            (List.of_list list)
            ~expect:list)

    TEST_UNIT "nth vs nth_exn" =
      Q.test ~sexp_of:<:sexp_of< int * t list >>
        (G.tuple2 G.size (G.list t_gen))
        ~f:(fun (i, list) ->
          <:test_eq< t option >>
            (List.nth list i)
            (Option.try_with (fun () ->
               List.nth_exn list i)))

    TEST_UNIT "init + nth_exn" =
      Q.test ~sexp_of:<:sexp_of< int * int * (int -> t) >>
        (G.size >>= fun size ->
         G.tuple3
           (G.return size)
           (G.int_between ~lower_bound:(Incl 0) ~upper_bound:(Excl size))
           (G.fn O.int t_gen))
        ~f:(fun (size, i, f) ->
          <:test_result< t >>
            (List.nth_exn (List.init size ~f) i)
            ~expect:(f i))

    TEST_UNIT "rev^2" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_result< t list >>
            (List.rev (List.rev list))
            ~expect:list)

    TEST_UNIT "rev_append vs rev + append" =
      Q.test ~sexp_of:<:sexp_of< t list * t list >>
        (G.tuple2 (G.list t_gen) (G.list t_gen))
        ~f:(fun (list1, list2) ->
          <:test_eq< t list >>
            (List.rev_append list1 list2)
            (List.append (List.rev list1) list2))

    TEST_UNIT "unordered_append vs append" =
      Q.test ~sexp_of:<:sexp_of< t list * t list * t >>
        (G.tuple3 (G.list t_gen) (G.list t_gen) t_gen)
        ~f:(fun (list1, list2, x) ->
          <:test_eq< bool >>
            (List.mem (List.append list1 list2) x ~equal)
            (List.mem (List.unordered_append list1 list2) x ~equal))

    TEST_UNIT "rev_map vs map + rev" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs t_gen))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.rev_map list ~f)
            (List.rev (List.map list ~f)))

    TEST_UNIT "fold vs fold_left" =
      Q.test ~sexp_of:<:sexp_of< t list * t * (t -> t -> t) >>
        (G.tuple3 (G.list t_gen) t_gen (G.fn2 t_obs t_obs t_gen))
        ~f:(fun (list, init, f) ->
          <:test_eq< t >>
            (List.fold list ~init ~f)
            (List.fold_left list ~init ~f))

    TEST_UNIT "unzip + iter2_exn vs iter" =
      Q.test ~sexp_of:<:sexp_of< (t * t) list * (t -> t -> t) >>
        (G.tuple2 (G.list (G.tuple2 t_gen t_gen)) (G.fn2 t_obs t_obs t_gen))
        ~f:(fun (pair_list, f) ->
          <:test_eq< t list >>
            (let q = Queue.create () in
             let list1, list2 = List.unzip pair_list in
             List.iter2_exn list1 list2 ~f:(fun x y ->
               Queue.enqueue q (f x y));
             Queue.to_list q)
            (let q = Queue.create () in
             List.iter pair_list ~f:(fun (x,y) ->
               Queue.enqueue q (f x y));
             Queue.to_list q))

    TEST_UNIT "rev_map2_exn vs rev + map2_exn" =
      Q.test ~sexp_of:<:sexp_of< (t * t) list * (t -> t -> t) >>
        (G.tuple2 (G.list (G.tuple2 t_gen t_gen)) (G.fn2 t_obs t_obs t_gen))
        ~f:(fun (pair_list, f) ->
          let list1, list2 = List.unzip pair_list in
          <:test_eq< t list >>
            (List.rev_map2_exn list1 list2 ~f)
            (List.rev (List.map2_exn list1 list2 ~f)))

    TEST_UNIT "unzip + fold2_exn + fold" =
      Q.test ~sexp_of:<:sexp_of< (t * t) list * t * (t -> t -> t -> t) >>
        (G.tuple3 (G.list (G.tuple2 t_gen t_gen)) t_gen (G.fn3 t_obs t_obs t_obs t_gen))
        ~f:(fun (pair_list, init, f) ->
          let list1, list2 = List.unzip pair_list in
          <:test_eq< t >>
            (List.fold2_exn list1 list2 ~init ~f)
            (List.fold (List.zip_exn list1 list2) ~init ~f:(fun acc (x,y) ->
               f acc x y)))

    TEST_UNIT "unzip + for_all2_exn vs for_all" =
      Q.test ~sexp_of:<:sexp_of< (t * t) list * (t -> t -> bool) >>
        (G.tuple2 (G.list (G.tuple2 t_gen t_gen)) (G.fn2 t_obs t_obs G.bool))
        ~f:(fun (pair_list, f) ->
          <:test_eq< bool >>
            (let list1, list2 = List.unzip pair_list in
             List.for_all2_exn list1 list2 ~f)
            (List.for_all pair_list ~f:(fun (x,y) -> f x y)))

    TEST_UNIT "unzip + exists2_exn vs exists" =
      Q.test ~sexp_of:<:sexp_of< (t * t) list * (t -> t -> bool) >>
        (G.tuple2 (G.list (G.tuple2 t_gen t_gen)) (G.fn2 t_obs t_obs G.bool))
        ~f:(fun (pair_list, f) ->
          <:test_eq< bool >>
            (let list1, list2 = List.unzip pair_list in
             List.exists2_exn list1 list2 ~f)
            (List.exists pair_list ~f:(fun (x,y) -> f x y)))

    TEST_UNIT "filter vs for_all" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_result< bool >>
            (List.for_all ~f (List.filter ~f list))
            ~expect:true)

    TEST_UNIT "filter true" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_result< t list >>
            (List.filter list ~f:(const true))
            ~expect:list)

    TEST_UNIT "filter vs rev_filter" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.rev (List.filter list ~f))
            (List.rev_filter list ~f))

    TEST_UNIT "filteri vs filter" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.filter list ~f)
            (List.filteri list ~f:(fun _ x -> f x)))

    TEST_UNIT "partition_map vs filter_map" =
      let partition_of_variant = function
        | `A a -> `Fst a
        | `B b -> `Snd b
      in
      Q.test ~sexp_of:<:sexp_of< t list * (t -> [ `Fst of t | `Snd of t ]) >>
        (G.tuple2 (G.list t_gen)
           (G.fn t_obs (G.variant2 t_gen t_gen >>| partition_of_variant)))
        ~f:(fun (list, f) ->
          <:test_eq< t list * t list >>
            (List.partition_map list ~f)
            (List.filter_map list ~f:(fun x ->
               match f x with
               | `Fst x -> Some x
               | `Snd _ -> None),
             List.filter_map list ~f:(fun x ->
               match f x with
               | `Fst _ -> None
               | `Snd x -> Some x)))

    TEST_UNIT "partition_tf vs partition_map" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< t list * t list >>
            (List.partition_tf list ~f)
            (List.partition_map list ~f:(fun x ->
               if f x then `Fst x else `Snd x)))

    TEST_UNIT "append + split_n" =
      Q.test ~sexp_of:<:sexp_of< t list * t list >>
        (G.tuple2 (G.list t_gen) (G.list t_gen))
        ~f:(fun (list1, list2) ->
          <:test_result< t list * t list >>
            (List.split_n (List.append list1 list2) (List.length list1))
            ~expect:(list1, list2))

    TEST_UNIT "sort vs stable_sort" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t -> int) >>
        (G.tuple2 (G.list t_gen) (G.compare_fn t_obs))
        ~f:(fun (list, cmp) ->
          (* When comparing [t] using [cmp], [sort] and [stable_sort] should be
             indistinguishable. *)
          let compare = cmp in
          <:test_eq< t list >>
            (List.sort        list ~cmp)
            (List.stable_sort list ~cmp))

    TEST_UNIT "stable_sort + merge vs append + stable_sort" =
      Q.test ~sexp_of:<:sexp_of< t list * t list * (t -> t -> int) >>
        (G.tuple3 (G.list t_gen) (G.list t_gen) (G.compare_fn t_obs))
        ~f:(fun (list1, list2, cmp) ->
          <:test_eq< t list >>
            (List.merge ~cmp
               (List.stable_sort ~cmp list1)
               (List.stable_sort ~cmp list2))
            (List.stable_sort ~cmp
               (List.append list1 list2)))

    TEST_UNIT "hd vs hd_exn" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_eq< t option >>
            (List.hd list)
            (Option.try_with (fun () -> List.hd_exn list)))

    TEST_UNIT "tl vs tl_exn" =
      Q.test ~sexp_of:<:sexp_of< t list >>
        (G.list t_gen)
        ~f:(fun list ->
          <:test_eq< t list option >>
            (List.tl list)
            (Option.try_with (fun () -> List.tl_exn list)))

    TEST_UNIT "find vs find_exn" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< t option >>
            (List.find list ~f)
            (Option.try_with (fun () -> List.find_exn list ~f)))

    TEST_UNIT "find vs findi" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> bool) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs G.bool))
        ~f:(fun (list, f) ->
          <:test_eq< t option >>
            (List.find list ~f)
            (Option.map ~f:snd (List.findi list ~f:(fun _ x -> f x))))

    TEST_UNIT "append + rev" =
      Q.test ~sexp_of:<:sexp_of< t list * t list >>
        (G.tuple2 (G.list t_gen) (G.list t_gen))
        ~f:(fun (list1, list2) ->
          <:test_eq< t list >>
            (List.rev (List.append list1 list2))
            (List.append (List.rev list2) (List.rev list1)))

    TEST_UNIT "append associativity" =
      Q.test ~sexp_of:<:sexp_of< t list * t list * t list >>
        (G.tuple3 (G.list t_gen) (G.list t_gen) (G.list t_gen))
        ~f:(fun (list1, list2, list3) ->
          <:test_eq< t list >>
            (List.append list1 (List.append list2 list3))
            (List.append (List.append list1 list2) list3))

    TEST_UNIT "map + rev vs rev + map" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs t_gen))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.rev (List.map list ~f))
            (List.map (List.rev list) ~f))

    TEST_UNIT "map + append vs append + map" =
      Q.test ~sexp_of:<:sexp_of< t list * t list * (t -> t) >>
        (G.tuple3 (G.list t_gen) (G.list t_gen) (G.fn t_obs t_gen))
        ~f:(fun (list1, list2, f) ->
          <:test_eq< t list >>
            (List.append (List.map list1 ~f) (List.map list2 ~f))
            (List.map (List.append list1 list2) ~f))

    TEST_UNIT "map vs concat_map" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs t_gen))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.map list ~f)
            (List.concat_map list ~f:(fun x -> [ f x ])))

    TEST_UNIT "concat_mapi vs concat_map" =
      Q.test ~sexp_of:<:sexp_of< t list * (t -> t list) >>
        (G.tuple2 (G.list t_gen) (G.fn t_obs (G.list t_gen)))
        ~f:(fun (list, f) ->
          <:test_eq< t list >>
            (List.concat_map list ~f)
            (List.concat_mapi list ~f:(fun _ x -> f x)))

    TEST_UNIT "unzip + map2_exn vs map" =
      Q.test ~sexp_of:<:sexp_of< (t * t) list * (t -> t -> t) >>
        (G.tuple2 (G.list (G.tuple2 t_gen t_gen)) (G.fn2 t_obs t_obs t_gen))
        ~f:(fun (pair_list, f) ->
          <:test_eq< t list >>
            (let list1, list2 = List.unzip pair_list in
             List.map2_exn list1 list2 ~f)
            (List.map pair_list ~f:(fun (x,y) -> f x y)))

    TEST_UNIT "unzip + map3_exn vs map" =
      Q.test ~sexp_of:<:sexp_of< (t * (t * t)) list * (t -> t -> t -> t) >>
        (G.tuple2
           (G.list (G.tuple2 t_gen (G.tuple2 t_gen t_gen)))
           (G.fn3 t_obs t_obs t_obs t_gen))
        ~f:(fun (triple_list, f) ->
          <:test_eq< t list >>
            (let list1, pair_list = List.unzip triple_list in
             let list2, list3     = List.unzip pair_list   in
             List.map3_exn list1 list2 list3 ~f)
            (List.map triple_list ~f:(fun (x,(y,z)) -> f x y z)))

    TEST_UNIT "rev + map3_exn vs rev_map3_exn" =
      Q.test ~sexp_of:<:sexp_of< (t * (t * t)) list * (t -> t -> t -> t) >>
        (G.tuple2
           (G.list (G.tuple2 t_gen (G.tuple2 t_gen t_gen)))
           (G.fn3 t_obs t_obs t_obs t_gen))
        ~f:(fun (triple_list, f) ->
          let list1, pair_list = List.unzip triple_list in
          let list2, list3     = List.unzip pair_list   in
          <:test_eq< t list >>
            (List.rev_map3_exn list1 list2 list3 ~f)
            (List.rev (List.map3_exn list1 list2 list3 ~f)))

  end

  (* Float with bitwise comparison. *)
  module Float_ = struct

    include Float

    let compare x y =
      Int64.compare
        (Int64.bits_of_float x)
        (Int64.bits_of_float y)

    let equal x y =
      Int64.equal
        (Int64.bits_of_float x)
        (Int64.bits_of_float y)

  end

  TEST = Float_.equal Float.nan Float.nan

  module Unit'   = struct let t_gen = G.unit   let t_obs = O.unit   include Unit   let module_name = "Unit"   end
  module Bool'   = struct let t_gen = G.bool   let t_obs = O.bool   include Bool   let module_name = "Bool"   end
  module Int'    = struct let t_gen = G.int    let t_obs = O.int    include Int    let module_name = "Int"    end
  module Float'  = struct let t_gen = G.float  let t_obs = O.float  include Float_ let module_name = "Float"  end
  module String' = struct let t_gen = G.string let t_obs = O.string include String let module_name = "String" end
  module Char'   = struct let t_gen = G.char   let t_obs = O.char   include Char   let module_name = "Char"   end
  module Sexp'   = struct let t_gen = G.sexp   let t_obs = O.sexp   include Sexp   let module_name = "Sexp"   end

  TEST_MODULE "unit w/ int"   = Make (Unit')   (Int')
  TEST_MODULE "bool w/ float" = Make (Bool')   (Float')
  TEST_MODULE "string w/ int" = Make (String') (Int')
  TEST_MODULE "char w/ float" = Make (Char')   (Float')
  TEST_MODULE "sexp w/ int"   = Make (Sexp')   (Int')

end
