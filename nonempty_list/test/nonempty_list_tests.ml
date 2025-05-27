open Core
open Composition_infix
open Nonempty_list

include (
  Base_test_helpers.Test_container.Test_S1_allow_skipping_tests (struct
    include Nonempty_list

    let of_list l = if List.is_empty l then `Skip_test else `Ok (of_list_exn l)
  end) :
  sig end)

(* Note that at least one expect test must be included for the above functor's tests to
   run.
*)

let t : _ Nonempty_list.t = [ 1; 2; 3; 4 ]

let quickcheck'
  gen
  (test_result :
    ?here:lexing_position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> expect:'a
    -> 'a
    -> unit)
  f_list
  f
  =
  Quickcheck.test gen ~f:(fun v -> test_result ~expect:(f_list v) (f v))
;;

let quickcheck
  (test_result :
    ?here:lexing_position list
    -> ?message:string
    -> ?equal:('a -> 'a -> bool)
    -> expect:'a
    -> 'a
    -> unit)
  f_list
  f
  =
  quickcheck' [%quickcheck.generator: int t] test_result (fun l -> f_list (to_list l)) f
;;

let%expect_test "sexp representations" =
  print_s [%sexp (t : int Nonempty_list.t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (t : int Nonempty_list.Stable.V2.t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (t : int Nonempty_list.Stable.V1.t)];
  [%expect {| (1 2 3 4) |}]
;;

let%expect_test "reduce" =
  quickcheck
    [%test_result: int]
    (fun l -> List.reduce l ~f:( + ) |> Core.Option.value_exn)
    (fun l -> reduce l ~f:( + ))
;;

let%expect_test "reverse" =
  quickcheck
    [%test_result: int list]
    (fun l -> List.rev l)
    (fun l -> reverse l |> to_list)
;;

let%expect_test "append" =
  quickcheck
    [%test_result: int list]
    (fun l -> List.append (to_list t) l)
    (fun l -> append t (to_list l) |> to_list)
;;

let%expect_test "append'" =
  print_s [%sexp (append' [] t : int t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (append' [ 5; 6; 7 ] t : int t)];
  [%expect {| (5 6 7 1 2 3 4) |}]
;;

let%expect_test "@" =
  quickcheck'
    [%quickcheck.generator: int t * int t]
    [%test_result: int list]
    (fun (t1, t2) -> to_list (t1 @ t2))
    (fun (t1, t2) -> List.Infix.( @ ) (to_list t1) (to_list t2))
;;

let%expect_test "unzip" =
  quickcheck'
    [%quickcheck.generator: (int * char) t]
    [%test_result: int list * char list]
    (fun l -> List.unzip (to_list l))
    (fun l -> unzip l |> Tuple2.map_both ~f1:to_list ~f2:to_list)
;;

let%expect_test "return" =
  print_s [%sexp (return 1 : int t)];
  [%expect {| (1) |}]
;;

let%expect_test "bind" =
  let t = [ 1; 2; 3 ] >>= (List.init ~f:Fn.id >> of_list_exn) in
  print_s [%sexp (t : int t)];
  [%expect {| (0 0 1 0 1 2) |}]
;;

let%expect_test "map2" =
  let test a b = print_s [%sexp (map2 a b ~f:( * ) : int t List.Or_unequal_lengths.t)] in
  test [ 1 ] [ 2 ];
  test [ 1; 2 ] [ 3; 4 ];
  test [ 1 ] [ 2; 3 ];
  test [ 1; 2 ] [ 3 ];
  [%expect
    {|
    (Ok (2))
    (Ok (3 8))
    Unequal_lengths
    Unequal_lengths
    |}]
;;

let%expect_test "map2_exn" =
  let test a b =
    print_s
      [%sexp (Or_error.try_with (fun () -> map2_exn a b ~f:( * )) : int t Or_error.t)]
  in
  test [ 1 ] [ 2 ];
  test [ 1; 2 ] [ 3; 4 ];
  test [ 1 ] [ 2; 3 ];
  test [ 1; 2 ] [ 3 ];
  [%expect
    {|
    (Ok (2))
    (Ok (3 8))
    (Error (Invalid_argument "length mismatch in map2_exn: 1 <> 2"))
    (Error (Invalid_argument "length mismatch in map2_exn: 2 <> 1"))
    |}]
;;

let%expect_test "zip" =
  let test a b = print_s [%sexp (zip a b : (int * int) t List.Or_unequal_lengths.t)] in
  test [ 1 ] [ 2 ];
  test [ 1; 2 ] [ 3; 4 ];
  test [ 1 ] [ 2; 3 ];
  test [ 1; 2 ] [ 3 ];
  [%expect
    {|
    (Ok ((1 2)))
    (Ok ((1 3) (2 4)))
    Unequal_lengths
    Unequal_lengths
    |}]
;;

let%expect_test "zip_exn" =
  let test a b =
    print_s [%sexp (Or_error.try_with (fun () -> zip_exn a b) : (int * int) t Or_error.t)]
  in
  test [ 1 ] [ 2 ];
  test [ 1; 2 ] [ 3; 4 ];
  test [ 1 ] [ 2; 3 ];
  test [ 1; 2 ] [ 3 ];
  [%expect
    {|
    (Ok ((1 2)))
    (Ok ((1 3) (2 4)))
    (Error (Invalid_argument "length mismatch in zip_exn: 1 <> 2"))
    (Error (Invalid_argument "length mismatch in zip_exn: 2 <> 1"))
    |}]
;;

let%expect_test "filter" =
  let test a ~f = print_s [%sexp (filter a ~f : int list)] in
  let is_even x = x % 2 = 0 in
  test [ 1 ] ~f:is_even;
  test [ 2 ] ~f:is_even;
  test [ 1; 2 ] ~f:is_even;
  test [ 2; 2; 3; 4 ] ~f:is_even;
  [%expect
    {|
    ()
    (2)
    (2)
    (2 2 4)
    |}]
;;

let%expect_test "filteri" =
  let test a ~f = print_s [%sexp (filteri a ~f : int list)] in
  let index_plus_value_is_even i x = (i + x) % 2 = 0 in
  test [ 1 ] ~f:index_plus_value_is_even;
  test [ 2 ] ~f:index_plus_value_is_even;
  test [ 1; 2 ] ~f:index_plus_value_is_even;
  test [ 2; 2; 3; 4 ] ~f:index_plus_value_is_even;
  test [ 2; 2; 4; 3 ] ~f:index_plus_value_is_even;
  [%expect
    {|
    ()
    (2)
    ()
    (2)
    (2 4 3)
    |}]
;;

let%expect_test "filter_map" =
  let test a ~f = print_s [%sexp (filter_map a ~f : int list)] in
  let double_if_even x = if x % 2 = 0 then Some (x * 2) else None in
  test [ 1 ] ~f:double_if_even;
  test [ 2 ] ~f:double_if_even;
  test [ 1; 2 ] ~f:double_if_even;
  test [ 2; 2; 3; 4 ] ~f:double_if_even;
  [%expect
    {|
    ()
    (4)
    (4)
    (4 4 8)
    |}]
;;

let%expect_test "filter_mapi" =
  let test a ~f = print_s [%sexp (filter_mapi a ~f : int list)] in
  let double_if_index_plus_value_is_even i x =
    if (i + x) % 2 = 0 then Some (x * 2) else None
  in
  test [ 1 ] ~f:double_if_index_plus_value_is_even;
  test [ 2 ] ~f:double_if_index_plus_value_is_even;
  test [ 1; 2 ] ~f:double_if_index_plus_value_is_even;
  test [ 2; 2; 3; 4 ] ~f:double_if_index_plus_value_is_even;
  test [ 2; 2; 4; 3 ] ~f:double_if_index_plus_value_is_even;
  [%expect
    {|
    ()
    (4)
    ()
    (4)
    (4 8 6)
    |}]
;;

let%expect_test "concat" =
  let test lists = print_s [%sexp (concat lists : int t)] in
  test [ [ 1 ] ];
  test [ [ 1 ]; [ 2 ]; [ 3 ] ];
  test [ [ 1; 2; 3 ] ];
  test [ [ 1 ]; [ 2; 3 ]; [ 4; 5; 6 ] ];
  [%expect
    {|
    (1)
    (1 2 3)
    (1 2 3)
    (1 2 3 4 5 6)
    |}]
;;

let%expect_test "nth" =
  let test n list = print_s [%sexp (nth list n : int option)] in
  test (-1) [ 1 ];
  test 0 [ 1; 2; 3 ];
  test 2 [ 1; 2; 3 ];
  test 3 [ 1; 2; 3 ];
  [%expect
    {|
    ()
    (1)
    (3)
    ()
    |}]
;;

let%expect_test "nth_exn" =
  let test n list =
    print_s [%sexp (Or_error.try_with (fun () -> nth_exn list n) : int Or_error.t)]
  in
  test (-1) [ 1 ];
  test 0 [ 1; 2; 3 ];
  test 2 [ 1; 2; 3 ];
  test 3 [ 1; 2; 3 ];
  [%expect
    {|
    (Error
     (Invalid_argument "Nonempty_list.nth_exn -1 called on list of length 1"))
    (Ok 1)
    (Ok 3)
    (Error
     (Invalid_argument "Nonempty_list.nth_exn 3 called on list of length 3"))
    |}]
;;

let%expect_test "last" =
  quickcheck
    [%test_result: int]
    (fun l -> List.last l |> Core.Option.value_exn)
    (fun l -> last l)
;;

let%expect_test "drop_last" =
  quickcheck
    [%test_result: int list]
    (fun l -> List.drop_last l |> Core.Option.value_exn)
    (fun l -> drop_last l)
;;

let%expect_test "to_sequence" =
  let test t = print_s [%sexp (to_sequence t : int Sequence.t)] in
  test [ 1 ];
  test [ 1; 2; 3 ];
  test [ 0; 2; 4; 6 ];
  [%expect
    {|
    (1)
    (1 2 3)
    (0 2 4 6)
    |}]
;;

let%expect_test "sort" =
  let test t = print_s [%sexp (sort ~compare:Int.compare t : int t)] in
  test [ 1 ];
  test [ 2; 4; 1; 4 ];
  [%expect
    {|
    (1)
    (1 2 4 4)
    |}]
;;

let%expect_test "stable_sort" =
  let test t =
    print_s
      [%sexp
        (stable_sort ~compare:(fun a b -> Comparable.lift ~f:fst Int.compare a b) t
         : (int * string) t)]
  in
  test [ 1, "_" ];
  test [ 2, "_"; 4, "a"; 1, "_"; 4, "b" ];
  test [ 2, "_"; 4, "b"; 1, "_"; 4, "a" ];
  [%expect
    {|
    ((1 _))
    ((1 _) (2 _) (4 a) (4 b))
    ((1 _) (2 _) (4 b) (4 a))
    |}]
;;

let%expect_test "stable_dedup" =
  let test t =
    print_s
      [%sexp
        (stable_dedup ~compare:(fun a b -> Comparable.lift ~f:fst Int.compare a b) t
         : (int * string) t)]
  in
  test [ 1, "_" ];
  test [ 2, "_"; 4, "a"; 1, "_"; 4, "b" ];
  test [ 2, "_"; 4, "b"; 1, "_"; 4, "a" ];
  [%expect
    {|
    ((1 _))
    ((2 _) (4 a) (1 _))
    ((2 _) (4 b) (1 _))
    |}]
;;

let%expect_test "dedup_and_sort" =
  let test t = print_s [%sexp (dedup_and_sort ~compare:Int.compare t : int t)] in
  test [ 1 ];
  test [ 2; 4; 1; 4 ];
  [%expect
    {|
    (1)
    (1 2 4)
    |}]
;;

let%expect_test "sort_and_group" =
  let test t = print_s [%sexp (sort_and_group ~compare:Int.compare t : int t t)] in
  test [ 1 ];
  test [ 2; 4; 1; 4 ];
  [%expect
    {|
    ((1))
    ((1) (2) (4 4))
    |}]
;;

let%expect_test "group" =
  quickcheck'
    [%quickcheck.generator: int t * (int -> int -> bool)]
    [%test_result: int list list]
    (fun (t, break) -> group t ~break |> to_list |> List.map ~f:to_list)
    (fun (t, break) -> List.group (to_list t) ~break)
;;

let%expect_test "all_equal" =
  let equal = [%equal: int] in
  quickcheck
    [%test_result: int option]
    (fun l -> List.all_equal ~equal l)
    (fun l -> all_equal ~equal l)
;;

let%expect_test "min_elt' max_elt'" =
  let compare = Int.compare in
  let l = [ 2; 3; 1; 4 ] in
  print_s [%sexp (min_elt' ~compare l : int)];
  [%expect {| 1 |}];
  print_s [%sexp (max_elt' ~compare l : int)];
  [%expect {| 4 |}]
;;

let%expect_test "map_add_multi" =
  let map = map_of_alist_multi_rev [ 0, 0; 0, 1; 1, 1 ] ~comparator:(module Int) in
  print_s [%sexp (map : int t Int.Map.t)];
  [%expect {| ((0 (1 0)) (1 (1))) |}];
  print_s [%sexp (map_add_multi map ~key:1 ~data:0 : int t Int.Map.t)];
  [%expect {| ((0 (1 0)) (1 (0 1))) |}]
;;

let%expect_test "hashtbl_add_multi" =
  let hashtbl = Hashtbl.create (module Int) in
  let print () = print_s [%sexp (hashtbl : int Nonempty_list.t Hashtbl.M(Int).t)] in
  print ();
  [%expect {| () |}];
  hashtbl_add_multi hashtbl ~key:0 ~data:0;
  (* adding to an key that doesn't exist should create the key with a singleton nonempty
     list *)
  print ();
  [%expect {| ((0 (0))) |}];
  hashtbl_add_multi hashtbl ~key:0 ~data:1;
  (* adding to an key that already exists should cons to the existing nonempty list *)
  print ();
  [%expect {| ((0 (1 0))) |}]
;;

let%expect_test "map_of_alist_multi_rev" =
  let test alist =
    print_s
      [%sexp (map_of_alist_multi_rev alist ~comparator:(module Int) : int t Int.Map.t)]
  in
  test [];
  test [ 0, 0; 0, 1; 1, 1 ];
  [%expect
    {|
    ()
    ((0 (1 0)) (1 (1)))
    |}]
;;

let%expect_test "map_of_alist_multi" =
  let test alist =
    print_s [%sexp (map_of_alist_multi alist ~comparator:(module Int) : int t Int.Map.t)];
    (* Also print the result of using [Map.of_alist_multi] to demonstrate that they're the
       same *)
    print_s [%sexp (Map.of_alist_multi (module Int) alist : int list Int.Map.t)]
  in
  test [];
  test [ 0, 0; 0, 1; 1, 1 ];
  [%expect
    {|
    ()
    ()
    ((0 (0 1)) (1 (1)))
    ((0 (0 1)) (1 (1)))
    |}]
;;

let%expect_test "map_of_list_with_key_multi_rev" =
  let get_key = Date.year in
  let test alist =
    print_s
      [%sexp
        (map_of_list_with_key_multi_rev alist ~comparator:(module Int) ~get_key
         : Date.t t Int.Map.t)]
  in
  test [];
  test ([ "2023-01-01"; "2023-03-03"; "2024-12-24" ] |> List.map ~f:Date.of_string);
  [%expect
    {|
    ()
    ((2023 (2023-03-03 2023-01-01)) (2024 (2024-12-24)))
    |}]
;;

let%expect_test "map_of_list_with_key_multi" =
  let get_key = Date.year in
  let test alist =
    print_s
      [%sexp
        (map_of_list_with_key_multi alist ~comparator:(module Int) ~get_key
         : Date.t t Int.Map.t)];
    (* Also print the result of using [Map.of_list_with_key_multi] to demonstrate that
       they're the same *)
    print_s
      [%sexp
        (Map.of_list_with_key_multi (module Int) alist ~get_key : Date.t list Int.Map.t)]
  in
  test [];
  test ([ "2023-01-01"; "2023-03-03"; "2024-12-24" ] |> List.map ~f:Date.of_string);
  [%expect
    {|
    ()
    ()
    ((2023 (2023-01-01 2023-03-03)) (2024 (2024-12-24)))
    ((2023 (2023-01-01 2023-03-03)) (2024 (2024-12-24)))
    |}]
;;

let%expect_test "map_of_sequence_multi_rev" =
  let test alist =
    print_s
      [%sexp (map_of_sequence_multi_rev alist ~comparator:(module Int) : int t Int.Map.t)]
  in
  test Sequence.empty;
  test (Sequence.of_list [ 0, 0; 0, 1; 1, 1 ]);
  [%expect
    {|
    ()
    ((0 (1 0)) (1 (1)))
    |}]
;;

let%expect_test "map_of_sequence_multi" =
  let test alist =
    print_s
      [%sexp (map_of_sequence_multi alist ~comparator:(module Int) : int t Int.Map.t)];
    (* Also print the result of using [Map.of_sequence_multi] to demonstrate that they're the
       same *)
    print_s [%sexp (Map.of_sequence_multi (module Int) alist : int list Int.Map.t)]
  in
  test Sequence.empty;
  test (Sequence.of_list [ 0, 0; 0, 1; 1, 1 ]);
  [%expect
    {|
    ()
    ()
    ((0 (0 1)) (1 (1)))
    ((0 (0 1)) (1 (1)))
    |}]
;;

let%expect_test "combine_errors" =
  let test rs = print_s [%sexp (combine_errors rs : (int t, int t) Result.t)] in
  test [ Ok 1 ];
  [%expect {| (Ok (1)) |}];
  test [ Error 1 ];
  [%expect {| (Error (1)) |}];
  test [ Ok 1; Error 2 ];
  [%expect {| (Error (2)) |}];
  test [ Error 1; Ok 2 ];
  [%expect {| (Error (1)) |}];
  test [ Ok 1; Ok 2; Ok 3; Ok 4 ];
  [%expect {| (Ok (1 2 3 4)) |}];
  test [ Error 1; Error 2; Error 3; Error 4 ];
  [%expect {| (Error (1 2 3 4)) |}];
  test [ Ok 1; Error 2; Error 3; Error 4 ];
  [%expect {| (Error (2 3 4)) |}]
;;

let%expect_test "combine_errors_unit" =
  let test rs = print_s [%sexp (combine_errors_unit rs : (unit, int t) Result.t)] in
  test [ Ok () ];
  [%expect {| (Ok ()) |}];
  test [ Error 1 ];
  [%expect {| (Error (1)) |}];
  test [ Ok (); Error 2 ];
  [%expect {| (Error (2)) |}];
  test [ Error 1; Ok () ];
  [%expect {| (Error (1)) |}];
  test [ Ok (); Ok (); Ok (); Ok () ];
  [%expect {| (Ok ()) |}];
  test [ Error 1; Error 2; Error 3; Error 4 ];
  [%expect {| (Error (1 2 3 4)) |}];
  test [ Ok (); Error 2; Error 3; Error 4 ];
  [%expect {| (Error (2 3 4)) |}]
;;

let%expect_test "combine_or_errors" =
  let e s = Or_error.error_string s in
  let test oes = print_s [%sexp (combine_or_errors oes : int t Or_error.t)] in
  test [ Ok 1 ];
  [%expect {| (Ok (1)) |}];
  test [ e "A" ];
  [%expect {| (Error A) |}];
  test [ Ok 1; e "B" ];
  [%expect {| (Error B) |}];
  test [ e "A"; Ok 2 ];
  [%expect {| (Error A) |}];
  test [ Ok 1; Ok 2; Ok 3; Ok 4 ];
  [%expect {| (Ok (1 2 3 4)) |}];
  test [ e "A"; e "B"; e "C"; e "D" ];
  [%expect {| (Error (A B C D)) |}];
  test [ Ok 1; e "B"; e "C"; e "D" ];
  [%expect {| (Error (B C D)) |}]
;;

let%expect_test "combine_or_errors_unit" =
  let e s = Or_error.error_string s in
  let test oes = print_s [%sexp (combine_or_errors_unit oes : unit Or_error.t)] in
  test [ Ok () ];
  [%expect {| (Ok ()) |}];
  test [ e "A" ];
  [%expect {| (Error A) |}];
  test [ Ok (); e "B" ];
  [%expect {| (Error B) |}];
  test [ e "A"; Ok () ];
  [%expect {| (Error A) |}];
  test [ Ok (); Ok (); Ok (); Ok () ];
  [%expect {| (Ok ()) |}];
  test [ e "A"; e "B"; e "C"; e "D" ];
  [%expect {| (Error (A B C D)) |}];
  test [ Ok (); e "B"; e "C"; e "D" ];
  [%expect {| (Error (B C D)) |}]
;;

let%expect_test "filter_ok_at_least_one" =
  let e s = Or_error.error_string s in
  let test oes = print_s [%sexp (filter_ok_at_least_one oes : int t Or_error.t)] in
  test [ Ok 1 ];
  [%expect {| (Ok (1)) |}];
  test [ e "A" ];
  [%expect {| (Error A) |}];
  test [ Ok 1; e "B" ];
  [%expect {| (Ok (1)) |}];
  test [ e "A"; Ok 2 ];
  [%expect {| (Ok (2)) |}];
  test [ Ok 1; Ok 2; Ok 3; Ok 4 ];
  [%expect {| (Ok (1 2 3 4)) |}];
  test [ e "A"; e "B"; e "C"; e "D" ];
  [%expect {| (Error (A B C D)) |}];
  test [ Ok 1; e "B"; e "C"; e "D" ];
  [%expect {| (Ok (1)) |}];
  test [ e "A"; e "B"; Ok 3; e "D"; Ok 5 ];
  [%expect {| (Ok (3 5)) |}]
;;

let%expect_test "option_all" =
  let test os = print_s [%sexp (option_all os : int t option)] in
  test [ Some 1 ];
  [%expect {| ((1)) |}];
  test [ None ];
  [%expect {| () |}];
  test [ Some 1; None ];
  [%expect {| () |}];
  test [ Some 1; Some 2 ];
  [%expect {| ((1 2)) |}];
  test [ None; None; Some 1 ];
  [%expect {| () |}];
  test [ None; None ];
  [%expect {| () |}];
  test [ Some 1; Some 2; Some 4 ];
  [%expect {| ((1 2 4)) |}]
;;

let%expect_test "basic accessor" =
  Accessor.iteri Nonempty_list_accessor.eachi t ~f:(fun [ i ] x ->
    printf "index: %d, value %d\n" i x);
  [%expect
    {|
    index: 0, value 1
    index: 1, value 2
    index: 2, value 3
    index: 3, value 4
    |}]
;;

(* Demonstrate how accessors allow types to be easily commuted: Here a ['a Or_error.t
   Nonempty_list.t] becomes a ['a Nonempty_list Or_error.t]. *)
let%expect_test "commute types" =
  let commute results = Accessor_base.Or_error.all Nonempty_list_accessor.each results in
  print_s [%sexp (commute [ Ok 1; Ok 2; Ok 3 ] : int t Or_error.t)];
  [%expect {| (Ok (1 2 3)) |}];
  print_s [%sexp (commute [ Ok 1; Ok 2; Or_error.error_string "foo" ] : int t Or_error.t)];
  [%expect {| (Error foo) |}]
;;

let%expect_test "stable types" =
  let test sexp_of_t t_of_sexp =
    let sexp = sexp_of_t sexp_of_int t in
    print_s sexp;
    print_s ([%sexp_of: bool] ([%equal: int t] t (t_of_sexp int_of_sexp sexp)))
  in
  test Stable.V1.sexp_of_t Stable.V1.t_of_sexp;
  test Stable.V2.sexp_of_t Stable.V2.t_of_sexp;
  test Stable.V3.sexp_of_t Stable.V3.t_of_sexp;
  test sexp_of_t t_of_sexp;
  [%expect
    {|
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    |}];
  let test bin_writer_t bin_read_t =
    let bytes = Bin_prot.Writer.to_bytes (bin_writer_t Int.bin_writer_t) t in
    bytes |> [%sexp_of: bytes] |> print_s;
    let buf = Bin_prot.Common.create_buf (Bytes.length bytes) in
    Bin_prot.Common.blit_bytes_buf bytes buf ~len:(Bytes.length bytes);
    let t' = bin_read_t Int.bin_read_t buf ~pos_ref:(ref 0) in
    print_s ([%sexp_of: bool] ([%equal: int t] t t'))
  in
  test Stable.V1.bin_writer_t Stable.V1.bin_read_t;
  test Stable.V2.bin_writer_t Stable.V2.bin_read_t;
  test Stable.V3.bin_writer_t Stable.V3.bin_read_t;
  test Unstable.bin_writer_t Unstable.bin_read_t;
  [%expect
    {|
    "\001\003\002\003\004"
    true
    "\001\003\002\003\004"
    true
    "\004\001\002\003\004"
    true
    "\004\001\002\003\004"
    true
    |}]
;;

let%expect_test "folds" =
  let module M = struct
    type t =
      | Init
      | F of t * t
      | Leaf of int
    [@@deriving equal, sexp_of, variants]

    let rec flatten = function
      | Init -> []
      | F (a, b) -> List.concat_map [ a; b ] ~f:flatten
      | Leaf i -> [ i ]
    ;;
  end
  in
  let test nums =
    let leaves = Nonempty_list.map nums ~f:M.leaf in
    let reduced = Nonempty_list.reduce leaves ~f:M.f in
    let folded = Nonempty_list.fold leaves ~init:M.init ~f:M.f in
    let folded_nonempty =
      Nonempty_list.fold_nonempty leaves ~init:(fun hd -> M.f M.init hd) ~f:M.f
    in
    let folded_right = Nonempty_list.fold_right leaves ~init:M.init ~f:M.f in
    let nums = Nonempty_list.to_list nums in
    assert ([%equal: int list] nums (M.flatten reduced));
    assert ([%equal: int list] nums (M.flatten folded));
    assert ([%equal: int list] nums (M.flatten folded_nonempty));
    assert ([%equal: int list] nums (M.flatten folded_right));
    print_s
      [%message
        (reduced : M.t) (folded : M.t) (folded_nonempty : M.t) (folded_right : M.t)]
  in
  test [ 1 ];
  [%expect
    {|
    ((reduced (Leaf 1)) (folded (F Init (Leaf 1)))
     (folded_nonempty (F Init (Leaf 1))) (folded_right (F (Leaf 1) Init)))
    |}];
  test [ 1; 2 ];
  [%expect
    {|
    ((reduced (F (Leaf 1) (Leaf 2))) (folded (F (F Init (Leaf 1)) (Leaf 2)))
     (folded_nonempty (F (F Init (Leaf 1)) (Leaf 2)))
     (folded_right (F (Leaf 1) (F (Leaf 2) Init))))
    |}];
  test [ 1; 2; 3 ];
  [%expect
    {|
    ((reduced (F (F (Leaf 1) (Leaf 2)) (Leaf 3)))
     (folded (F (F (F Init (Leaf 1)) (Leaf 2)) (Leaf 3)))
     (folded_nonempty (F (F (F Init (Leaf 1)) (Leaf 2)) (Leaf 3)))
     (folded_right (F (Leaf 1) (F (Leaf 2) (F (Leaf 3) Init)))))
    |}];
  test [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ];
  [%expect
    {|
    ((reduced
      (F
       (F
        (F
         (F (F (F (F (F (Leaf 1) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5))
          (Leaf 6))
         (Leaf 7))
        (Leaf 8))
       (Leaf 9)))
     (folded
      (F
       (F
        (F
         (F (F (F (F (F (F Init (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5))
          (Leaf 6))
         (Leaf 7))
        (Leaf 8))
       (Leaf 9)))
     (folded_nonempty
      (F
       (F
        (F
         (F (F (F (F (F (F Init (Leaf 1)) (Leaf 2)) (Leaf 3)) (Leaf 4)) (Leaf 5))
          (Leaf 6))
         (Leaf 7))
        (Leaf 8))
       (Leaf 9)))
     (folded_right
      (F (Leaf 1)
       (F (Leaf 2)
        (F (Leaf 3)
         (F (Leaf 4)
          (F (Leaf 5) (F (Leaf 6) (F (Leaf 7) (F (Leaf 8) (F (Leaf 9) Init)))))))))))
    |}];
  ()
;;

let%expect_test "folding_map" =
  print_s
    [%sexp
      (folding_map [ 1; 2; 3; 4; 5; 6 ] ~init:0 ~f:(fun acc x -> acc + x, acc) : int t)];
  [%expect {| (0 1 3 6 10 15) |}]
;;

let%expect_test "fold_map" =
  print_s
    [%sexp
      (fold_map [ 1; 2; 3; 4; 5; 6 ] ~init:0 ~f:(fun acc x -> acc + x, acc) : int * int t)];
  [%expect {| (21 (0 1 3 6 10 15)) |}]
;;

let%expect_test "mapi" =
  print_s [%sexp (mapi [ "a"; "b"; "c"; "d" ] ~f:(fun i x -> i, x) : (int * string) t)];
  [%expect {| ((0 a) (1 b) (2 c) (3 d)) |}]
;;

let%expect_test "transpose" =
  print_s [%sexp (transpose [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] : int t t option)];
  [%expect {| (((1 4 7) (2 5 8) (3 6 9))) |}];
  print_s [%sexp (transpose [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ] : int t t option)];
  [%expect {| (((1 3 5) (2 4 6))) |}];
  print_s [%sexp (transpose [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] : int t t option)];
  [%expect {| (((1 4) (2 5) (3 6))) |}];
  print_s [%sexp (transpose [ [ 1 ] ] : int t t option)];
  [%expect {| (((1))) |}];
  print_s [%sexp (transpose [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ] : int t t option)];
  [%expect {| () |}];
  print_s [%sexp (transpose [ [ 1 ]; [ 2; 3 ] ] : int t t option)];
  [%expect {| () |}]
;;

let%expect_test "transpose_exn" =
  print_s [%sexp (transpose_exn [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] : int t t)];
  [%expect {| ((1 4 7) (2 5 8) (3 6 9)) |}];
  print_s [%sexp (transpose_exn [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ] ] : int t t)];
  [%expect {| ((1 3 5) (2 4 6)) |}];
  print_s [%sexp (transpose_exn [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] : int t t)];
  [%expect {| ((1 4) (2 5) (3 6)) |}];
  print_s [%sexp (transpose_exn [ [ 1 ] ] : int t t)];
  [%expect {| ((1)) |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    transpose_exn [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ]);
  [%expect {| ("transpose got lists of different lengths" (lengths (2 2 1))) |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    transpose_exn [ [ 1 ]; [ 2; 3 ] ]);
  [%expect {| ("transpose got lists of different lengths" (lengths (1 2))) |}]
;;

let%expect_test "rev_append" =
  let test xs ys = rev_append xs ys |> [%sexp_of: int Nonempty_list.t] |> print_s in
  test [ 1; 2; 3 ] [ 4 ];
  [%expect {| (3 2 1 4) |}];
  test [ 1; 2 ] [ 3; 4 ];
  [%expect {| (2 1 3 4) |}];
  test [ 1 ] [ 2; 3; 4 ];
  [%expect {| (1 2 3 4) |}];
  test [] [ 1; 2; 3; 4 ];
  [%expect {| (1 2 3 4) |}]
;;

let%expect_test "rev'" =
  let test xs =
    rev' xs |> [%sexp_of: int Nonempty_list.Reversed.With_sexp_of.t] |> print_s
  in
  test [ 1; 2; 3; 4 ];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ];
  [%expect {| (3 2 1) |}];
  test [ 1; 2 ];
  [%expect {| (2 1) |}];
  test [ 1 ];
  [%expect {| (1) |}]
;;

let%expect_test "Reversed.rev" =
  let test xs = Reversed.rev xs |> [%sexp_of: int Nonempty_list.t] |> print_s in
  test [ 1; 2; 3; 4 ];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ];
  [%expect {| (3 2 1) |}];
  test [ 1; 2 ];
  [%expect {| (2 1) |}];
  test [ 1 ];
  [%expect {| (1) |}]
;;

let%expect_test "Reversed.rev_append" =
  let test xs ys =
    Reversed.rev_append xs ys |> [%sexp_of: int Nonempty_list.t] |> print_s
  in
  test [ 1; 2; 3; 4 ] [];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ] [ 4 ];
  [%expect {| (3 2 1 4) |}];
  test [ 1; 2 ] [ 3; 4 ];
  [%expect {| (2 1 3 4) |}];
  test [ 1 ] [ 2; 3; 4 ];
  [%expect {| (1 2 3 4) |}]
;;

let%expect_test "Reversed.rev_map" =
  let test xs =
    Reversed.rev_map ~f:Int.to_string xs |> [%sexp_of: string Nonempty_list.t] |> print_s
  in
  test [ 1; 2; 3; 4 ];
  [%expect {| (4 3 2 1) |}];
  test [ 1; 2; 3 ];
  [%expect {| (3 2 1) |}];
  test [ 1; 2 ];
  [%expect {| (2 1) |}];
  test [ 1 ];
  [%expect {| (1) |}]
;;

let%expect_test "Reversed.rev_mapi" =
  let test xs =
    Reversed.rev_mapi xs ~f:(fun i n -> sprintf "%d/%d" n i)
    |> [%sexp_of: string Nonempty_list.t]
    |> print_s
  in
  test [ 5; 6; 7; 8 ];
  [%expect {| (8/3 7/2 6/1 5/0) |}];
  test [ 5; 6; 7 ];
  [%expect {| (7/2 6/1 5/0) |}];
  test [ 5; 6 ];
  [%expect {| (6/1 5/0) |}];
  test [ 5 ];
  [%expect {| (5/0) |}]
;;

let%expect_test "Reversed.With_sexp_of" =
  print_s [%sexp ([ 1 ] : int Reversed.With_sexp_of.t)];
  [%expect {| (1) |}];
  print_s [%sexp ([ 1; 2 ] : int Reversed.With_sexp_of.t)];
  [%expect {| (1 2) |}];
  print_s [%sexp ([ 1; 2; 3 ] : int Reversed.With_sexp_of.t)];
  [%expect {| (1 2 3) |}];
  [%expect {| |}]
;;

let%expect_test "Reversed.With_rev_sexp_of" =
  print_s [%sexp ([ 1 ] : int Reversed.With_rev_sexp_of.t)];
  [%expect {| (1) |}];
  print_s [%sexp ([ 1; 2 ] : int Reversed.With_rev_sexp_of.t)];
  [%expect {| (2 1) |}];
  print_s [%sexp ([ 1; 2; 3 ] : int Reversed.With_rev_sexp_of.t)];
  [%expect {| (3 2 1) |}]
;;

let%expect_test "init" =
  let test n =
    Nonempty_list.init n ~f:Int.to_string |> [%sexp_of: string Nonempty_list.t] |> print_s
  in
  test 4;
  [%expect {| (0 1 2 3) |}];
  test 3;
  [%expect {| (0 1 2) |}];
  test 2;
  [%expect {| (0 1) |}];
  test 1;
  [%expect {| (0) |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test 0);
  [%expect {| (Invalid_argument "Nonempty_list.init 0") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test (-1));
  [%expect {| (Invalid_argument "Nonempty_list.init -1") |}];
  ()
;;

let%expect_test "iteri" =
  let test xs = Nonempty_list.iteri xs ~f:(fun i n -> printf "%d/%d " n i) in
  test [ 5; 6; 7; 8 ];
  [%expect {| 5/0 6/1 7/2 8/3 |}];
  test [ 5; 6; 7 ];
  [%expect {| 5/0 6/1 7/2 |}];
  test [ 5; 6 ];
  [%expect {| 5/0 6/1 |}];
  test [ 5 ];
  [%expect {| 5/0 |}]
;;

let%expect_test "findi" =
  let test k v =
    Nonempty_list.init 3 ~f:(fun x -> x * x)
    |> findi ~f:(fun i x -> i = k && x = v)
    |> printf !"%{sexp: (int * int) option}"
  in
  test 0 0;
  [%expect {| ((0 0)) |}];
  test 0 1;
  [%expect {| () |}];
  test 1 1;
  [%expect {| ((1 1)) |}];
  test 1 2;
  [%expect {| () |}];
  test 2 4;
  [%expect {| ((2 4)) |}];
  test 2 5;
  [%expect {| () |}];
  ()
;;

let%expect_test "findi_exn" =
  let test k v =
    Nonempty_list.init 3 ~f:(fun x -> x * x)
    |> findi_exn ~f:(fun i x -> i = k && x = v)
    |> printf !"%{sexp: (int * int)}"
  in
  test 0 0;
  [%expect {| (0 0) |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test 0 1);
  [%expect {| (Not_found_s "Nonempty_list.findi_exn: not found") |}];
  test 1 1;
  [%expect {| (1 1) |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test 1 2);
  [%expect {| (Not_found_s "Nonempty_list.findi_exn: not found") |}];
  test 2 4;
  [%expect {| (2 4) |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test 2 5);
  [%expect {| (Not_found_s "Nonempty_list.findi_exn: not found") |}];
  ()
;;

let%expect_test "find_mapi" =
  let test n =
    Nonempty_list.init 5 ~f:(fun x -> x * x)
    |> find_mapi ~f:(fun i x -> if i = n then Some x else None)
    |> printf !"%{sexp: int option}"
  in
  test 0;
  [%expect {| (0) |}];
  test 2;
  [%expect {| (4) |}];
  test 4;
  [%expect {| (16) |}];
  test 6;
  [%expect {| () |}];
  ()
;;

let%expect_test "counti" =
  let test l = l |> counti ~f:(fun i x -> i = x) |> printf !"%d" in
  test [ 0; 1; 2 ];
  [%expect {| 3 |}];
  test [ 9; 1; 2 ];
  [%expect {| 2 |}];
  test [ 9; 1; 9 ];
  [%expect {| 1 |}];
  test [ 9; 9; 9 ];
  [%expect {| 0 |}];
  ()
;;

let%expect_test "for_alli" =
  let test l = l |> for_alli ~f:(fun i x -> i = x) |> printf !"%b" in
  test [ 0; 1; 2 ];
  [%expect {| true |}];
  test [ 9; 1; 2 ];
  [%expect {| false |}];
  test [ 9; 1; 9 ];
  [%expect {| false |}];
  test [ 9; 9; 9 ];
  [%expect {| false |}];
  ()
;;

let%expect_test "existsi" =
  let test l = l |> existsi ~f:(fun i x -> i = x) |> printf !"%b" in
  test [ 0; 1; 2 ];
  [%expect {| true |}];
  test [ 9; 1; 2 ];
  [%expect {| true |}];
  test [ 9; 1; 9 ];
  [%expect {| true |}];
  test [ 9; 9; 9 ];
  [%expect {| false |}];
  ()
;;

let%expect_test "foldi" =
  Nonempty_list.init 5 ~f:(fun x -> x * x)
  |> foldi ~init:[] ~f:(fun i acc x -> (i, x) :: acc)
  |> List.rev
  |> printf !"%{sexp: (int * int) list}";
  [%expect {| ((0 0) (1 1) (2 4) (3 9) (4 16)) |}]
;;

(* Since the behavior of [Option] functions differs fundamentally between empty and
   non-empty lists, explicitly ensure that we test both cases in addition to whatever
   tests quickcheck generates (even though it's extremely likely that the quickcheck tests
   do contain empty lists. *)
let run_on_empty_and_nonempty_lists f =
  Quickcheck.test [%quickcheck.generator: int list] ~examples:[ []; [ 1 ] ] ~f
;;

let%expect_test "Option" =
  run_on_empty_and_nonempty_lists (fun l ->
    match%optional.Nonempty_list.Option l with
    | None -> assert (List.is_empty l)
    | Some nonempty ->
      assert ([%equal: int Nonempty_list.t] nonempty (Nonempty_list.of_list_exn l)))
;;

let%expect_test "Option does not allocate" =
  run_on_empty_and_nonempty_lists (fun l ->
    let round_tripped =
      Expect_test_helpers_core.require_no_allocation (fun () ->
        match%optional.Nonempty_list.Option l with
        | None -> Sys.opaque_identity Nonempty_list.Option.none
        | Some nonempty ->
          Sys.opaque_identity (Nonempty_list.Option.some (Sys.opaque_identity nonempty)))
    in
    assert (phys_equal l round_tripped))
;;

let%expect_test "remove_consecutive_duplicates" =
  let test l ~which_to_keep =
    mapi l ~f:(fun idx elem -> elem, idx)
    |> remove_consecutive_duplicates ~which_to_keep ~equal:[%equal: int * _]
    |> [%sexp_of: (int * int) t]
    |> print_s
  in
  test [ 0; 1; 1; 2 ] ~which_to_keep:`First;
  [%expect {| ((0 0) (1 1) (2 3)) |}];
  test [ 0; 0; 2; 2 ] ~which_to_keep:`Last;
  [%expect {| ((0 1) (2 3)) |}];
  test [ 0; 0; 2; 2; 0 ] ~which_to_keep:`First;
  [%expect {| ((0 0) (2 2) (0 4)) |}];
  test [ 0 ] ~which_to_keep:`First;
  [%expect {| ((0 0)) |}];
  test [ 0; 0; 0; 0 ] ~which_to_keep:`Last;
  [%expect {| ((0 3)) |}];
  ()
;;

module%test Partition = struct
  let%expect_test "partition_tf" =
    let test xs =
      let f x = x % 2 = 0 in
      let partition = Nonempty_list.partition_tf xs ~f in
      partition |> [%sexp_of: (int, int) Partition.t] |> print_s;
      assert (
        [%equal: int list * int list]
          ( Partition.fst partition |> Option.of_option
          , Partition.snd partition |> Option.of_option )
          (Nonempty_list.partition_tf' xs ~f))
    in
    test t;
    [%expect {| (Both ((2 4) (1 3))) |}];
    test [ 1; 3 ];
    [%expect {| (Snd (1 3)) |}];
    test [ 2; 4 ];
    [%expect {| (Fst (2 4)) |}]
  ;;

  let%expect_test "partition_map" =
    let test xs =
      let f x : _ Either.t =
        if x % 2 = 0 then First x else Second [%string "odd %{x#Int}"]
      in
      let partition = Nonempty_list.partition_map xs ~f in
      partition |> [%sexp_of: (int, string) Partition.t] |> print_s;
      assert (
        [%equal: int list * string list]
          ( Partition.fst partition |> Option.of_option
          , Partition.snd partition |> Option.of_option )
          (Nonempty_list.partition_map' xs ~f))
    in
    test t;
    [%expect {| (Both ((2 4) ("odd 1" "odd 3"))) |}];
    test [ 1; 3 ];
    [%expect {| (Snd ("odd 1" "odd 3")) |}];
    test [ 2; 4 ];
    [%expect {| (Fst (2 4)) |}]
  ;;

  let%expect_test "partition_result" =
    let test xs =
      let xs =
        Nonempty_list.map xs ~f:(fun x ->
          if x % 2 = 0 then Ok x else Error [%string "odd %{x#Int}"])
      in
      let partition = Nonempty_list.partition_result xs in
      partition |> [%sexp_of: (int, string) Partition.t] |> print_s;
      assert (
        [%equal: int list * string list]
          ( Partition.fst partition |> Option.of_option
          , Partition.snd partition |> Option.of_option )
          (Nonempty_list.partition_result' xs))
    in
    test t;
    [%expect {| (Both ((2 4) ("odd 1" "odd 3"))) |}];
    test [ 1; 3 ];
    [%expect {| (Snd ("odd 1" "odd 3")) |}];
    test [ 2; 4 ];
    [%expect {| (Fst (2 4)) |}]
  ;;
end

module%test Partition3 = struct
  let f x =
    match x % 3 with
    | 0 -> `Fst x
    | 1 -> `Snd (Int.to_float x)
    | 2 -> `Trd (Int.to_string x)
    | _ -> assert false
  ;;

  let convert (partition3 : _ Nonempty_list.Partition3.t) =
    match partition3 with
    | Fst t -> to_list t, [], []
    | Snd t -> [], to_list t, []
    | Trd t -> [], [], to_list t
    | Fst_snd (fsts, snds) -> to_list fsts, to_list snds, []
    | Fst_trd (fsts, trds) -> to_list fsts, [], to_list trds
    | Snd_trd (snds, trds) -> [], to_list snds, to_list trds
    | Fst_snd_trd (fsts, snds, trds) -> to_list fsts, to_list snds, to_list trds
  ;;

  let%expect_test "partition3_map (manual)" =
    let test xs =
      let partition3 = Nonempty_list.partition3_map xs ~f in
      let from_list = List.partition3_map (to_list xs) ~f in
      let three_lists = convert partition3 in
      assert ([%equal: int list * float list * string list] three_lists from_list);
      partition3 |> [%sexp_of: (int, float, string) Partition3.t] |> print_s
    in
    test [ 1; 2; 3; 4 ];
    [%expect {| (Fst_snd_trd (3) (1 4) (2)) |}];
    test [ 1; 3 ];
    [%expect {| (Fst_snd (3) (1)) |}];
    test [ 5; 3 ];
    [%expect {| (Fst_trd (3) (5)) |}];
    test [ 2; 4 ];
    [%expect {| (Snd_trd (4) (2)) |}];
    test [ 6; 3 ];
    [%expect {| (Fst (6 3)) |}];
    test [ 4; 7 ];
    [%expect {| (Snd (4 7)) |}];
    test [ 5; 2 ];
    [%expect {| (Trd (5 2)) |}]
  ;;

  let%expect_test "partition3_map (quickcheck)" =
    quickcheck
      [%test_result: int list * float list * string list]
      (fun l -> List.partition3_map l ~f)
      (fun l -> Nonempty_list.partition3_map l ~f |> convert)
  ;;
end
