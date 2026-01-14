open! Core
open Poly
open Expect_test_helpers_core
module Heap = Pairing_heap

(* Container tests. Heap has no t_of_sexp because there is no way to deserialize a
   comparison function, so we simulate it. *)
include Base_test_helpers.Test_container.Test_S0 (struct
    include Heap

    module Elt = struct
      type t = int [@@deriving sexp]

      let of_int = Fn.id
      let to_int = Fn.id
    end

    type nonrec t = int t [@@deriving sexp_of]

    let mem t int = mem t int ~equal:Int.equal
    let of_list ints = of_list ints ~cmp:Int.compare
    let t_of_sexp sexp = sexp |> [%of_sexp: int list] |> of_list
  end)

let%expect_test "Heap.sexp_of_t" =
  let test list =
    let heap = Heap.of_list list ~cmp:Int.compare in
    print_s [%sexp (heap : int Heap.t)];
    (* test for stability of element order in sexps, and make sure [sexp_of_t] does not
       accidentally mutate [t] *)
    while not (Heap.is_empty heap) do
      ignore (Heap.pop_exn heap : int);
      print_s [%sexp (heap : int Heap.t)]
    done
  in
  test [];
  [%expect {| () |}];
  test [ 3 ];
  [%expect
    {|
    (3)
    ()
    |}];
  test [ 3; 1; 4 ];
  [%expect
    {|
    (1 3 4)
    (3 4)
    (4)
    ()
    |}];
  test [ 3; 1; 4; 1; 5; 9; 2; 6; 5 ];
  [%expect
    {|
    (1 1 2 3 4 5 5 6 9)
    (1 2 3 4 5 5 6 9)
    (2 3 4 5 5 6 9)
    (3 4 5 5 6 9)
    (4 5 5 6 9)
    (5 5 6 9)
    (5 6 9)
    (6 9)
    (9)
    ()
    |}]
;;

let%expect_test "Heap.sexp_of_t with removes" =
  let test list =
    let heap = Heap.create ~cmp:Int.compare () in
    let elts = List.map list ~f:(Heap.add_removable heap) in
    print_s [%sexp (heap : int Heap.t)];
    (* test for stability of element order in sexps, and make sure [sexp_of_t] does not
       accidentally mutate [t] *)
    List.iter elts ~f:(fun elt ->
      Heap.remove heap elt;
      print_s [%sexp (heap : int Heap.t)])
  in
  test [];
  [%expect {| () |}];
  test [ 3 ];
  [%expect
    {|
    (3)
    ()
    |}];
  test [ 3; 1; 4 ];
  [%expect
    {|
    (1 3 4)
    (1 4)
    (4)
    ()
    |}];
  test [ 3; 1; 4; 1; 5; 9; 2; 6; 5 ];
  [%expect
    {|
    (1 1 2 3 4 5 5 6 9)
    (1 1 2 4 5 5 6 9)
    (1 2 4 5 5 6 9)
    (1 2 5 5 6 9)
    (2 5 5 6 9)
    (2 5 6 9)
    (2 5 6)
    (5 6)
    (5)
    ()
    |}]
;;

open! Heap

module%test _ = struct
  let data = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
  let t = of_list data ~cmp:Int.compare
  let () = invariant Fn.ignore t

  (* pop the zero at the top to force some heap structuring. This does not touch the sum. *)
  let (_ : int option) = pop t
  let () = invariant Fn.ignore t
  let list_sum = List.fold data ~init:0 ~f:(fun sum v -> sum + v)
  let heap_fold_sum = fold t ~init:0 ~f:(fun sum v -> sum + v)

  let heap_iter_sum =
    let r = ref 0 in
    iter t ~f:(fun v -> r := !r + v);
    !r
  ;;

  let%test _ = Int.( = ) list_sum heap_fold_sum
  let%test _ = Int.( = ) list_sum heap_iter_sum
end

module%test _ = struct
  module type Heap_intf = sig
    type 'a t [@@deriving sexp_of]

    include Invariant.S1 with type 'a t := 'a t

    val create : cmp:('a -> 'a -> int) -> 'a t
    val add : 'a t -> 'a -> unit
    val pop : 'a t -> 'a option
    val length : 'a t -> int
    val top : 'a t -> 'a option
    val remove_top : 'a t -> unit
    val to_list : 'a t -> 'a list
  end

  module That_heap : Heap_intf = struct
    type 'a t =
      { cmp : 'a -> 'a -> int
      ; mutable heap : 'a list
      }

    let sexp_of_t sexp_of_v t = List.sexp_of_t sexp_of_v t.heap
    let create ~cmp = { cmp; heap = [] }
    let add t v = t.heap <- List.sort ~compare:t.cmp (v :: t.heap)

    let pop t =
      match t.heap with
      | [] -> None
      | x :: xs ->
        t.heap <- xs;
        Some x
    ;;

    let length t = List.length t.heap
    let top t = List.hd t.heap

    let remove_top t =
      match t.heap with
      | [] -> ()
      | _ :: xs -> t.heap <- xs
    ;;

    let to_list t = t.heap
    let invariant _ = Fn.ignore
  end

  module This_heap : Heap_intf = struct
    type nonrec 'a t = 'a t [@@deriving sexp_of]

    let create ~cmp = create ~cmp ()
    let add = add
    let pop = pop
    let length = length
    let top = top
    let remove_top = remove_top
    let to_list = to_list
    let invariant = invariant
  end

  let this_to_string this = Sexp.to_string (This_heap.sexp_of_t Int.sexp_of_t this)
  let that_to_string that = Sexp.to_string (That_heap.sexp_of_t Int.sexp_of_t that)

  let length_check (t_a, t_b) =
    let this_len = This_heap.length t_a in
    let that_len = That_heap.length t_b in
    if this_len <> that_len
    then
      failwithf
        "error in length: %i (for %s) <> %i (for %s)"
        this_len
        (this_to_string t_a)
        that_len
        (that_to_string t_b)
        ()
  ;;

  let create () =
    let cmp = Int.compare in
    This_heap.create ~cmp, That_heap.create ~cmp
  ;;

  let add (this_t, that_t) v =
    This_heap.add this_t v;
    That_heap.add that_t v;
    length_check (this_t, that_t)
  ;;

  let pop (this_t, that_t) =
    let res1 = This_heap.pop this_t in
    let res2 = That_heap.pop that_t in
    if res1 <> res2
    then
      failwithf
        "pop results differ (%s, %s)"
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res2))
        ()
  ;;

  let top (this_t, that_t) =
    let res1 = This_heap.top this_t in
    let res2 = That_heap.top that_t in
    if res1 <> res2
    then
      failwithf
        "top results differ (%s, %s)"
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res2))
        ()
  ;;

  let remove_top (this_t, that_t) =
    This_heap.remove_top this_t;
    That_heap.remove_top that_t;
    length_check (this_t, that_t)
  ;;

  let internal_check (this_t, that_t) =
    let this_list = List.sort ~compare:Int.compare (This_heap.to_list this_t) in
    let that_list = List.sort ~compare:Int.compare (That_heap.to_list that_t) in
    assert (this_list = that_list);
    This_heap.invariant Fn.ignore this_t;
    That_heap.invariant Fn.ignore that_t
  ;;

  let%expect_test _ =
    let generator =
      let add =
        let%map.Quickcheck.Generator i = Int.gen_uniform_incl 0 100 in
        `Add i
      in
      let return = Quickcheck.Generator.return in
      Quickcheck.Generator.weighted_union
        (* This is biased towards adding (0.5 probability of add, 0.3 probability of pop
           or remove top), so the heap will tend to grow over time. This should test more
           interesting cases compared to always testing small heaps. *)
        [ 0.5, add
        ; 0.2, return `Pop
        ; 0.1, return `Top
        ; 0.1, return `Remove_top
        ; 0.1, return `Internal_check
        ]
      |> Quickcheck.Generator.list
    in
    Quickcheck.test generator ~sizes:(Sequence.repeat 10_000) ~trials:100 ~f:(fun ops ->
      let t = create () in
      List.iter ops ~f:(function
        | `Add i -> add t i
        | `Pop -> pop t
        | `Top -> top t
        | `Remove_top -> remove_top t
        | `Internal_check -> internal_check t))
  ;;
end

let integer_test f =
  let generator =
    let%map.Quickcheck.Generator ops =
      let add =
        let%map.Quickcheck.Generator i =
          Int.gen_uniform_incl Int.min_value Int.max_value
        in
        `Add i
      in
      (* We need to pop from time to time to trigger the amortized tree reorganizations.
         If we don't do this the resulting structure is just a linked list and the caller
         is not flexed as completely as it should be. *)
      Quickcheck.Generator.weighted_union
        [ 0.1, Quickcheck.Generator.return `Pop; 0.9, add ]
      |> Quickcheck.Generator.list
    in
    let t = create ~cmp:Int.compare () in
    List.iter ops ~f:(function
      | `Add i -> add t i
      | `Pop -> ignore (pop t : int option));
    t
  in
  Quickcheck.test generator ~f
;;

let%test_unit "clear" =
  integer_test (fun t ->
    let initial_elts = to_list t in
    clear t;
    if length t <> 0
    then
      raise_s
        [%message
          "not empty after clear"
            (initial_elts : int list)
            ~length_after_clear:(length t : int)])
;;

let test_copy ~add_removable ~remove =
  let sum t = fold t ~init:0 ~f:(fun acc i -> acc + i) in
  integer_test (fun t ->
    let token = add_removable t 100 in
    invariant Fn.ignore t;
    let t' = copy t in
    invariant Fn.ignore t';
    assert (sum t = sum t');
    assert (to_list t = to_list t');
    remove t token;
    assert (sum t = sum t' - 100))
;;

let%test_unit _ = test_copy ~add_removable ~remove
let%test_unit _ = test_copy ~add_removable:Unsafe.add_removable ~remove:Unsafe.remove

let test_removal ~add_removable ~remove ~elt_value_exn =
  let generator =
    let%map.Quickcheck.Generator elts =
      Int.gen_uniform_incl Int.min_value Int.max_value |> Quickcheck.Generator.list
    in
    let t = create ~cmp:Int.compare () in
    let tokens = List.map elts ~f:(fun i -> i, add_removable t i) in
    t, tokens
  in
  Quickcheck.test
    generator
    (* This replaced an old test that added exactly 10k elements, and this combination of
       sizes and trials produces a few of about that size. Reaching this exact size is
       probably not all that important. *)
    ~sizes:(Sequence.repeat 30_000)
    ~trials:20
    ~f:(fun (t, tokens) ->
      invariant Fn.ignore t;
      assert (length t = List.length tokens);
      let even_elts =
        List.filter_map tokens ~f:(fun (elt, token) ->
          assert (elt_value_exn token t = elt);
          if elt % 2 = 0
          then Some elt
          else (
            remove t token;
            None))
      in
      assert (length t = List.length even_elts);
      invariant Fn.ignore t;
      List.iteri (List.sort ~compare:Int.compare even_elts) ~f:(fun i elt ->
        if i % 1000 = 0 then invariant Fn.ignore t;
        match pop t with
        | None -> assert false
        | Some v -> assert (v = elt));
      assert (is_empty t))
;;

let%test_unit "remove" =
  test_removal ~add_removable ~remove ~elt_value_exn:(fun token _ -> Elt.value_exn token)
;;

let%test_unit "remove" =
  test_removal
    ~add_removable:Unsafe.add_removable
    ~remove:Unsafe.remove
    ~elt_value_exn:Unsafe.Elt.value
;;

let%test_unit _ =
  let generator =
    let%map.Quickcheck.Generator elts =
      Int.gen_uniform_incl Int.min_value Int.max_value |> Quickcheck.Generator.list
    in
    let t = create ~cmp:Int.compare () in
    List.iter elts ~f:(fun elt -> add t elt);
    t
  in
  Quickcheck.test generator ~sizes:(Sequence.repeat 30_000) ~trials:20 ~f:(fun t ->
    let rec loop last count =
      if count % 1_000 = 0 then invariant Fn.ignore t;
      match pop t with
      | None -> ()
      | Some v ->
        assert (v >= last);
        loop v (count + 1)
    in
    loop Int.min_value 0)
;;

let%test_unit _ = ignore (of_array [||] ~cmp:Int.compare : int t)

let%expect_test "operations on removed elements" =
  let h = create ~cmp:Int.compare () in
  let elt = add_removable h 1 in
  print_s [%sexp (elt : int Elt.t)];
  [%expect {| (1) |}];
  ignore (pop_exn h : int);
  require_does_raise (fun () -> Elt.value_exn elt);
  [%expect {| (Failure "Heap.value_exn: node was removed from the heap") |}];
  print_s [%sexp (elt : int Elt.t)];
  [%expect {| () |}]
;;

let%expect_test "pop_if" =
  let heap = Heap.of_list [ -1; 1; 2; 3 ] ~cmp:Int.compare in
  let show () = print_s [%sexp (heap : int Heap.t)] in
  show ();
  [%expect {| (-1 1 2 3) |}];
  require_some ~print_some:[%sexp_of: int] (Heap.pop_if heap (fun i -> i < 0));
  [%expect {| -1 |}];
  show ();
  [%expect {| (1 2 3) |}];
  require_none [%sexp_of: int] (Heap.pop_if heap (fun i -> i < 0));
  show ();
  [%expect {| (1 2 3) |}]
;;

let%expect_test "pop_if on empty heap" =
  let empty = Heap.create ~cmp:Int.compare () in
  require_none [%sexp_of: int] (Heap.pop_if empty (fun _ -> true))
;;

let test_pop_while_negative values =
  let heap = Heap.of_list values ~cmp:Int.compare in
  print_s [%message "before" (heap : int Heap.t)];
  let popped = Heap.pop_while heap (fun i -> i < 0) in
  print_s [%message "after" (popped : int list) (heap : int Heap.t)]
;;

let%expect_test "pop_while single" =
  test_pop_while_negative [ -1; 1; 2; 3 ];
  [%expect
    {|
    (before (heap (-1 1 2 3)))
    (after (popped (-1)) (heap (1 2 3)))
    |}]
;;

let%expect_test "pop_while empty" =
  test_pop_while_negative [];
  [%expect
    {|
    (before (heap ()))
    (after
      (popped ())
      (heap   ()))
    |}]
;;

let%expect_test "pop_while no popped" =
  test_pop_while_negative [ 5; 1; 10; 3; 2 ];
  [%expect
    {|
    (before (heap (1 2 3 5 10)))
    (after (popped ()) (heap (1 2 3 5 10)))
    |}]
;;

let%expect_test "pop_while all popped" =
  test_pop_while_negative [ -10; -1; -3; -2 ];
  [%expect
    {|
    (before (heap (-10 -3 -2 -1)))
    (after (popped (-10 -3 -2 -1)) (heap ()))
    |}]
;;

let%expect_test "pop_while repeated" =
  test_pop_while_negative [ -10; 2; -1; -1; -1; 3; 10 ];
  [%expect
    {|
    (before (heap (-10 -1 -1 -1 2 3 10)))
    (after (popped (-10 -1 -1 -1)) (heap (2 3 10)))
    |}]
;;
