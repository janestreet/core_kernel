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

(* This pins down current behavior of an unsupported use, not desired behavior. We use
   [int] so the test is memory-safe: with boxed elements the freed slot's dummy is
   returned typed as ['a], which is memory-unsafe. *)
let%expect_test "cmp that mutates the heap corrupts it (documented as unsupported)" =
  let heap = ref None in
  let cmp a b =
    Option.iter !heap ~f:(fun t ->
      heap := None;
      remove_top t);
    Int.compare a b
  in
  let t = create ~cmp () in
  heap := Some t;
  add t 1;
  add t 2;
  print_s
    [%message
      ""
        ~top:(top t : int option)
        ~length:(length t : int)
        ~to_list:(to_list t : int list)];
  [%expect {| ((top (0)) (length 1) (to_list (0 2))) |}];
  invariant ignore t;
  print_endline "invariant did not raise";
  [%expect {| invariant did not raise |}]
;;

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

let%expect_test "operations on explicitly removed elements" =
  let h = create ~cmp:Int.compare () in
  let elt = add_removable h 1 in
  ignore (add_removable h 2 : int Elt.t);
  remove h elt;
  print_s [%sexp (elt : int Elt.t)];
  [%expect {| () |}];
  require_does_raise (fun () -> Elt.value_exn elt);
  [%expect {| (Failure "Heap.value_exn: node was removed from the heap") |}];
  (* removing again is a no-op *)
  remove h elt;
  print_s [%sexp (h : int t)];
  [%expect {| (2) |}]
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

(* The order in which the heap returns elements that compare equal is not specified, but
   it is deterministic and some callers' outputs depend on it. These tests pin it down.
*)
module%test [@name "order of equal elements"] _ = struct
  let cmp = [%compare: int * _]
  let create () = create ~cmp ()
  let add t key ~id = add t (key, id)
  let drain t = List.init (length t) ~f:(fun _ -> pop_exn t)
  let ids l = List.map l ~f:snd

  (* Compact summary of a long sequence of observations. *)
  let fingerprint =
    List.fold ~init:0 ~f:(fun acc x -> [%hash: int * (int * int)] (acc, x))
  ;;

  let%expect_test "all equal, added then drained" =
    let t = create () in
    for id = 0 to 9 do
      add t 0 ~id
    done;
    print_s [%sexp (ids (drain t) : int list)];
    [%expect {| (0 1 2 3 4 5 6 7 8 9) |}]
  ;;

  let%expect_test "all equal, pops interleaved with adds" =
    let t = create () in
    let popped = ref [] in
    for id = 0 to 29 do
      add t 0 ~id;
      if id % 3 = 2 then popped := pop_exn t :: !popped
    done;
    print_s [%sexp (ids (List.rev !popped) : int list)];
    [%expect {| (2 5 8 11 14 17 20 23 26 29) |}];
    print_s [%sexp (ids (drain t) : int list)];
    [%expect {| (0 1 3 4 6 7 9 10 12 13 15 16 18 19 21 22 24 25 27 28) |}]
  ;;

  let%expect_test "duplicate keys" =
    let t = create () in
    List.iteri [ 3; 1; 3; 2; 1; 3; 0; 2; 2; 1; 3; 0; 1; 3; 2 ] ~f:(fun id key ->
      add t key ~id);
    print_s [%sexp (drain t : (int * int) list)];
    [%expect
      {|
      ((3 13)
       (3 10)
       (3 5)
       (3 0)
       (3 2)
       (2 3)
       (2 8)
       (2 7)
       (2 14)
       (1 12)
       (1 9)
       (1 1)
       (1 4)
       (0 6)
       (0 11))
      |}]
  ;;

  let%expect_test "all equal, long list of children (exercises allocating_merge_pairs)" =
    let t = create () in
    for id = 0 to 4999 do
      add t 0 ~id
    done;
    let first = List.init 40 ~f:(fun _ -> pop_exn t) in
    print_s [%sexp (ids first : int list)];
    [%expect
      {|
      (4960
       4961
       4962
       4963
       4964
       4965
       4966
       4967
       4968
       4969
       4970
       4971
       4972
       4973
       4974
       4975
       4976
       4977
       4978
       4979
       4980
       4981
       4982
       4983
       4984
       4985
       4986
       4987
       4988
       4989
       4990
       4991
       4992
       4993
       4994
       4995
       4996
       4997
       4998
       4999)
      |}];
    print_s [%sexp (fingerprint (drain t) : int)];
    [%expect {| 1030249178 |}]
  ;;

  let%expect_test "equal elements with removals" =
    let t = create () in
    let tokens = List.init 20 ~f:(fun id -> add_removable t (0, id)) in
    List.iteri tokens ~f:(fun i token -> if i % 4 = 1 then remove t token);
    ignore (pop_exn t : int * int);
    List.iteri tokens ~f:(fun i token -> if i % 4 = 3 then remove t token);
    print_s [%sexp (ids (drain t) : int list)];
    [%expect {| (4 8 12 16 0 2 6 10 14) |}]
  ;;

  let%expect_test "deterministic random operations on a few distinct keys" =
    let random = Random.State.make [| 0 |] in
    let t = create () in
    let tokens = Stack.create () in
    let observations = ref [] in
    let observe x = observations := x :: !observations in
    for id = 1 to 20_000 do
      let key = Random.State.int random 5 in
      match Random.State.int random 10 with
      | 0 | 1 | 2 | 3 -> add t key ~id
      | 4 -> Stack.push tokens (add_removable t (key, id))
      | 5 ->
        (match Stack.pop tokens with
         | None -> ()
         | Some token -> remove t token)
      | 6 ->
        (match Stack.pop tokens with
         | None -> ()
         | Some token -> Stack.push tokens (update t token (key, id)))
      | 7 | 8 | 9 -> Option.iter (pop t) ~f:observe
      | _ -> assert false
    done;
    invariant ignore t;
    print_s [%sexp (length t : int)];
    [%expect {| 3013 |}];
    print_s [%sexp (fingerprint (List.rev !observations) : int)];
    [%expect {| 158864271 |}];
    print_s [%sexp (fingerprint (drain t) : int)];
    [%expect {| 662961811 |}]
  ;;
end

module%test [@name "pool reuse, growth and copy"] _ = struct
  let%expect_test "nodes are reused after pop and tokens stay invalid" =
    let t = create ~min_size:1 ~cmp:Int.compare () in
    let tokens = List.init 100 ~f:(fun i -> add_removable t i) in
    invariant ignore t;
    for _ = 1 to 100 do
      ignore (pop_exn t : int)
    done;
    invariant ignore t;
    (* refilling the heap reuses the freed nodes... *)
    for i = 100 to 199 do
      add t i
    done;
    invariant ignore t;
    (* ...but the tokens of the popped elements do not come back to life *)
    print_s [%sexp (List.filter_map tokens ~f:Elt.value : int list)];
    [%expect {| () |}];
    List.iter tokens ~f:(remove t);
    print_s
      [%sexp (length t : int), (to_list t |> List.sort ~compare:Int.compare : int list)];
    [%expect
      {|
      (100 (
        100
        101
        102
        103
        104
        105
        106
        107
        108
        109
        110
        111
        112
        113
        114
        115
        116
        117
        118
        119
        120
        121
        122
        123
        124
        125
        126
        127
        128
        129
        130
        131
        132
        133
        134
        135
        136
        137
        138
        139
        140
        141
        142
        143
        144
        145
        146
        147
        148
        149
        150
        151
        152
        153
        154
        155
        156
        157
        158
        159
        160
        161
        162
        163
        164
        165
        166
        167
        168
        169
        170
        171
        172
        173
        174
        175
        176
        177
        178
        179
        180
        181
        182
        183
        184
        185
        186
        187
        188
        189
        190
        191
        192
        193
        194
        195
        196
        197
        198
        199))
      |}]
  ;;

  let%expect_test "growth" =
    let t = create ~min_size:1 ~cmp:Int.compare () in
    let expected = ref [] in
    for i = 1 to 1000 do
      add t (-i);
      expected := -i :: !expected;
      if i % 3 = 0
      then (
        let popped = pop_exn t in
        expected := List.filter !expected ~f:(fun x -> x <> popped))
    done;
    invariant ignore t;
    print_s [%sexp (length t : int)];
    [%expect {| 667 |}];
    require_equal (module Int) (length t) (List.length !expected);
    require
      ([%equal: int list]
         (List.sort (to_list t) ~compare:Int.compare)
         (List.sort !expected ~compare:Int.compare))
  ;;

  let%expect_test "copy is independent of the original, including free nodes" =
    let t = create ~min_size:1 ~cmp:Int.compare () in
    let tokens = List.init 10 ~f:(fun i -> add_removable t i) in
    List.iteri tokens ~f:(fun i token -> if i % 2 = 0 then remove t token);
    let t' = copy t in
    invariant ignore t;
    invariant ignore t';
    add t 100;
    add t' 200;
    add t' 300;
    remove t (List.nth_exn tokens 1);
    print_s [%sexp (to_list t |> List.sort ~compare:Int.compare : int list)];
    [%expect {| (3 5 7 9 100) |}];
    print_s [%sexp (to_list t' |> List.sort ~compare:Int.compare : int list)];
    [%expect {| (1 3 5 7 9 200 300) |}];
    invariant ignore t;
    invariant ignore t';
    print_s [%sexp (List.init 6 ~f:(fun _ -> pop_exn t') : int list)];
    [%expect {| (200 9 7 5 3 1) |}];
    print_s [%sexp (List.init 5 ~f:(fun _ -> pop_exn t) : int list)];
    [%expect {| (100 9 7 5 3) |}]
  ;;
end
