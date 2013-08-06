open Std_internal
open Int.Replace_polymorphic_compare

module Test (Pool : Pool.S) = struct

  module Pointer = Pool.Pointer

  TEST_UNIT "get_tuple with length = 1" =
    let pool = Pool.create Pool.Slots.t1 ~capacity:10 ~dummy:0 in
    let ptr = Pool.new1 pool 42 in
    let tuple = Pool.get_tuple pool ptr in
    assert (tuple = 42)
  ;;

  TEST_UNIT "get_tuple with length > 1" =
    let pool = Pool.create Pool.Slots.t3 ~capacity:10 ~dummy:(0, 0, 0) in
    let ptr = Pool.new3 pool 42 42 42 in
    let tuple = Pool.get_tuple pool ptr in
    assert (Poly.equal tuple (42, 42, 42))
  ;;

  module rec List_ : sig
    module Pool : sig
      type 'a t with sexp_of

      val create : capacity:int -> dummy:'a -> 'a t
      val length : _ t -> int
      val is_full : _ t -> bool
      val grow : ?capacity:int -> 'a t -> 'a t
    end

    type 'a t with sexp_of

    val nil : unit -> _ t
    val is_nil : _ t -> bool
    val create : 'a Pool.t -> 'a -> 'a t -> 'a t
    val free : 'a Pool.t -> 'a t -> unit
    val head : 'a Pool.t -> 'a t -> 'a
    val tail : 'a Pool.t -> 'a t -> 'a t
    val get : 'a Pool.t -> 'a t -> ('a * 'a t) option

    val equal : 'a t -> 'a t -> bool

    module Id : sig
      type t
    end

    val id_of_pointer : 'a Pool.t -> 'a t -> Id.t
    val pointer_of_id_exn_is_supported : bool
    val pointer_of_id_exn : 'a Pool.t -> Id.t -> 'a t
  end = struct

    type 'a ty = ('a, 'a ty Pointer.t) Pool.Slots.t2 with sexp_of

    type 'a t = 'a ty Pointer.t with sexp_of

    let create = Pool.new2
    let free = Pool.free

    let nil = Pointer.null
    let is_nil = Pointer.is_null

    let equal = Pointer.phys_equal

    let head p t = Pool.get p t Pool.Slot.t0
    let tail p t = Pool.get p t Pool.Slot.t1

    let get t p = if is_nil p then None else Some (Pool.get_tuple t p)

    module Id = struct
      type t = Pointer.Id.t
    end

    let id_of_pointer = Pool.id_of_pointer
    let pointer_of_id_exn_is_supported = Pool.pointer_of_id_exn_is_supported
    let pointer_of_id_exn = Pool.pointer_of_id_exn

    module Pool = struct
      type 'a t = 'a ty Pool.t with sexp_of

      let create ~capacity ~dummy =
        Pool.create Pool.Slots.t2 ~capacity ~dummy:(dummy, nil ())
      ;;
      let is_full = Pool.is_full
      let grow = Pool.grow
      let length = Pool.length
    end
  end

  open List_

  (* [create] with invalid capacity *)
  TEST_UNIT =
    List.iter [ -1 ] ~f:(fun capacity ->
      assert (Result.is_error (Result.try_with (fun () ->
        ignore (Pool.create ~capacity ~dummy:())))))
  ;;

  (* [length] *)
  TEST_UNIT =
    let t = Pool.create ~capacity:3 ~dummy:13 in
    assert (Pool.length t = 0);
    let l1 = create t 13 (nil ()) in
    assert (Pool.length t = 1);
    let l2 = create t 13 (nil ()) in
    assert (Pool.length t = 2);
    free t l1;
    assert (Pool.length t = 1);
    free t l2;
    assert (Pool.length t = 0);
  ;;

  let rec grow_loop p num_left =
    if num_left > 0 then grow_loop (Pool.grow p) (num_left - 1)
  ;;

  (* [grow] an empty pool *)
  TEST_UNIT = grow_loop (Pool.create ~capacity:1 ~dummy:()) 10

  (* [grow] a non-empty pool *)
  TEST_UNIT =
    let p = Pool.create ~capacity:1 ~dummy:0 in
    ignore (create p 13 (nil ()));
    grow_loop p 10;
  ;;

  (* [grow] a non-empty pool, while adding each time *)
  TEST_UNIT =
    let rec loop p num_left =
      if num_left > 0 then begin
        ignore (create p 13 (nil ()));
        loop (Pool.grow p) (num_left - 1);
      end;
    in
    loop (Pool.create ~capacity:1 ~dummy:0) 10;
  ;;

  let rec fold p list ~init ~f =
    if is_nil list then
      init
    else
      fold p (tail p list) ~init:(f init (head p list)) ~f
  ;;

  let to_list p list = List.rev (fold p list ~init:[] ~f:(fun l a -> a :: l))

  (* [grow] on demand *)
  TEST_UNIT =
    let total_length = 10_000 in
    let rec loop i p list =
      let i = i - 1 in
      if i < 0 then
        assert (Poly.equal (to_list p list) (List.init total_length ~f:Fn.id))
      else begin
        let p =
          if not (Pool.is_full p) then
            p
          else
            Pool.grow p
        in
        loop i p (create p i list);
      end;
    in
    loop total_length (Pool.create ~capacity:0 ~dummy:0) (nil ());
  ;;

  (* [free] *)
  TEST_UNIT =
    let n = 10 in
    let p = Pool.create ~capacity:n ~dummy:0 in
    for _i = 1 to 4 do
      let ls = List.init n ~f:(fun i -> create p i (nil ())) in
      assert (Pool.is_full p);
      List.iter ls ~f:(fun l -> free p l);
    done;
  ;;

  (* [free] *)
  TEST_UNIT =
    let rec loop p num_iters_left num_to_alloc_this_iter live =
      if num_iters_left = 0 then
        List.iter live ~f:(fun l -> free p l)
      else
        let p, live =
          List.fold (List.init num_to_alloc_this_iter ~f:Fn.id) ~init:(p, live)
            ~f:(fun (p, live) i ->
              let p = if Pool.is_full p then Pool.grow p else p in
              p, (create p i (nil ()) :: live))
        in
        let to_free, live =
          let r = ref true in
          List.partition_map live ~f:(fun a ->
            r := not !r;
            if !r then `Fst a else `Snd a)
        in
        List.iter to_free ~f:(fun l -> free p l);
        loop p (num_iters_left - 1) (num_to_alloc_this_iter * 2) live;
    in
    loop (Pool.create ~capacity:1 ~dummy:0) 10 1 [];
  ;;

  (* [get_tuple] *)
  TEST_UNIT =
    let p = Pool.create ~capacity:10 ~dummy:0 in
    let l = create p 13 (nil ()) in
    let z1 = Option.value_exn (get p l) in
    let z2 = (13, nil ()) in
    assert (Poly.equal z1 z2);
  ;;

  (* [get] *)
  TEST_UNIT =
    let p = Pool.create ~capacity:10 ~dummy:0 in
    try
      let l = create p 13 (nil ()) in
      assert (not (is_nil l));
      assert (head p l = 13);
      assert (is_nil (tail p l));
      free p l;
    with exn -> failwiths "failure" (exn, p) <:sexp_of< exn * _ Pool.t >>
  ;;

  (* [sexp_of] *)
  TEST_UNIT =
    let sexp_of = <:sexp_of< int t >> in
    ignore (sexp_of (nil ()));
    let p = Pool.create ~capacity:10 ~dummy:0 in
    try
      let l = create p 13 (nil ()) in
      ignore (sexp_of l);
    with exn -> failwiths "failure" (exn, p) <:sexp_of< exn * _ Pool.t >>
  ;;

  (* [id_of_pointer], [pointer_of_id_exn] *)
  TEST_UNIT =
    if pointer_of_id_exn_is_supported then begin
      let capacity = 10 in
      let p = Pool.create ~capacity ~dummy:0 in
      let does_round_trip l = equal l (pointer_of_id_exn p (id_of_pointer p l)) in
      assert (does_round_trip (nil ()));
      let alloc_all () = Array.init capacity ~f:(fun _ -> create p 13 (nil ())) in
      let ls = alloc_all () in
      assert (Array.for_all ls ~f:does_round_trip);
      Array.iter ls ~f:(fun l -> free p l);
      let all_ls_fail () =
        Array.for_all ls ~f:(fun l ->
          does_raise (fun () -> pointer_of_id_exn p (id_of_pointer p l)))
      in
      assert (all_ls_fail ());
      let _ls' = alloc_all () in
      assert (all_ls_fail ());
    end;
  ;;

end

TEST_MODULE = Test (Pool)

TEST_MODULE =
  Test (struct
    include Pool.Unsafe
    let create slots ~capacity ~dummy:_ = create slots ~capacity
  end)
