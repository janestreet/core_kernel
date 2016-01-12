let%test_module _ = (module (struct
  open Common
  open Sexplib.Conv
  module Sexp = Sexplib.Sexp
  module Array = Core_array
  module List = Core_list
  module Int = Core_int

  module Core_queue = Core_queue_debug.Debug (Core_queue)
  open Core_queue

  let does_raise = Exn.does_raise

  let () = show_messages := false

  type nonrec 'a t = 'a t [@@deriving bin_io, sexp]

  let capacity = capacity
  let set_capacity = set_capacity

  let%test_unit _ =
    let t = create () in
    assert (capacity t = 1);
    enqueue t 1;
    assert (capacity t = 1);
    enqueue t 2;
    assert (capacity t = 2);
    enqueue t 3;
    assert (capacity t = 4);
    set_capacity t 0;
    assert (capacity t = 4);
    set_capacity t 3;
    assert (capacity t = 4);
    set_capacity t 100;
    assert (capacity t = 128);
    enqueue t 4;
    enqueue t 5;
    set_capacity t 0;
    assert (capacity t = 8)
  ;;


  let round_trip_sexp t =
    let sexp = sexp_of_t Int.sexp_of_t t in
    let t'   = t_of_sexp Int.t_of_sexp sexp in
    assert (to_list t = to_list t')
  ;;
  let%test_unit _ = round_trip_sexp (of_list [1; 2; 3; 4])
  let%test_unit _ = round_trip_sexp (create ())
  let%test_unit _ = round_trip_sexp (of_list [])

  let invariant = invariant

  let create = create

  let singleton = singleton
  let%test_unit _ =
    let t = singleton 7 in
    assert (length t = 1);
    assert (capacity t = 1);
    assert (dequeue t = Some 7);
    assert (dequeue t = None)
  ;;

  let get = get
  let set = set
  let%test_unit _ =
    let t = create () in
    let get_opt t i = try Some (get t i) with _ -> None in
    assert (get_opt t 0 = None);
    assert (get_opt t (-1) = None);
    assert (get_opt t 10 = None);
    List.iter [ -1; 0; 1 ] ~f:(fun i -> assert (does_raise (fun () -> set t i 0)));
    enqueue t 0;
    enqueue t 1;
    enqueue t 2;
    assert (get_opt t 0 = Some 0);
    assert (get_opt t 1 = Some 1);
    assert (get_opt t 2 = Some 2);
    assert (get_opt t 3 = None);
    ignore (dequeue_exn t);
    assert (get_opt t 0 = Some 1);
    assert (get_opt t 1 = Some 2);
    assert (get_opt t 2 = None);
    set t 0 3;
    assert (get_opt t 0 = Some 3);
    assert (get_opt t 1 = Some 2);
    List.iter [ -1; 2 ] ~f:(fun i -> assert (does_raise (fun () -> set t i 0)))
  ;;

  let map = map
  let%test_unit _ =
    for i = 0 to 5 do
      let l = List.init i ~f:Fn.id in
      let t = of_list l in
      let f x = x * 2 in
      let t' = map t ~f in
      assert (to_list t' = List.map l ~f);
    done
  ;;

  let%test_unit _ =
    let t = create () in
    let t' = map t ~f:(fun x -> x * 2) in
    assert (length t' = length t);
    assert (length t' = 0);
    assert (to_list t' = [])
  ;;

  include Container_unit_tests.Test_S1 (Core_queue)

  let dequeue_exn = dequeue_exn
  let enqueue     = enqueue
  let peek        = peek
  let peek_exn    = peek_exn
  let last        = last
  let last_exn    = last_exn
  let%test_unit _ =
    let t = create () in
    assert (is_none (peek t));
    assert (is_none (last t));
    enqueue t 1;
    enqueue t 2;
    assert (peek t = Some 1);
    assert (peek_exn t = 1);
    assert (last t = Some 2);
    assert (last_exn t = 2);
    assert (dequeue_exn t = 1);
    assert (dequeue_exn t = 2);
    assert (does_raise (fun () -> dequeue_exn t));
    assert (does_raise (fun () -> peek_exn t));
    assert (does_raise (fun () -> last_exn t))
  ;;

  let enqueue_all = enqueue_all
  let%test_unit _ =
    let t = create () in
    enqueue_all t [1; 2; 3];
    assert (dequeue_exn t = 1);
    assert (dequeue_exn t = 2);
    assert (last t = Some 3);
    enqueue_all t [4; 5];
    assert (last t = Some 5);
    assert (dequeue_exn t = 3);
    assert (dequeue_exn t = 4);
    assert (dequeue_exn t = 5);
    assert (does_raise (fun () -> dequeue_exn t));
    enqueue_all t [];
    assert (does_raise (fun () -> dequeue_exn t));
  ;;

  let of_list = of_list
  let to_list = to_list

  let%test_unit _ =
    for i = 0 to 4 do
      let list = List.init i ~f:Fn.id in
      assert (Poly.equal (to_list (of_list list)) list);
    done
  ;;

  let%test _ =
    let t = create () in
    begin
      for i = 1 to 5 do enqueue t i done;
      to_list t = [1;2;3;4;5]
    end
  ;;

  let of_array = of_array
  let to_array = to_array

  let%test_unit _ =
    for len = 0 to 4 do
      let array = Array.init len ~f:Fn.id in
      assert (Poly.equal (to_array (of_array array)) array);
    done
  ;;

  let compare = compare
  let equal   = equal

  let%test_module "comparisons" = (module struct

    let sign x = if x < 0 then ~-1 else if x > 0 then 1 else 0

    let test t1 t2 =
      [%test_result: bool]
        (equal Int.equal t1 t2)
        ~expect:(List.equal ~equal:Int.equal (to_list t1) (to_list t2));
      [%test_result: int]
        (sign (compare Int.compare t1 t2))
        ~expect:(sign (List.compare Int.compare (to_list t1) (to_list t2)))
    ;;

    let lists =
      [ []
      ; [ 1 ]
      ; [ 2 ]
      ; [ 1; 1 ]
      ; [ 1; 2 ]
      ; [ 2; 1 ]
      ; [ 1; 1; 1 ]
      ; [ 1; 2; 3 ]
      ; [ 1; 2; 4 ]
      ; [ 1; 2; 4; 8 ]
      ; [ 1; 2; 3; 4; 5 ]
      ]
    ;;

    let%test_unit _ = (* [phys_equal] inputs *)
      List.iter lists ~f:(fun list ->
        let t = of_list list in
        test t t)
    ;;

    let%test_unit _ =
      List.iter lists ~f:(fun list1 ->
        List.iter lists ~f:(fun list2 ->
          test (of_list list1) (of_list list2)))
    ;;
  end)

  let clear          = clear
  let concat_map     = concat_map

  let blit_transfer  = blit_transfer

  let%test_unit _ =
    let q_list = [1; 2; 3; 4] in
    let q = of_list q_list in
    let q' = create () in
    blit_transfer ~src:q ~dst:q' ();
    assert (to_list q' = q_list);
    assert (to_list q = [])
  ;;

  let%test_unit _ =
    let q = of_list [1; 2; 3; 4] in
    let q' = create () in
    blit_transfer ~src:q ~dst:q' ~len:2 ();
    assert (to_list q' = [1; 2]);
    assert (to_list q  = [3; 4])
  ;;

  let%test_unit "blit_transfer on wrapped queues" =
    let list = [1; 2; 3; 4] in
    let q = of_list list in
    let q' = copy q in
    ignore (dequeue_exn q);
    ignore (dequeue_exn q);
    ignore (dequeue_exn q');
    ignore (dequeue_exn q');
    ignore (dequeue_exn q');
    enqueue q 5;
    enqueue q 6;
    blit_transfer ~src:q ~dst:q' ~len:3 ();
    assert (to_list q' = [4; 3; 4; 5]);
    assert (to_list q = [6])
  ;;

  let copy           = copy
  let dequeue        = dequeue
  let filter         = filter
  let filter_inplace = filter_inplace
  let filter_map     = filter_map
  let iter           = iter

  let%test_module "Linked_queue bisimulation" = (module struct
    module type Queue_intf = sig
      type 'a t [@@deriving sexp_of]

      val create : unit -> 'a t
      val enqueue : 'a t -> 'a -> unit
      val dequeue : 'a t -> 'a option
      val to_array : 'a t -> 'a array
      val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
      val iter : 'a t -> f:('a -> unit) -> unit
      val length : 'a t -> int
      val clear : 'a t -> unit
      val concat_map : 'a t -> f:('a -> 'b list) -> 'b t
      val filter_map     : 'a t -> f:('a -> 'b option) -> 'b t
      val filter         : 'a t -> f:('a -> bool)      -> 'a t
      val filter_inplace : 'a t -> f:('a -> bool)      -> unit
      val transfer : src:'a t -> dst:'a t -> unit
      val copy : 'a t -> 'a t
    end

    module That_queue : Queue_intf = Linked_queue

    module This_queue : Queue_intf = struct
      include Core_queue
      let create () = create ()
      let transfer ~src ~dst = blit_transfer ~src ~dst ()
    end

    let this_to_string this_t =
      Sexp.to_string (this_t |> [%sexp_of: int This_queue.t])
    ;;

    let that_to_string that_t =
      Sexp.to_string (that_t |> [%sexp_of: int That_queue.t])
    ;;

    let array_string arr =
      Sexp.to_string (arr |> [%sexp_of: int array])
    ;;

    let create () = (This_queue.create (), That_queue.create ())


    let enqueue (t_a, t_b) v =
      let start_a = This_queue.to_array t_a in
      let start_b = That_queue.to_array t_b in
      This_queue.enqueue t_a v;
      That_queue.enqueue t_b v;
      let end_a = This_queue.to_array t_a in
      let end_b = That_queue.to_array t_b in
      if end_a <> end_b
      then failwithf "enqueue transition failure of: %s -> %s vs. %s -> %s"
             (array_string start_a)
             (array_string end_a)
             (array_string start_b)
             (array_string end_b)
             ()
    ;;

    let iter (t_a, t_b) =
      let r_a, r_b = ref 0, ref 0 in
      This_queue.iter t_a ~f:(fun x -> r_a := !r_a + x);
      That_queue.iter t_b ~f:(fun x -> r_b := !r_b + x);
      if !r_a <> !r_b
      then failwithf "error in iter: %s (from %s) <> %s (from %s)"
             (Int.to_string !r_a)
             (this_to_string t_a)
             (Int.to_string !r_b)
             (that_to_string t_b)
             ()
    ;;

    let dequeue (t_a, t_b) =
      let start_a = This_queue.to_array t_a in
      let start_b = That_queue.to_array t_b in
      let a, b = This_queue.dequeue t_a, That_queue.dequeue t_b in
      let end_a = This_queue.to_array t_a in
      let end_b = That_queue.to_array t_b in
      if a <> b || end_a <> end_b
      then failwithf "error in dequeue: %s (%s -> %s) <> %s (%s -> %s)"
             (Option.value ~default:"None" (Option.map a ~f:Int.to_string))
             (array_string start_a)
             (array_string end_a)
             (Option.value ~default:"None" (Option.map b ~f:Int.to_string))
             (array_string start_b)
             (array_string end_b)
             ()
    ;;

    let clear (t_a, t_b) =
      This_queue.clear t_a;
      That_queue.clear t_b;
    ;;

    let is_even x = (x land 1) = 0

    let filter (t_a, t_b) =
      let t_a' = This_queue.filter t_a ~f:is_even in
      let t_b' = That_queue.filter t_b ~f:is_even in
      if This_queue.to_array t_a' <> That_queue.to_array t_b'
      then failwithf "error in filter: %s -> %s vs. %s -> %s"
             (this_to_string t_a)
             (this_to_string t_a')
             (that_to_string t_b)
             (that_to_string t_b')
             ()
    ;;

    let filter_inplace (t_a, t_b) =
      let start_a = This_queue.to_array t_a in
      let start_b = That_queue.to_array t_b in
      This_queue.filter_inplace t_a ~f:is_even;
      That_queue.filter_inplace t_b ~f:is_even;
      let end_a = This_queue.to_array t_a in
      let end_b = That_queue.to_array t_b in
      if end_a <> end_b
      then failwithf "error in filter_inplace: %s -> %s vs. %s -> %s"
             (array_string start_a)
             (array_string end_a)
             (array_string start_b)
             (array_string end_b)
             ()
    ;;

    let concat_map (t_a, t_b) =
      let f x = [x; x + 1; x + 2] in
      let t_a' = This_queue.concat_map t_a ~f in
      let t_b' = That_queue.concat_map t_b ~f in
      if (This_queue.to_array t_a') <> (That_queue.to_array t_b')
      then failwithf "error in concat_map: %s (for %s) <> %s (for %s)"
             (this_to_string t_a')
             (this_to_string t_a)
             (that_to_string t_b')
             (that_to_string t_b)
             ()
    ;;

    let filter_map (t_a, t_b) =
      let f x = if is_even x then None else Some (x + 1) in
      let t_a' = This_queue.filter_map t_a ~f in
      let t_b' = That_queue.filter_map t_b ~f in
      if (This_queue.to_array t_a') <> (That_queue.to_array t_b')
      then failwithf "error in filter_map: %s (for %s) <> %s (for %s)"
             (this_to_string t_a')
             (this_to_string t_a)
             (that_to_string t_b')
             (that_to_string t_b)
             ()
    ;;

    let copy (t_a, t_b) =
      let copy_a = This_queue.copy t_a in
      let copy_b = That_queue.copy t_b in
      let start_a = This_queue.to_array t_a in
      let start_b = That_queue.to_array t_b in
      let end_a = This_queue.to_array copy_a in
      let end_b = That_queue.to_array copy_b in
      if end_a <> end_b
      then failwithf "error in copy: %s -> %s vs. %s -> %s"
             (array_string start_a)
             (array_string end_a)
             (array_string start_b)
             (array_string end_b)
             ()
    ;;

    let transfer (t_a, t_b) =
      let dst_a = This_queue.create () in
      let dst_b = That_queue.create () in
      (* sometimes puts some elements in the destination queues *)
      if Random.bool ()
      then begin
        List.iter [ 1; 2; 3; 4; 5 ] ~f:(fun elem ->
          This_queue.enqueue dst_a elem;
          That_queue.enqueue dst_b elem);
      end;
      let start_a = This_queue.to_array t_a in
      let start_b = That_queue.to_array t_b in
      This_queue.transfer ~src:t_a ~dst:dst_a;
      That_queue.transfer ~src:t_b ~dst:dst_b;
      let end_a  = This_queue.to_array t_a in
      let end_b  = That_queue.to_array t_b in
      let end_a' = This_queue.to_array dst_a in
      let end_b' = That_queue.to_array dst_b in
      if end_a' <> end_b' || end_a <> end_b
      then failwithf "error in transfer: %s -> (%s, %s) vs. %s -> (%s, %s)"
             (array_string start_a)
             (array_string end_a)
             (array_string end_a')
             (array_string start_b)
             (array_string end_b)
             (array_string end_b)
             ()
    ;;

    let fold_check (t_a, t_b) =
      let make_list fold t =
        fold t ~init:[] ~f:(fun acc x -> x :: acc)
      in
      let this_l = make_list This_queue.fold t_a in
      let that_l = make_list That_queue.fold t_b in
      if this_l <> that_l
      then failwithf "error in fold:  %s (from %s) <> %s (from %s)"
             (Sexp.to_string (this_l |> [%sexp_of: int list]))
             (this_to_string t_a)
             (Sexp.to_string (that_l |> [%sexp_of: int list]))
             (that_to_string t_b)
             ()
    ;;

    let length_check (t_a, t_b) =
      let this_len = This_queue.length t_a in
      let that_len = That_queue.length t_b in
      if this_len <> that_len
      then failwithf "error in length: %i (for %s) <> %i (for %s)"
             this_len (this_to_string t_a)
             that_len (that_to_string t_b)
             ()
    ;;

    let%test_unit _ =
      let t = create () in
      let rec loop ~all_ops ~non_empty_ops =
        if all_ops <= 0 && non_empty_ops <= 0
        then begin
          let (t_a, t_b) = t in
          let arr_a = This_queue.to_array t_a in
          let arr_b = That_queue.to_array t_b in
          if arr_a <> arr_b
          then failwithf "queue final states not equal: %s vs. %s"
                 (array_string arr_a)
                 (array_string arr_b)
                 ()
        end else begin
          let queue_was_empty = This_queue.length (fst t) = 0 in
          let r = Random.int 160 in
          begin
            if r < 60
            then enqueue t (Random.int 10_000)
            else if r < 65
            then dequeue t
            else if r < 70
            then clear t
            else if r < 80
            then iter t
            else if r < 90
            then fold_check t
            else if r < 100
            then filter t
            else if r < 110
            then concat_map t
            else if r < 120
            then transfer t
            else if r < 130
            then filter_map t
            else if r < 140
            then copy t
            else if r < 150
            then filter_inplace t
            else length_check t
          end;
          loop
            ~all_ops:(all_ops - 1)
            ~non_empty_ops:(if queue_was_empty then non_empty_ops else non_empty_ops - 1)
        end
      in
      loop ~all_ops:7_500 ~non_empty_ops:5_000
    ;;
  end)

  let binary_search = binary_search
  let binary_search_segmented = binary_search_segmented

  let%test_unit "modification-during-iteration" =
    let x = `A 0 in
    let t = of_list [x; x] in
    let f (`A n) = ignore n; clear t in
    assert (does_raise (fun () -> iter t ~f))
  ;;

  let%test_unit "more-modification-during-iteration" =
    let nested_iter_okay = ref false in
    let t = of_list [ `iter; `clear ] in
    assert (does_raise (fun () ->
      iter t ~f:(function
        | `iter -> iter t ~f:ignore; nested_iter_okay := true
        | `clear -> clear t)));
    assert !nested_iter_okay
  ;;

  let%test_unit "modification-during-filter" =
    let reached_unreachable = ref false in
    let t = of_list [`clear; `unreachable] in
    let f x =
      match x with
      | `clear -> clear t; false
      | `unreachable -> reached_unreachable := true; false
    in
    assert (does_raise (fun () -> filter t ~f));
    assert (not !reached_unreachable)
  ;;

  let%test_unit "modification-during-filter-inplace" =
    let reached_unreachable = ref false in
    let t = of_list [`drop_this; `enqueue_new_element; `unreachable] in
    let f x =
      begin match x with
      | `drop_this | `new_element -> ()
      | `enqueue_new_element -> enqueue t `new_element
      | `unreachable -> reached_unreachable := true
      end;
      false
    in
    assert (does_raise (fun () -> filter_inplace t ~f));
    (* even though we said to drop the first element, the aborted call to [filter_inplace]
       shouldn't have made that change *)
    assert (peek_exn t = `drop_this);
    assert (not !reached_unreachable)
  ;;

  let%test_unit "filter-inplace-during-iteration" =
    let reached_unreachable = ref false in
    let t = of_list [`filter_inplace; `unreachable] in
    let f x =
      match x with
      | `filter_inplace -> filter_inplace t ~f:(fun _ -> false)
      | `unreachable -> reached_unreachable := true
    in
    assert (does_raise (fun () -> iter t ~f));
    assert (not !reached_unreachable)
  ;;
end
(** This signature is here to remind us to update the unit tests whenever we change
    [Core_queue]. *)
: module type of Core_queue))
