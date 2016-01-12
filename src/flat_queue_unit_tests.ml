open! Std_internal
open! Int.Replace_polymorphic_compare

let eprints = Debug.eprints

module Flat_queue = Flat_queue_debug.Debug (Flat_queue)
let () = Flat_queue.show_messages := false
open Flat_queue

module Slots = Slots
module Slot  = Slot

let does_raise = Exn.does_raise

type nonrec 'slots t = 'slots t [@@deriving sexp_of]

let invariant = invariant

let capacity      = capacity
let clear         = clear
let create        = create
let drop_front    = drop_front
let enqueue1      = enqueue1
let enqueue2      = enqueue2
let enqueue3      = enqueue3
let enqueue4      = enqueue4
let enqueue5      = enqueue5
let enqueue6      = enqueue6
let enqueue7      = enqueue7
let enqueue8      = enqueue8
let enqueue9      = enqueue9
let fold          = fold
let get           = get
let get_all_slots = get_all_slots
let is_empty      = is_empty
let iter          = iter
let length        = length
let set           = set
let set_all_slots = set_all_slots
let set_capacity  = set_capacity
let unsafe_get    = unsafe_get
let unsafe_set    = unsafe_set

let%test_unit _ =
  let num_enqueues = 5 in
  let check (type a) (type tuple) (type variant)
        (init : a)
        (changed : a)
        (slots : (tuple, variant) Slots.t)
        (all_slots : (variant, a) Slot.t list)
        (make_tuple : a -> tuple)
        (_sexp_of_tuple : tuple -> Sexp.t)
        (enqueue : (tuple, variant) Slots.t t -> a -> unit) : unit =
    if false then eprints "testing" slots [%sexp_of: (_, _) Slots.t];
    let changed_tuple = make_tuple changed in
    let init_tuple = make_tuple init in
    let t = create slots in
    let index_is_valid i =
      List.for_all all_slots ~f:(fun slot ->
        let b1 = is_ok (Result.try_with (fun () -> ignore (get t i slot : a))) in
        let b2 = is_ok (Result.try_with (fun () -> set t i slot changed)) in
        let b3 = is_ok (Result.try_with (fun () -> ignore (get_all_slots t i : tuple))) in
        assert (Bool.equal b1 b2 && Bool.equal b2 b3);
        b1)
    in
    let behaves_like_it_is_empty t =
      length t = 0
      && is_empty t
      && does_raise (fun () -> drop_front t)
      && not (index_is_valid (-1))
      && not (index_is_valid 0)
      && (iter t ~f:(fun _ -> assert false); true)
      && fold t ~init:true ~f:(fun _ _ -> false)
    in
    List.iter [ Int.min_value; -1; 0 ] ~f:(fun capacity ->
      assert (does_raise (fun () -> create ~capacity slots)));
    List.iter [ 1; 100 ] ~f:(fun capacity ->
      let t = create ~capacity slots in
      assert (Flat_queue.capacity t >= capacity);
      List.iter [ Int.min_value; -1; 0 ] ~f:(fun capacity ->
        assert (does_raise (fun () -> set_capacity t capacity)));
      List.iter [ 1; 2; 100 ] ~f:(fun capacity ->
        set_capacity t capacity;
        assert (Flat_queue.capacity t >= capacity));
      for _ = 1 to 10 do
        enqueue t changed;
      done;
      List.iter [ Int.min_value; -1; 0; length t - 1 ] ~f:(fun capacity ->
        set_capacity t capacity);
      set_capacity t (length t));
    assert (behaves_like_it_is_empty t);
    for i = 1 to num_enqueues do
      enqueue t changed;
      assert (length t = i);
      assert (not (is_empty t));
      assert (not (index_is_valid (-1)));
      for index = 0 to i - 1 do
        assert (index_is_valid index);
      done;
      assert (not (index_is_valid i));
    done;
    assert (num_enqueues
            = fold t ~init:0 ~f:(fun i tuple ->
              assert (Poly.equal tuple changed_tuple);
              i + 1));
    let () =
      let r = ref 0 in
      iter t ~f:(fun tuple -> incr r; assert (Poly.equal tuple changed_tuple));
      assert (!r = num_enqueues);
    in
    for i = 0 to num_enqueues - 1 do
      assert (Poly.equal (get_all_slots t i) changed_tuple);
      set_all_slots t i init_tuple;
      assert (Poly.equal (get_all_slots t i) init_tuple);
      set_all_slots t i changed_tuple;
      List.iter all_slots ~f:(fun slot ->
        assert (Poly.equal (get t i slot) changed);
        assert (Poly.equal (unsafe_get t i slot) changed);
        set t i slot init;
        assert (Poly.equal (get t i slot) init);
        assert (Poly.equal (unsafe_get t i slot) init));
    done;
    for i = 1 to num_enqueues do
      drop_front t;
      assert (length t = num_enqueues - i);
    done;
    assert (behaves_like_it_is_empty t);
    (* Check the circular buffer. *)
    for _ = 1 to 2 do
      enqueue t changed;
    done;
    for _ = 1 to 100 do
      enqueue t changed;
      drop_front t;
      assert (length t = 2);
    done;
    for _ = 1 to 2 do
      drop_front t;
    done;
    assert (behaves_like_it_is_empty t);
    (* Check [clear]. *)
    for i = 0 to 5 do
      for _ = 1 to i do
        enqueue t changed;
      done;
      clear t;
      assert (behaves_like_it_is_empty t);
    done;
    (* Check growing with the front at various places. *)
    for shift = 0 to 3 do
      let t = create slots in
      for _ = 1 to 3 do
        enqueue t changed;
      done;
      for _ = 1 to shift do
        enqueue t changed;
        drop_front t;
      done;
      for _ = 1 to 4 do
        enqueue t changed;
      done;
      for i = 0 to 6 do
        assert (Poly.equal (get_all_slots t i) changed_tuple);
      done;
      assert (not (index_is_valid 7));
    done
  in
  let check z = check 13 17 z in
  let slots0 () = [] in
  let slots1 () = slots0 () @ [ Slot.t0 ] in
  let slots2 () = slots1 () @ [ Slot.t1 ] in
  let slots3 () = slots2 () @ [ Slot.t2 ] in
  let slots4 () = slots3 () @ [ Slot.t3 ] in
  let slots5 () = slots4 () @ [ Slot.t4 ] in
  let slots6 () = slots5 () @ [ Slot.t5 ] in
  let slots7 () = slots6 () @ [ Slot.t6 ] in
  let slots8 () = slots7 () @ [ Slot.t7 ] in
  let slots9 () = slots8 () @ [ Slot.t8 ] in
  check Slots.t1 (slots1 ()) (fun i -> i)
    [%sexp_of: int]
    (fun t a -> enqueue1 t a);
  check Slots.t2 (slots2 ()) (fun i -> (i, i))
    [%sexp_of: int * int]
    (fun t a -> enqueue2 t a a);
  check Slots.t3 (slots3 ()) (fun i -> (i, i, i))
    [%sexp_of: int * int * int]
    (fun t a -> enqueue3 t a a a);
  check Slots.t4 (slots4 ()) (fun i -> (i, i, i, i))
    [%sexp_of: int * int * int * int]
    (fun t a -> enqueue4 t a a a a);
  check Slots.t5 (slots5 ()) (fun i -> (i, i, i, i, i))
    [%sexp_of: int * int * int * int * int]
    (fun t a -> enqueue5 t a a a a a);
  check Slots.t6 (slots6 ()) (fun i -> (i, i, i, i, i, i))
    [%sexp_of: int * int * int * int * int * int]
    (fun t a -> enqueue6 t a a a a a a);
  check Slots.t7 (slots7 ()) (fun i -> (i, i, i, i, i, i, i))
    [%sexp_of: int * int * int * int * int * int * int]
    (fun t a -> enqueue7 t a a a a a a a);
  check Slots.t8 (slots8 ()) (fun i -> (i, i, i, i, i, i, i, i))
    [%sexp_of: int * int * int * int * int * int * int * int]
    (fun t a -> enqueue8 t a a a a a a a a);
  check Slots.t9 (slots9 ()) (fun i -> (i, i, i, i, i, i, i, i, i))
    [%sexp_of: int * int * int * int * int * int * int * int * int]
    (fun t a -> enqueue9 t a a a a a a a a a)
;;

let%test_unit _ = (* mutation during [fold] *)
  let t = create Slots.t1 in
  enqueue1 t ();
  assert (does_raise (fun () -> fold t ~init:() ~f:(fun () () -> enqueue1 t ())))
;;

let%test_unit _ = (* mutation during [iter] *)
  let t = create Slots.t1 in
  enqueue1 t ();
  assert (does_raise (fun () -> iter t ~f:(fun () -> enqueue1 t ())))
;;

(* Compare [Flat_queue] with [Core_queue]. *)
let%test_module _ = (module struct
  module type Queue = sig
    type 'a t [@@deriving sexp_of]

    val create : unit -> _ t
    val enqueue : 'a t -> 'a -> unit
    val dequeue : 'a t -> 'a option
    val to_list : 'a t -> 'a list
  end

  module This_queue : Queue = struct
    type 'a t = 'a Slots.t1 Flat_queue.t [@@deriving sexp_of]

    let create () = Flat_queue.create Slots.t1

    let enqueue t a = Flat_queue.enqueue1 t a

    let dequeue t =
      if is_empty t
      then None
      else
        let a = Flat_queue.get t 0 Slot.t0 in
        Flat_queue.drop_front t;
        Some a
    ;;

    let to_list t =
      List.init (Flat_queue.length t) ~f:(fun i -> Flat_queue.get t i Slot.t0)
    ;;
  end

  module That_queue : Queue = struct
    include Core_queue

    (* [Core_queue.create] takes an optional argument, so this is necessary *)
    let create () = create ()
  end

  module Queue : sig
    include Queue
    val create : (module Queue) -> _ t
  end = struct
    module type Q = sig
      include Queue
      type a
      val t : a t
    end

    type 'a t = (module Q with type a = 'a)

    let sexp_of_t (type a) sexp_of_a (t : a t) =
      let module Q = (val t) in
      [%sexp_of: a Q.t] Q.t
    ;;

    let create (type a) (q : (module Queue)) : a t =
      let module Q = (val q) in
      (module struct
         include Q
         type nonrec a = a
         let t = Q.create ()
       end)
    ;;

    let enqueue (type a) (t : a t) a = let module Q = (val t) in Q.enqueue Q.t a
    let dequeue (type a) (t : a t)   = let module Q = (val t) in Q.dequeue Q.t
    let to_list (type a) (t : a t)   = let module Q = (val t) in Q.to_list Q.t
  end

  let all_are_equal l ~equal =
    match l with
    | [] -> true
    | a1 :: ls ->
      with_return (fun r ->
        let rec loop a1 ls =
          match ls with
          | [] -> ()
          | a2 :: ls ->
            if not (equal a1 a2) then r.return false;
            loop a2 ls
        in
        loop a1 ls;
        true)
  ;;

  module Here = Source_code_position

  module type Elt = sig
    type t [@@deriving sexp_of]
    val equal : t -> t -> bool
  end

  module Queues : sig
    type 'a t [@@deriving sexp_of]
    val create
      :  (module Elt with type t = 'a)
      -> (module Queue) list
      -> 'a t
    val enqueue : 'a t -> 'a -> unit
    val dequeue : 'a t -> 'a option
  end = struct

    type 'a t =
      { elt    : (module Elt with type t = 'a)
      ; queues : 'a Queue.t list;
      }

    let equal_a   (type a) (t : a t) = let module Elt = (val t.elt) in Elt.equal
    let sexp_of_a (type a) (t : a t) = let module Elt = (val t.elt) in Elt.sexp_of_t

    let sexp_of_t sexp_of_a t = [%sexp_of: a Queue.t list] t.queues

    let ensure_consistent here t =
      if not (all_are_equal (List.map t.queues ~f:Queue.to_list)
                ~equal:(fun l1 l2 -> List.equal l1 l2 ~equal:(equal_a t)))
      then
        let sexp_of_a = sexp_of_a t in
        failwiths "inconsistent queues" (here, t) [%sexp_of: Here.t * a t]
    ;;

    let create elt queues =
      if List.is_empty queues then failwiths "empty queues" [%here] [%sexp_of: Here.t];
      let t =
        { elt
        ; queues = List.map queues ~f:Queue.create
        }
      in
      ensure_consistent [%here] t;
      t
    ;;

    let enqueue t a =
      List.iter t.queues ~f:(fun q -> Queue.enqueue q a);
      ensure_consistent [%here] t;
    ;;

    let dequeue t =
      let as_ = List.map t.queues ~f:(fun q -> Queue.dequeue q) in
      if not (all_are_equal as_ ~equal:(Option.equal (equal_a t)))
      then begin
        let sexp_of_a = sexp_of_a t in
        failwiths "dequeue inconsistency" ([%here], t, as_)
          [%sexp_of: Here.t * a t * a option list];
      end;
      ensure_consistent [%here] t;
      List.hd_exn as_;
    ;;
  end

  let%test_unit _ =
    let random_state = Random.State.make [||] in
    let qs =
      Queues.create (module Int) [ (module This_queue); (module That_queue)]
    in
    let r = ref 0 in
    for _ = 1 to 1_000 do
      let op = Random.State.int random_state 10 in
      if op < 7
      then (incr r; Queues.enqueue qs !r)
      else ignore (Queues.dequeue qs : int option);
    done
  ;;
end)
