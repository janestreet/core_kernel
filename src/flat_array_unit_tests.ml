open! Std_internal
open! Int.Replace_polymorphic_compare

(* module Flat_array = Flat_array_debug.Debug (Flat_array) *)
open Flat_array

module Slots = Slots
module Slot = Slot

let does_raise = Exn.does_raise

type nonrec 'slots t = 'slots t [@@deriving sexp_of]

let invariant = invariant

let create = create

let%test_unit _ =
  List.iter [ Int.min_value; -1 ] ~f:(fun len ->
    assert (does_raise (fun () -> create Slots.t1 ~len ())))
;;

let length = length

let%test_unit _ =
  List.iter [ 0; 1; 2; 100; 1_000 ] ~f:(fun len ->
    assert (length (create Slots.t1 ~len ()) = len))
;;

let copy          = copy
let get           = get
let get_all_slots = get_all_slots
let is_init       = is_init
let set           = set
let set_all_slots = set_all_slots
let set_to_init   = set_to_init
let slots         = slots
let unsafe_get    = unsafe_get
let unsafe_set    = unsafe_set

let%test_unit _ =
  let check (type tuple) (type variant) (type a)
        (init : a)
        (changed : a)
        (slots : (tuple, variant) Slots.t)
        (slot_list : (variant, a) Slot.t list)
        (make_tuple : a -> tuple) : unit =
    for len = 0 to 3 do
      let tuple = make_tuple init in
      let changed_tuple = make_tuple changed in
      let t = create slots ~len tuple in
      for i = 0 to len - 1 do
        assert (is_init t i);
      done;
      assert (phys_equal slots (Flat_array.slots t));
      let t_copy = copy t in
      assert (length t_copy = len);
      for i = 0 to len - 1 do
        assert (Poly.equal (get_all_slots t_copy i) tuple);
        List.iter slot_list ~f:(fun slot -> set t_copy i slot changed);
        assert (not (is_init t_copy i));
      done;
      (* Make sure changing [t_copy] didn't change [t]. *)
      for i = 0 to len - 1 do
        assert (Poly.equal (get_all_slots t i) tuple);
      done;
      (* Ensure invalid indices fail. *)
      List.iter [ -1; len ] ~f:(fun i ->
        assert (does_raise (fun () -> is_init t i));
        assert (does_raise (fun () -> set_to_init t i));
        assert (does_raise (fun () -> get_all_slots t i));
        List.iter slot_list ~f:(fun slot ->
          assert (does_raise (fun () -> get t i slot))));
      List.iter
        [ get, set;
          unsafe_get, unsafe_set;
        ]
        ~f:(fun (get, set) ->
          for i = 0 to len - 1 do
            assert (Poly.equal (get_all_slots t i) tuple);
            set_all_slots t i changed_tuple;
            assert (Poly.equal (get_all_slots t i) changed_tuple);
            set_all_slots t i tuple;
            List.iter slot_list ~f:(fun changed_slot ->
              set t i changed_slot changed;
              Flat_array.set_to_init t i;
              assert (Poly.equal (get t i changed_slot) init);
              set t i changed_slot changed;
              List.iter slot_list ~f:(fun slot ->
                for j = 0 to len - 1 do
                  assert (Poly.equal
                            (get t j slot)
                            (if i = j && Slot.equal slot changed_slot
                             then changed
                             else init));
                done);
              set t i changed_slot init);
          done);
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
  check Slots.t1 (slots1 ()) Fn.id;
  check Slots.t2 (slots2 ()) (fun i -> (i, i));
  check Slots.t3 (slots3 ()) (fun i -> (i, i, i));
  check Slots.t4 (slots4 ()) (fun i -> (i, i, i, i));
  check Slots.t5 (slots5 ()) (fun i -> (i, i, i, i, i));
  check Slots.t6 (slots6 ()) (fun i -> (i, i, i, i, i, i));
  check Slots.t7 (slots7 ()) (fun i -> (i, i, i, i, i, i, i));
  check Slots.t8 (slots8 ()) (fun i -> (i, i, i, i, i, i, i, i));
  check Slots.t9 (slots9 ()) (fun i -> (i, i, i, i, i, i, i, i, i))
;;

(* These are unit tested via [Blit.Make_flat] in flat_tuple_array.ml *)
let blit        = blit
let blito       = blito
let sub         = sub
let subo        = subo
let unsafe_blit = unsafe_blit
