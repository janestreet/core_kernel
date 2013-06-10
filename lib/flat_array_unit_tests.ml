open Std_internal
open Import

(* module Flat_array = Flat_array_debug.Debug (Flat_array) *)
open Flat_array

module Slots = Slots
module Slot = Slot

type nonrec 'slots t = 'slots t with sexp_of

let invariant = invariant

let create = create

TEST_UNIT =
  List.iter [ Int.min_value; -1 ] ~f:(fun len ->
    assert (does_fail (fun () -> create Slots.t1 ~len ())))
;;

let length = length

TEST_UNIT =
  List.iter [ 0; 1; 2; 100; 1_000 ] ~f:(fun len ->
    assert (length (create Slots.t1 ~len ()) = len));
;;

let copy       = copy
let get        = get
let unsafe_get = unsafe_get
let set        = set
let unsafe_set = unsafe_set
let get_tuple  = get_tuple

TEST_UNIT =
  let check (type tuple) (type variant) (type a)
        (init : a)
        (changed : a)
        (slots : (tuple, variant) Slots.t)
        (slot_list : (variant, a) Slot.t list)
        (make_tuple : a -> tuple) : unit =
    for len = 0 to 3 do
      let tuple = make_tuple init in
      let t = create slots ~len tuple in
      let t_copy = copy t in
      assert (length t_copy = len);
      for i = 0 to len - 1 do
        assert (Poly.equal (get_tuple t_copy i) tuple);
        List.iter slot_list ~f:(fun slot -> set t_copy i slot changed);
      done;
      (* Make sure changing [t_copy] didn't change [t]. *)
      for i = 0 to len - 1 do
        assert (Poly.equal (get_tuple t i) tuple);
      done;
      (* Ensure invalid indices fail. *)
      List.iter [ -1; len ] ~f:(fun i ->
        assert (does_fail (fun () -> get_tuple t i));
        List.iter slot_list ~f:(fun slot ->
          assert (does_fail (fun () -> get t i slot))));
      List.iter
        [ get, set;
          unsafe_get, unsafe_set;
        ]
        ~f:(fun (get, set) ->
          for i = 0 to len - 1 do
            assert (Poly.equal (get_tuple t i) tuple);
            List.iter slot_list ~f:(fun changed_slot ->
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
  check Slots.t9 (slots9 ()) (fun i -> (i, i, i, i, i, i, i, i, i));
;;
