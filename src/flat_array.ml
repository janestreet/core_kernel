open! Std_internal
open! Int.Replace_polymorphic_compare

module Slots = Tuple_type.Slots

module Slot = struct
  type ('slots, 'a) t = int [@@deriving sexp_of]

  let equal (t1 : (_, _) t) t2 = t1 = t2

  let t0  = 0
  let t1  = 1
  let t2  = 2
  let t3  = 3
  let t4  = 4
  let t5  = 5
  let t6  = 6
  let t7  = 7
  let t8  = 8
  let t9  = 9
  let t10 = 10
  let t11 = 11
end

let metadata_index = 0

let start_of_tuples_index = 1

module Metadata = struct
  (* We rely on immutability of the metadata in the [copy] function, which shares the
     pointer to the metadata between the original and the copy. *)
  type 'slots t =
    { slots : 'slots
      (* [slots_per_tuple] is number of slots in a tuple as seen by the user; i.e. not
         counting the next-free pointer. *)
    ; slots_per_tuple : int
    ; length : int
      (* [Obj_array.length dummy = slots_per_tuple]. *)
    ; dummy : Obj_array.t sexp_opaque
    }
  [@@deriving fields, sexp_of]

  let array_indices_per_tuple t = t.slots_per_tuple

  let array_length t = start_of_tuples_index + t.length * array_indices_per_tuple t

  let tuple_num_to_first_slot_index t tuple_num =
    start_of_tuples_index + tuple_num * array_indices_per_tuple t
  ;;

  let slot_index t tuple_num slot = tuple_num_to_first_slot_index t tuple_num + slot
end

open Metadata

type 'slots t = Obj_array.t

let metadata (type slots) (t : slots t) =
  (Obj.obj (Obj_array.unsafe_get t metadata_index) : slots Metadata.t)
;;

let length t = (metadata t).length
let slots  t = (metadata t).slots

let sexp_of_t sexp_of_slots t = Metadata.sexp_of_t sexp_of_slots (metadata t)

let invariant slots_invariant t : unit =
  try
    let metadata = metadata t in
    let check f field = f (Field.get field metadata) in
    Metadata.Fields.iter
      ~slots:(check slots_invariant)
      ~slots_per_tuple:(check (fun slots_per_tuple -> assert (slots_per_tuple > 0)))
      ~length:(check (fun length ->
        assert (length >= 0);
        assert (Obj_array.length t = Metadata.array_length metadata)))
      ~dummy:(check (fun dummy ->
        assert (Obj_array.length dummy = metadata.slots_per_tuple)))
  with exn ->
    failwiths "Flat_array.invariant failed" (exn, t) [%sexp_of: exn * _ t]
;;

let set_metadata (type slots) (t : slots t) metadata =
  Obj_array.set t metadata_index (Obj.repr (metadata : slots Metadata.t));
;;

let create_array (type slots) (metadata : slots Metadata.t) : slots t =
  let t = Obj_array.create ~len:(Metadata.array_length metadata) in
  set_metadata t metadata;
  t
;;

let check_index t metadata i =
  if i < 0 || i >= metadata.length
  then failwiths "Flat_array got invalid index" (i, t) [%sexp_of: int * _ t];
;;

let set_to_init t tuple_num =
  let metadata = metadata t in
  check_index t metadata tuple_num;
  Obj_array.blit ~len:metadata.slots_per_tuple
    ~src:metadata.dummy ~src_pos:0
    ~dst:t              ~dst_pos:(tuple_num_to_first_slot_index metadata tuple_num)
;;

let is_init t tuple_num =
  let metadata = metadata t in
  check_index t metadata tuple_num;
  let dummy = metadata.dummy in
  let r = ref true in
  for slot = 0 to metadata.slots_per_tuple - 1 do
    if not (phys_equal
              (Obj_array.get dummy slot)
              (Obj_array.get t (Metadata.slot_index metadata tuple_num slot)))
    then r := false
  done;
  !r
;;

let create (type tuple) (slots : (tuple, _) Slots.t) ~len:length dummy =
  if length < 0
  then failwiths "Flat_array.create got invalid length" length [%sexp_of: int];
  let slots_per_tuple = Slots.slots_per_tuple slots in
  let dummy =
    if slots_per_tuple = 1
    then Obj_array.singleton (Obj.repr (dummy : tuple))
    else (Obj.magic (dummy : tuple) : Obj_array.t)
  in
  let metadata = { Metadata. slots; slots_per_tuple; length; dummy } in
  let t = create_array metadata in
  for tuple_num = 0 to length - 1 do
    set_to_init t tuple_num;
  done;
  t
;;

(* The backing obj_array is copied, including the pointer to the metadata.  We don't have
   to deep copy the metadata because it is immutable. *)
let copy t = Obj_array.copy t

let get (type a) t i (slot : (_, a) Slot.t) =
  let metadata = metadata t in
  check_index t metadata i;
  (Obj.obj (Obj_array.unsafe_get t (Metadata.slot_index metadata i slot)) : a)
;;

let unsafe_get (type a) t i (slot : (_, a) Slot.t) =
  let metadata = metadata t in
  (Obj.obj (Obj_array.unsafe_get t (Metadata.slot_index metadata i slot)) : a)
;;

let set (type a) t i (slot : (_, a) Slot.t) a =
  let metadata = metadata t in
  check_index t metadata i;
  Obj_array.unsafe_set t (Metadata.slot_index metadata i slot) (Obj.repr (a : a))
;;

let unsafe_set (type a) t i (slot : (_, a) Slot.t) a =
  let metadata = metadata t in
  Obj_array.unsafe_set t (Metadata.slot_index metadata i slot) (Obj.repr (a : a))
;;

let get_all_slots (type tuple) (t : (tuple, _) Slots.t t) i =
  let metadata = metadata t in
  check_index t metadata i;
  let len = metadata.slots_per_tuple in
  if len = 1
  then unsafe_get t i Slot.t0
  else
    let obj =
      Obj_array.sub t ~pos:(Metadata.tuple_num_to_first_slot_index metadata i) ~len
    in
    (Obj.magic (obj : Obj_array.t) : tuple)
;;

let set_all_slots (type tuple) (t : (tuple, _) Slots.t t) i tuple =
  let metadata = metadata t in
  check_index t metadata i;
  let len = metadata.slots_per_tuple in
  if len = 1
  then unsafe_set t i Slot.t0 tuple
  else Obj_array.blit
         ~src:(Obj.magic (tuple : tuple) : Obj_array.t) ~src_pos:0
         ~dst:t ~dst_pos:(Metadata.tuple_num_to_first_slot_index metadata i)
         ~len
;;

let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  let m = metadata src in
  Obj_array.blit
    ~src ~src_pos:(Metadata.tuple_num_to_first_slot_index m src_pos)
    ~dst ~dst_pos:(Metadata.tuple_num_to_first_slot_index (metadata dst) dst_pos)
    ~len:(len * m.slots_per_tuple)
;;

include
  Blit.Make1
    (struct
      module Slots = Slots
      type nonrec 'a t = 'a t [@@deriving sexp_of]
      type 'a z = 'a Slots.t1 t
      let length      = length
      let get         = get_all_slots
      let set         = set_all_slots
      let unsafe_blit = unsafe_blit
      let create_like ~len t =
        let metadata = metadata t in
        create metadata.slots ~len metadata.dummy
      ;;
      let create_bool ~len = create Slots.t1 ~len false
    end)
;;
