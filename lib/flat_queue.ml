open Std_internal
open Import  let _ = _squelch_unused_module_warning_

module A = Flat_array

module Slots = A.Slots
module Slot  = A.Slot

type 'slots t =
  { mutable elements : 'slots Flat_array.t;
    (* [create_elements] is used when we need to create a new [elements] array when we
       shrink or grow the queue.  We store it as a function that is closed over the [init]
       value, because we cannot store the [init] value for the array in [t], because we
       cannot write down its type in terms of ['slots]. *)
    create_elements : len:int -> 'slots Flat_array.t;
    (* [mask = A.length elements - 1].  Having it makes it quick to go from a queue index
       to an array index -- since the array's length is a power of 2, [index land mask]
       equals [index mod A.length elements]. *)
    mutable mask : int;
    (* [front] is the index of the first element in the queue. *)
    mutable front : int;
    mutable length : int;
  }
with fields, sexp_of

let capacity t = A.length t.elements

let offset t i = (t.front + i) land t.mask

let invariant slots_invariant t =
  Invariant.invariant _here_ t <:sexp_of< _ t >> (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~elements:(check (fun elements ->
        assert (Int.is_pow2 (capacity t));
        A.invariant slots_invariant elements;
        for i = length t to capacity t - 1 do
          assert (A.is_init elements (offset t i));
        done))
      ~create_elements:ignore
      ~mask:(check (fun mask -> assert (mask = capacity t - 1)))
      ~front:(check (fun front ->
        assert (front >= 0);
        assert (front < capacity t)))
      ~length:(check (fun length ->
        assert (length >= 0);
        assert (length <= capacity t))))
;;

let create
      (type tuple) (type variant)
      ?capacity
      (slots : (tuple, variant) Slots.t)
  : (tuple, variant) Slots.t t =
  let slots_per_tuple = Slots.slots_per_tuple slots in
  let init : tuple =
    (* The uses of [Obj.magic] are to create a dummy value that will be stored in the
       slots of array elements that don't currently hold queue elements.  It is a bug (of
       the segfault variety) in [Flat_queue] if this dummy value is ever exposed to user
       code. *)
    if slots_per_tuple = 1
    then (Obj.magic 0 : tuple)
    else (Obj.magic (Obj_array.create ~len:slots_per_tuple) : tuple)
  in
  let capacity =
    match capacity with
    | None -> 1
    | Some capacity ->
      if capacity <= 0 then
        failwiths "Flat_queue.create got nonpositive capacity" capacity <:sexp_of< int >>
      else
        Int.ceil_pow2 capacity
  in
  let create_elements ~len : (tuple, variant) Slots.t A.t = A.create slots ~len init in
  { create_elements;
    mask     = capacity - 1;
    elements = create_elements ~len:capacity;
    front    = 0;
    length   = 0;
  }
;;

let is_empty t = length t = 0

let is_full t = length t = capacity t

let drop_front ?(n = 1) t =
  if n < 0 || n > length t then
    failwiths "Flat_queue.drop_front got invalid n" (n, t) <:sexp_of< int * _ t >>;
  for _i = 1 to n do
    A.set_to_init t.elements t.front;
    t.front <- offset t 1;
    t.length <- t.length - 1;
  done;
;;

let clear t =
  for i = 0 to length t - 1 do
    A.set_to_init t.elements (offset t i);
  done;
  t.length <- 0;
  t.front <- 0;
;;

let unsafe_get t i slot   = A.unsafe_get t.elements (offset t i) slot
let unsafe_set t i slot a = A.unsafe_set t.elements (offset t i) slot a

let check_index t i =
  if i < 0 || i >= t.length then
    failwiths "invalid index in Flat_queue" (i, t) <:sexp_of< int * _ t >>;
;;

let get t i slot   = check_index t i; unsafe_get t i slot
let set t i slot a = check_index t i; unsafe_set t i slot a

let get_all_slots t i       = check_index t i; A.get_all_slots t.elements (offset t i)
let set_all_slots t i tuple = check_index t i; A.set_all_slots t.elements (offset t i) tuple

let resize t ~capacity =
  let old_elements = t.elements in
  let old_front = t.front in
  t.mask <- capacity - 1;
  t.elements <- t.create_elements ~len:capacity;
  let len1 = Int.min t.length (A.length old_elements - old_front) in
  let len2 = t.length - len1 in
  A.blit ~len:len1
    ~src:old_elements ~src_pos:old_front
    ~dst:t.elements   ~dst_pos:0;
  A.blit ~len:len2
    ~src:old_elements ~src_pos:0
    ~dst:t.elements   ~dst_pos:len1;
  t.front <- 0;
;;

let maybe_grow t = if is_full t then resize t ~capacity:(capacity t * 2)

let set_capacity t new_capacity =
  if new_capacity < length t then
    failwiths "Flat_queue.set_capacity got capacity smaller than length"
      (new_capacity, length t) <:sexp_of< int * int >>;
  let new_capacity = Int.ceil_pow2 new_capacity in
  if new_capacity <> capacity t then resize t ~capacity:new_capacity;
;;

let enqueue1 t a0 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  t.length <- t.length + 1;
;;

let enqueue2 t a0 a1 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  t.length <- t.length + 1;
;;

let enqueue3 t a0 a1 a2 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  t.length <- t.length + 1;
;;

let enqueue4 t a0 a1 a2 a3 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  A.unsafe_set elements i Slot.t3 a3;
  t.length <- t.length + 1;
;;

let enqueue5 t a0 a1 a2 a3 a4 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  A.unsafe_set elements i Slot.t3 a3;
  A.unsafe_set elements i Slot.t4 a4;
  t.length <- t.length + 1;
;;

let enqueue6 t a0 a1 a2 a3 a4 a5 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  A.unsafe_set elements i Slot.t3 a3;
  A.unsafe_set elements i Slot.t4 a4;
  A.unsafe_set elements i Slot.t5 a5;
  t.length <- t.length + 1;
;;

let enqueue7 t a0 a1 a2 a3 a4 a5 a6 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  A.unsafe_set elements i Slot.t3 a3;
  A.unsafe_set elements i Slot.t4 a4;
  A.unsafe_set elements i Slot.t5 a5;
  A.unsafe_set elements i Slot.t6 a6;
  t.length <- t.length + 1;
;;

let enqueue8 t a0 a1 a2 a3 a4 a5 a6 a7 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  A.unsafe_set elements i Slot.t3 a3;
  A.unsafe_set elements i Slot.t4 a4;
  A.unsafe_set elements i Slot.t5 a5;
  A.unsafe_set elements i Slot.t6 a6;
  A.unsafe_set elements i Slot.t7 a7;
  t.length <- t.length + 1;
;;

let enqueue9 t a0 a1 a2 a3 a4 a5 a6 a7 a8 =
  maybe_grow t;
  let elements = t.elements in
  let i = offset t t.length in
  A.unsafe_set elements i Slot.t0 a0;
  A.unsafe_set elements i Slot.t1 a1;
  A.unsafe_set elements i Slot.t2 a2;
  A.unsafe_set elements i Slot.t3 a3;
  A.unsafe_set elements i Slot.t4 a4;
  A.unsafe_set elements i Slot.t5 a5;
  A.unsafe_set elements i Slot.t6 a6;
  A.unsafe_set elements i Slot.t7 a7;
  A.unsafe_set elements i Slot.t8 a8;
  t.length <- t.length + 1;
;;

let fold t ~init ~f =
  let r = ref init in
  for i = 0 to length t - 1 do
    r := f !r (get_all_slots t i);
  done;
  !r
;;

let iter t ~f =
  for i = 0 to length t - 1 do
    f (get_all_slots t i);
  done;
;;
