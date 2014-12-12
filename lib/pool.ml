INCLUDE "config.mlh"

open Int_replace_polymorphic_compare
open Sexplib.Conv
open Bin_prot.Std
open Pool_intf

let failwiths = Error.failwiths
let phys_equal = Caml.(==)

module Sexp = Sexplib.Sexp
module List = Core_list
module Field = Core_field
module Array = Core_array

module Int = struct
  let num_bits = Word_size.num_bits Word_size.word_size - 1
  let max_value = Pervasives.max_int
  let to_string = string_of_int
end

(* dynamically choose Int63 implementation based on int size *)
module type Int63 = sig
  type t with bin_io, sexp
  val of_int : int -> t
  val to_int_exn : t -> int
end
module Int63_64 = struct
  type t = int with bin_io, sexp
  let of_int i = i
  let to_int_exn i = i
end
module Int63_32 = struct
  type t = int64 with bin_io, sexp
  let of_int = Int_conversions.int_to_int64
  let to_int_exn = Int_conversions.int64_to_int_exn
end
module Int63 = 
  (val (if Int.num_bits = 63 then (module Int63_64 : Int63) else (module Int63_32 : Int63)))

let sprintf = Core_printf.sprintf

let concat l = Caml.StringLabels.concat ~sep:"" l

module type S = S

module Pool = struct

  let grow_capacity ~capacity ~old_capacity =
    match capacity with
    | None -> if old_capacity = 0 then 1 else old_capacity * 2
    | Some capacity ->
      if capacity <= old_capacity then
        failwiths "Pool.grow got too small capacity"
          (`capacity capacity, `old_capacity old_capacity)
          (<:sexp_of< [ `capacity of int ] * [ `old_capacity of int ] >>);
      capacity
  ;;

  module Slots = Tuple_type.Slots

  let max_slot = 12

  (* The pool is represented as a single [Obj_array.t], where index zero has the metadata
     about the pool and the remaining indices are the tuples layed out one after the
     other.  Each tuple takes [1 + slots_per_tuple] indices in the pool, where the first
     index holds a header and the remaining indices hold the tuple's slots:

     {v
     | header | s0 | s1 | ... | s<N-1> |
     v}

     A [Pointer.t] to a tuple contains the integer index where its header is, as well as
     (a mask of) the tuple's unique id.

     The free tuples are singly linked via the headers.

     When a tuple is in use, its header is marked to indicate so, and also to include the
     tuple's unique id.  This allows us to check in constant time whether a pointer is
     valid, by comparing the id in the pointer with the id in the header.

     When a tuple is not in use, its header is part of the free list, and its tuple slots
     have dummy values of the appropriate types, from the [dummy] tuple supplied to
     [create].  We must have dummy values of the correct type to prevent a segfault in
     code that (mistakenly) uses a pointer to a free tuple.

     For [Pool.Unsafe], a slot in a free object is guaranteed to be an int; it must not be
     pointer to prevent a space leak.  However, the int in the slot may not represent a
     valid value of the type.
  *)

  module Slot = struct
    type ('slots, 'a) t = int with sexp_of

    let equal (t1 : (_, _) t) t2 = t1 = t2

    let t0 = 1
    let t1 = 2
    let t2 = 3
    let t3 = 4
    let t4 = 5
    let t5 = 6
    let t6 = 7
    let t7 = 8
    let t8 = 9
    let t9 = 10
    let t10 = 11
    let t11 = 12

    TEST = t11 = max_slot
  end

  (* This code has been changed to accommodate js_of_ocaml.  It previously used 
     'IFDEF ARCH_SIXTYFOUR' to choose these settings and would assert if Int.num_bits
     was not of an expected size.

     NOTE: other ARCH_SIXTYFOUR defines in this file will also need to be changeed 
     to ensure correct operation of the module, but this is enough to get 
     core_kernel to run in js_of_ocaml.
  *)
  let array_index_num_bits, masked_tuple_id_num_bits = 
    if Int.num_bits = 63 then 30, 33
    else 27, 4

  TEST = array_index_num_bits > 0
  TEST = masked_tuple_id_num_bits > 0
  TEST = array_index_num_bits + masked_tuple_id_num_bits <= Int.num_bits

  let max_array_length = 1 lsl array_index_num_bits

  module Tuple_id : sig
    type t = private int with sexp_of

    include Invariant.S with type t := t

    val to_string : t -> string

    val equal : t -> t -> bool

    val init : t
    val next : t -> t

    val of_int : int -> t
    val to_int : t -> int

    val examples : t list
  end = struct
    type t = int with sexp_of

    (* We guarantee that tuple ids are nonnegative so that they can be encoded in
       headers. *)
    let invariant t = assert (t >= 0)

    let to_string = Int.to_string

    let equal (t1 : t) t2 = t1 = t2

    let init = 0

    (* NOTE: this code used 'ARCHSIXTYFOUR' rather than 'ARCH_SIXTYFOUR'
       which means it probably wasn't actually selecting between platforms
       the way that was intended *)
    let next = 
      if Int.num_bits = 63 then (fun t -> t + 1)
      else (fun t -> if t = Int.max_value then 0 else t + 1)

    let to_int t = t

    let of_int i =
      if i < 0 then failwiths "Tuple_id.of_int got negative int" i <:sexp_of< int >>;
      i
    ;;

    let examples = [ 0; 1; 0x1FFF_FFFF; Int.max_value ]
end

  let tuple_id_mask = (1 lsl masked_tuple_id_num_bits) - 1

  module Pointer : sig
    (* [Pointer.t] is an encoding as an [int] of the following sum type:

       {[
         | Null
         | Normal of { header_index : int; masked_tuple_id : int }
       ]}

       The encoding is chosen to optimize the most common operation, namely tuple-slot
       access, the [slot_index] function.  The encoding is designed so that [slot_index]
       produces a negative number for [Null], which will cause the subsequent array bounds
       check to fail. *)
    type 'slots t = private int with sexp_of

    include Invariant.S1 with type 'a t := 'a t

    val phys_compare : 'a t -> 'a t -> int
    val phys_equal   : 'a t -> 'a t -> bool

    (* The null pointer.  [null] is a function due to issues with the value restriction. *)
    val null : unit -> _ t
    val is_null : _ t -> bool

    (* Normal pointers. *)
    val create : header_index:int -> Tuple_id.t -> _ t
    val header_index     : _ t -> int
    val masked_tuple_id  : _ t -> int
    val slot_index       : _ t -> (  _, _) Slot.t -> int
    val first_slot_index : _ t -> int

    module Id : sig
      type t with bin_io, sexp
    end

    val to_id : _ t -> Id.t
    val of_id_exn : Id.t -> _ t

  end = struct
    (* A pointer is either [null] or the (positive) index in the pool of the next-free
       field preceeding the tuple's slots. *)
    type 'slots t = int

    let sexp_of_t _ t = Sexp.Atom (sprintf "<Obj_array.Pointer.t: 0x%08x>" t)

    let phys_equal (t1 : _ t) t2 = phys_equal t1 t2

    let phys_compare = compare

    let null () = -max_slot - 1

    let is_null t = phys_equal t (null ())

    (* [null] must be such that [null + slot] is an invalid array index for all slots.
       Otherwise get/set on the null pointer may lead to a segfault. *)
    TEST = null () + max_slot < 0

    let create ~header_index (tuple_id : Tuple_id.t) =
      header_index
      lor ((Tuple_id.to_int tuple_id land tuple_id_mask) lsl array_index_num_bits)
    ;;

    let header_index_mask = (1 lsl array_index_num_bits) - 1

    let masked_tuple_id t = t lsr array_index_num_bits

    let header_index t = t land header_index_mask

    let invariant _ t =
      if not (is_null t) then assert (header_index t > 0);
    ;;

    TEST_UNIT = invariant ignore (null ())

    TEST_UNIT =
      List.iter Tuple_id.examples ~f:(fun tuple_id ->
        invariant ignore (create ~header_index:1 tuple_id));
    ;;

    let slot_index t slot = header_index t + slot

    let first_slot_index t = slot_index t Slot.t0

    module Id = struct
      type t = Int63.t with bin_io, sexp
    end

    let to_id t = Int63.of_int t

    let of_id_exn id =
      try
        let t = Int63.to_int_exn id in
        if is_null t then
          t
        else
          let should_equal =
            create ~header_index:(header_index t) (Tuple_id.of_int (masked_tuple_id t))
          in
          if phys_equal t should_equal
          then t
          else failwiths "should equal" should_equal <:sexp_of< _ t >>
      with exn ->
        failwiths "Pointer.of_id_exn got strange id" (id, exn) <:sexp_of< Id.t * exn >>
    ;;
  end

  module Header : sig
    (* A [Header.t] is an encoding as an [int] of the following type:

       {[
         | Null
         | Free of { next_free_header_index : int }
         | Used of { tuple_id : int }
       ]}

       If a tuple is free, its header is set to either [Null] or [Free] with
       [next_free_header_index] indicating the header of the next tuple on the free list.
       If a tuple is in use, it header is set to [Used]. *)
    type t = private int with sexp_of

    val null : t
    val is_null : t -> bool

    val free : next_free_header_index:int -> t
    val is_free : t -> bool
    val next_free_header_index : t -> int (* only valid if [is_free t] *)

    val used : Tuple_id.t -> t
    val is_used : t -> bool
    val tuple_id : t -> Tuple_id.t (* only valid if [is_used t] *)
  end = struct

    type t = int

    let null = 0
    let is_null t = t = 0

    (* We know that header indices are [> 0], because index [0] holds the metadata. *)
    let free ~next_free_header_index = next_free_header_index
    let is_free t = t > 0
    let next_free_header_index t = t

    let used (tuple_id : Tuple_id.t) = -1 - (tuple_id :> int)
    let is_used t = t < 0
    let tuple_id t = Tuple_id.of_int (- (t + 1))

    TEST_UNIT =
      List.iter Tuple_id.examples ~f:(fun id ->
        let t = used id in
        assert (is_used t);
        assert (Tuple_id.equal (tuple_id t) id));
    ;;

    let sexp_of_t t =
      if is_null t
      then Sexp.Atom "null"
      else if is_free t
      then Sexp.(List [ Atom "Free"; Atom (Int.to_string (next_free_header_index t)) ])
      else Sexp.(List [ Atom "Used"; Atom (Tuple_id.to_string (tuple_id t)) ])
    ;;

  end

  let metadata_index = 0

  let start_of_tuples_index = 1

  let max_capacity ~slots_per_tuple =
    (max_array_length - start_of_tuples_index) / (1 + slots_per_tuple)
  ;;

  TEST_UNIT =
    for slots_per_tuple = 1 to max_slot do
      assert ((start_of_tuples_index
               + (1 + slots_per_tuple) * max_capacity ~slots_per_tuple)
              <= max_array_length);
    done;
  ;;

  module Metadata = struct
    type 'slots t =
      { (* [slots_per_tuple] is number of slots in a tuple as seen by the user; i.e. not
           counting the next-free pointer. *)
        slots_per_tuple : int;
        capacity : int;
        mutable length : int;
        mutable next_id : Tuple_id.t;
        mutable first_free : Header.t;
        (* [dummy] is [None] in an unsafe pool.  In a safe pool, [dummy] is [Some a], with
           [Obj_array.length a = slots_per_tuple]. *)
        dummy : Obj_array.t sexp_opaque option;
      }
    with fields, sexp_of

    let array_indices_per_tuple t = 1 + t.slots_per_tuple

    let array_length t = start_of_tuples_index + t.capacity * array_indices_per_tuple t

    let header_index_to_tuple_num t ~header_index =
      (header_index - start_of_tuples_index) / array_indices_per_tuple t
    ;;

    let tuple_num_to_header_index t tuple_num =
      start_of_tuples_index + tuple_num * array_indices_per_tuple t
    ;;

    let tuple_num_to_first_slot_index t tuple_num =
      tuple_num_to_header_index t tuple_num + 1
    ;;

    let is_full t = t.length = t.capacity
  end

  open Metadata

  type 'slots t = Obj_array.t

  let metadata (type slots) (t : slots t) =
    (Obj.obj (Obj_array.unsafe_get t metadata_index) : slots Metadata.t)
  ;;

  let length t = (metadata t).length

  let sexp_of_t sexp_of_ty t = Metadata.sexp_of_t sexp_of_ty (metadata t)

  (* Because [unsafe_header] and [unsafe_set_header] do not do a bounds check, one must be
     sure that one has a valid [header_index] before calling them. *)
  let unsafe_header t ~header_index =
    (Obj.obj (Obj_array.unsafe_get t header_index) : Header.t)
  ;;

  let unsafe_set_header t ~header_index (header : Header.t) =
    Obj_array.unsafe_set_int_assuming_currently_int t header_index (header :> int)
  ;;

  let header_index_is_in_bounds t ~header_index =
    header_index >= start_of_tuples_index
    && header_index < Obj_array.length t
  ;;

  let unsafe_pointer_is_live t pointer =
    let header_index = Pointer.header_index pointer in
    let header = unsafe_header t ~header_index in
    Header.is_used header
    && (Tuple_id.to_int (Header.tuple_id header)) land tuple_id_mask
       = Pointer.masked_tuple_id pointer
  ;;

  let pointer_is_valid t pointer =
    header_index_is_in_bounds t ~header_index:(Pointer.header_index pointer)
    (* At this point, we know the pointer isn't [null] and is in bounds, so we know it is
       the index of a header, since we maintain the invariant that all pointers other than
       [null] are. *)
    && unsafe_pointer_is_live t pointer
  ;;

  let id_of_pointer _t pointer = Pointer.to_id pointer

  let is_valid_header_index t ~header_index =
    let metadata = metadata t in
    header_index_is_in_bounds t ~header_index
    && 0 = (header_index - start_of_tuples_index)
           mod (Metadata.array_indices_per_tuple metadata)
  ;;

  let pointer_of_id_exn_is_supported = true

  let pointer_of_id_exn t id =
    try
      let pointer = Pointer.of_id_exn id in
      if not (Pointer.is_null pointer) then begin
        let header_index = Pointer.header_index pointer in
        if not (is_valid_header_index t ~header_index) then
          failwiths "invalid header index" header_index <:sexp_of< int >>;
        if not (unsafe_pointer_is_live t pointer) then failwith "pointer not live";
      end;
      pointer
    with exn ->
      failwiths "Pool.pointer_of_id_exn got invalid id" (id, t, exn)
        <:sexp_of< Pointer.Id.t * _ t * exn >>
  ;;

  let invariant _invariant_a t : unit =
    try
      let metadata = metadata t in
      let check f field = f (Field.get field metadata) in
      Metadata.Fields.iter
        ~slots_per_tuple:(check (fun slots_per_tuple -> assert (slots_per_tuple > 0)))
        ~capacity:(check (fun capacity ->
          assert (capacity >= 0);
          assert (Obj_array.length t = Metadata.array_length metadata)))
        ~length:(check (fun length ->
          assert (length >= 0);
          assert (length <= metadata.capacity)))
        ~next_id:(check Tuple_id.invariant)
        ~first_free:(check (fun first_free ->
          let free = Array.create ~len:metadata.capacity false in
          let r = ref first_free in
          while not (Header.is_null !r) do
            let header = !r in
            assert (Header.is_free header);
            let header_index = Header.next_free_header_index header in
            assert (is_valid_header_index t ~header_index);
            let tuple_num = header_index_to_tuple_num metadata ~header_index in
            if free.(tuple_num) then
              failwiths "cycle in free list" tuple_num <:sexp_of< int >>;
            free.(tuple_num) <- true;
            r := unsafe_header t ~header_index;
          done))
        ~dummy:(check (function
          | Some dummy -> assert (Obj_array.length dummy = metadata.slots_per_tuple)
          | None ->
            for tuple_num = 0 to metadata.capacity - 1 do
              let header_index = tuple_num_to_header_index metadata tuple_num in
              let header = unsafe_header t ~header_index in
              if Header.is_free header then
                let first_slot = tuple_num_to_first_slot_index metadata tuple_num in
                for slot = 0 to metadata.slots_per_tuple - 1 do
                  assert (Obj.is_int (Obj_array.get t (first_slot + slot)));
                done;
            done))
    with exn ->
      failwiths "Pool.invariant failed" (exn, t) (<:sexp_of< exn * _ t >>)
  ;;

  let capacity t = (metadata t).capacity

  let is_full t = Metadata.is_full (metadata t)

  let unsafe_add_to_free_list t metadata ~header_index =
    unsafe_set_header t ~header_index metadata.first_free;
    metadata.first_free <- Header.free ~next_free_header_index:header_index;
  ;;

  let set_metadata (type slots) (t : slots t) metadata =
    Obj_array.set t metadata_index (Obj.repr (metadata : slots Metadata.t));
  ;;

  let create_array (type slots) (metadata : slots Metadata.t) : slots t =
    let t = Obj_array.create ~len:(Metadata.array_length metadata) in
    set_metadata t metadata;
    t
  ;;

  (* Initialize tuples numbered from [lo] (inclusive) up to [hi] (exclusive).  For each
     tuple, this puts dummy values in the tuple's slots and adds the tuple to the free
     list. *)
  let unsafe_init_range t metadata ~lo ~hi =
    begin match metadata.dummy with
    | None -> ()
    | Some dummy ->
      for tuple_num = lo to hi - 1 do
        Obj_array.blit
          ~src:dummy ~src_pos:0
          ~dst:t     ~dst_pos:(tuple_num_to_first_slot_index metadata tuple_num)
          ~len:metadata.slots_per_tuple
      done
    end;
    for tuple_num = hi - 1 downto lo do
      unsafe_add_to_free_list t metadata
        ~header_index:(tuple_num_to_header_index metadata tuple_num);
    done;
  ;;

  let create_with_dummy slots ~capacity ~dummy =
    if capacity < 0 then
      failwiths "Pool.create got invalid capacity" capacity <:sexp_of< int >>;
    let slots_per_tuple = Slots.slots_per_tuple slots in
    let max_capacity = max_capacity ~slots_per_tuple in
    if capacity > max_capacity then
      failwiths "Pool.create got too large capacity" (capacity, `max max_capacity)
        <:sexp_of< int * [ `max of int ] >>;
    let metadata =
      { Metadata.
        slots_per_tuple;
        capacity;
        length = 0;
        next_id = Tuple_id.init;
        first_free = Header.null;
        dummy;
      }
    in
    let t = create_array metadata in
    unsafe_init_range t metadata ~lo:0 ~hi:capacity;
    t
  ;;

  let create (type tuple) (slots : (tuple, _) Slots.t) ~capacity ~dummy =
    let dummy =
      if Slots.slots_per_tuple slots = 1
      then Obj_array.singleton (Obj.repr (dummy : tuple))
      else (Obj.magic (dummy : tuple) : Obj_array.t)
    in
    create_with_dummy slots ~capacity ~dummy:(Some dummy)
  ;;

  (* Purge a pool and make it unusable. *)
  let destroy t =
    let metadata = metadata t in
    let metadata =
      { Metadata.
        slots_per_tuple = metadata.slots_per_tuple;
        capacity = 0;
        length = 0;
        next_id = metadata.next_id;
        first_free = Header.null;
        dummy = metadata.dummy;
      }
    in
    set_metadata t metadata;
    (* We truncate the pool so it holds just its metadata, but uses no space for
       tuples.  This causes all pointers to be invalid. *)
    Obj_array.truncate t ~len:1;
  ;;

  let grow ?capacity t =
    let { Metadata.
          slots_per_tuple;
          capacity = old_capacity;
          length;
          next_id;
          first_free = _;
          dummy } =
      metadata t
    in
    let capacity =
      min (max_capacity ~slots_per_tuple) (grow_capacity ~capacity ~old_capacity)
    in
    if capacity = old_capacity then
      failwiths "Pool.grow cannot grow pool; capacity already at maximum" capacity
        <:sexp_of< int >>;
    let metadata =
      { Metadata.
        slots_per_tuple;
        capacity;
        length;
        next_id;
        first_free = Header.null;
        dummy;
      }
    in
    let t' = create_array metadata in
    Obj_array.blit
      ~src:t  ~src_pos:start_of_tuples_index
      ~dst:t' ~dst_pos:start_of_tuples_index
      ~len:(old_capacity * Metadata.array_indices_per_tuple metadata);
    destroy t;
    unsafe_init_range t' metadata ~lo:old_capacity ~hi:capacity;
    for tuple_num = old_capacity - 1 downto 0 do
      let header_index = tuple_num_to_header_index metadata tuple_num in
      let header = unsafe_header t' ~header_index in
      if not (Header.is_used header) then
        unsafe_add_to_free_list t' metadata ~header_index;
    done;
    t'
  ;;

  let malloc (type slots) (t : slots t) : slots Pointer.t =
    let metadata = metadata t in
    let first_free = metadata.first_free in
    if Header.is_null first_free then
      failwiths "Pool.malloc of full pool" t <:sexp_of< _ t >>;
    let header_index = Header.next_free_header_index first_free in
    metadata.first_free <- unsafe_header t ~header_index;
    metadata.length <- metadata.length + 1;
    let tuple_id = metadata.next_id in
    unsafe_set_header t ~header_index (Header.used tuple_id);
    metadata.next_id <- Tuple_id.next tuple_id;
    Pointer.create ~header_index tuple_id;
  ;;

  let unsafe_free (type slots) (t : slots t) (pointer : slots Pointer.t) =
    let metadata = metadata t in
    metadata.length <- metadata.length - 1;
    unsafe_add_to_free_list t metadata ~header_index:(Pointer.header_index pointer);
    begin match metadata.dummy with
    | None ->
      let pos = Pointer.first_slot_index pointer in
      for i = 0 to metadata.slots_per_tuple - 1 do
        Obj_array.unsafe_clear_if_pointer t (pos + i);
      done;
    | Some dummy ->
      Obj_array.unsafe_blit
        ~src:dummy ~src_pos:0 ~len:metadata.slots_per_tuple
        ~dst:t     ~dst_pos:(Pointer.first_slot_index pointer)
    end;
  ;;

  let free (type slots) (t : slots t) (pointer : slots Pointer.t) =
    (* Check [pointer_is_valid] to:
       - avoid freeing a null pointer
       - avoid freeing a free pointer (this would lead to a pool inconsistency)
       - be able to use unsafe functions after. *)
    if not (pointer_is_valid t pointer) then
      failwiths "Pool.free of invalid pointer" (pointer, t)
        (<:sexp_of< _ Pointer.t * _ t >>);
    unsafe_free t pointer
  ;;

  let new1 t a0 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    pointer;
  ;;

  let new2 t a0 a1 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    pointer;
  ;;

  let new3 t a0 a1 a2 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    pointer;
  ;;

  let new4 t a0 a1 a2 a3 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4) (Obj.repr a3);
    pointer;
  ;;

  let new5 t a0 a1 a2 a3 a4 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4) (Obj.repr a3);
    Obj_array.unsafe_set t (offset + 5) (Obj.repr a4);
    pointer;
  ;;

  let new6 t a0 a1 a2 a3 a4 a5 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4) (Obj.repr a3);
    Obj_array.unsafe_set t (offset + 5) (Obj.repr a4);
    Obj_array.unsafe_set t (offset + 6) (Obj.repr a5);
    pointer;
  ;;

  let new7 t a0 a1 a2 a3 a4 a5 a6 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4) (Obj.repr a3);
    Obj_array.unsafe_set t (offset + 5) (Obj.repr a4);
    Obj_array.unsafe_set t (offset + 6) (Obj.repr a5);
    Obj_array.unsafe_set t (offset + 7) (Obj.repr a6);
    pointer;
  ;;

  let new8 t a0 a1 a2 a3 a4 a5 a6 a7 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4) (Obj.repr a3);
    Obj_array.unsafe_set t (offset + 5) (Obj.repr a4);
    Obj_array.unsafe_set t (offset + 6) (Obj.repr a5);
    Obj_array.unsafe_set t (offset + 7) (Obj.repr a6);
    Obj_array.unsafe_set t (offset + 8) (Obj.repr a7);
    pointer;
  ;;

  let new9 t a0 a1 a2 a3 a4 a5 a6 a7 a8 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4) (Obj.repr a3);
    Obj_array.unsafe_set t (offset + 5) (Obj.repr a4);
    Obj_array.unsafe_set t (offset + 6) (Obj.repr a5);
    Obj_array.unsafe_set t (offset + 7) (Obj.repr a6);
    Obj_array.unsafe_set t (offset + 8) (Obj.repr a7);
    Obj_array.unsafe_set t (offset + 9) (Obj.repr a8);
    pointer;
  ;;

  let new10 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1 ) (Obj.repr a0);
    Obj_array.unsafe_set t (offset + 2 ) (Obj.repr a1);
    Obj_array.unsafe_set t (offset + 3 ) (Obj.repr a2);
    Obj_array.unsafe_set t (offset + 4 ) (Obj.repr a3);
    Obj_array.unsafe_set t (offset + 5 ) (Obj.repr a4);
    Obj_array.unsafe_set t (offset + 6 ) (Obj.repr a5);
    Obj_array.unsafe_set t (offset + 7 ) (Obj.repr a6);
    Obj_array.unsafe_set t (offset + 8 ) (Obj.repr a7);
    Obj_array.unsafe_set t (offset + 9 ) (Obj.repr a8);
    Obj_array.unsafe_set t (offset + 10) (Obj.repr a9);
    pointer;
  ;;

  let new11 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1 ) (Obj.repr a0 );
    Obj_array.unsafe_set t (offset + 2 ) (Obj.repr a1 );
    Obj_array.unsafe_set t (offset + 3 ) (Obj.repr a2 );
    Obj_array.unsafe_set t (offset + 4 ) (Obj.repr a3 );
    Obj_array.unsafe_set t (offset + 5 ) (Obj.repr a4 );
    Obj_array.unsafe_set t (offset + 6 ) (Obj.repr a5 );
    Obj_array.unsafe_set t (offset + 7 ) (Obj.repr a6 );
    Obj_array.unsafe_set t (offset + 8 ) (Obj.repr a7 );
    Obj_array.unsafe_set t (offset + 9 ) (Obj.repr a8 );
    Obj_array.unsafe_set t (offset + 10) (Obj.repr a9 );
    Obj_array.unsafe_set t (offset + 11) (Obj.repr a10);
    pointer;
  ;;

  let new12 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
    let pointer = malloc t in
    let offset = Pointer.header_index pointer in
    Obj_array.unsafe_set t (offset + 1 ) (Obj.repr a0 );
    Obj_array.unsafe_set t (offset + 2 ) (Obj.repr a1 );
    Obj_array.unsafe_set t (offset + 3 ) (Obj.repr a2 );
    Obj_array.unsafe_set t (offset + 4 ) (Obj.repr a3 );
    Obj_array.unsafe_set t (offset + 5 ) (Obj.repr a4 );
    Obj_array.unsafe_set t (offset + 6 ) (Obj.repr a5 );
    Obj_array.unsafe_set t (offset + 7 ) (Obj.repr a6 );
    Obj_array.unsafe_set t (offset + 8 ) (Obj.repr a7 );
    Obj_array.unsafe_set t (offset + 9 ) (Obj.repr a8 );
    Obj_array.unsafe_set t (offset + 10) (Obj.repr a9 );
    Obj_array.unsafe_set t (offset + 11) (Obj.repr a10);
    Obj_array.unsafe_set t (offset + 12) (Obj.repr a11);
    pointer;
  ;;

  let get        t p slot = Obj.obj (Obj_array.get        t (Pointer.slot_index p slot))
  let unsafe_get t p slot = Obj.obj (Obj_array.unsafe_get t (Pointer.slot_index p slot))

  let set        t p slot x = Obj_array.set        t (Pointer.slot_index p slot) (Obj.repr x)
  let unsafe_set t p slot x = Obj_array.unsafe_set t (Pointer.slot_index p slot) (Obj.repr x)

  let get_tuple (type tuple) (t : (tuple, _) Slots.t t) pointer =
    let metadata = metadata t in
    let len = metadata.slots_per_tuple in
    if len = 1 then
      get t pointer Slot.t0
    else
      (Obj.magic
         (Obj_array.sub t ~pos:(Pointer.first_slot_index pointer) ~len : Obj_array.t)
       : tuple)
  ;;
end

include Pool

module Unsafe = struct
  include Pool

  let create slots ~capacity = create_with_dummy slots ~capacity ~dummy:None
end

module Debug (Pool : S) = struct
  open Pool

  let check_invariant = ref true

  let show_messages = ref true

  let debug name ts arg sexp_of_arg sexp_of_result f =
    let prefix = "Pool." in
    if !check_invariant then List.iter ts ~f:(invariant ignore);
    if !show_messages then Debug.eprints (concat [ prefix; name ]) arg sexp_of_arg;
    let result_or_exn = Result.try_with f in
    if !show_messages then
      Debug.eprints (concat [ prefix; name; " result" ]) result_or_exn
        (<:sexp_of< (result, exn) Result.t >>);
    Result.ok_exn result_or_exn;
  ;;

  module Slots = Slots

  module Slot = Slot

  module Pointer = struct
    open Pointer

    type nonrec 'slots t = 'slots t with sexp_of

    let phys_compare t1 t2 =
      debug "Pointer.phys_compare" [] (t1, t2) <:sexp_of< _ t * _ t >> <:sexp_of< int >>
        (fun () -> phys_compare t1 t2)
    ;;

    let phys_equal t1 t2 =
      debug "Pointer.phys_equal" [] (t1, t2) <:sexp_of< _ t * _ t >> <:sexp_of< bool >>
        (fun () -> phys_equal t1 t2)
    ;;

    let is_null t =
      debug "Pointer.is_null" [] t <:sexp_of< _ t >> <:sexp_of< bool >>
        (fun () -> is_null t)
    ;;

    let null = null

    module Id = struct
      open Id

      type nonrec t = t with bin_io, sexp
    end
  end

  type nonrec 'slots t = 'slots t with sexp_of

  let invariant = invariant

  let length = length

  let id_of_pointer t pointer =
    debug "id_of_pointer" [t] pointer
      <:sexp_of< _ Pointer.t >> <:sexp_of< Pointer.Id.t >>
      (fun () -> id_of_pointer t pointer)
  ;;

  let pointer_of_id_exn_is_supported = pointer_of_id_exn_is_supported

  let pointer_of_id_exn t id =
    debug "pointer_of_id_exn" [t] id
      <:sexp_of< Pointer.Id.t >> <:sexp_of< _ Pointer.t >>
      (fun () -> pointer_of_id_exn t id)
  ;;

  let pointer_is_valid t pointer =
    debug "pointer_is_valid" [t] pointer <:sexp_of< _ Pointer.t >> <:sexp_of< bool >>
      (fun () -> pointer_is_valid t pointer)
  ;;

  let create slots ~capacity ~dummy =
    debug "create" [] capacity <:sexp_of< int >> <:sexp_of< _ t >>
      (fun () -> create slots ~capacity ~dummy)
  ;;

  let capacity t =
    debug "capacity" [t] t <:sexp_of< _ t >> <:sexp_of< int >>
      (fun () -> capacity t)
  ;;

  let grow ?capacity t =
    debug "grow" [t] (`capacity capacity) (<:sexp_of< [ `capacity of int option ] >>)
      (<:sexp_of< _ t >>) (fun () -> grow ?capacity t)
  ;;

  let is_full t =
    debug "is_full" [t] t <:sexp_of< _ t >> <:sexp_of< bool >>
      (fun () -> is_full t)
  ;;

  let unsafe_free t p =
    debug "unsafe_free" [t] p <:sexp_of< _ Pointer.t >> <:sexp_of< unit >>
      (fun () -> unsafe_free t p)
  ;;

  let free t p =
    debug "free" [t] p <:sexp_of< _ Pointer.t >> <:sexp_of< unit >>
      (fun () -> free t p)
  ;;

  let debug_new t f =
    debug "new" [t] () <:sexp_of< unit >> <:sexp_of< _ Pointer.t >> f
  ;;

  let new1 t a0                         = debug_new t (fun () -> new1 t a0)
  let new2 t a0 a1                      = debug_new t (fun () -> new2 t a0 a1)
  let new3 t a0 a1 a2                   = debug_new t (fun () -> new3 t a0 a1 a2)
  let new4 t a0 a1 a2 a3                = debug_new t (fun () -> new4 t a0 a1 a2 a3)
  let new5 t a0 a1 a2 a3 a4             = debug_new t (fun () -> new5 t a0 a1 a2 a3 a4)
  let new6 t a0 a1 a2 a3 a4 a5          = debug_new t (fun () -> new6 t a0 a1 a2 a3 a4 a5)
  let new7 t a0 a1 a2 a3 a4 a5 a6
    = debug_new t (fun () -> new7 t a0 a1 a2 a3 a4 a5 a6)
  let new8 t a0 a1 a2 a3 a4 a5 a6 a7
    = debug_new t (fun () -> new8 t a0 a1 a2 a3 a4 a5 a6 a7)
  let new9 t a0 a1 a2 a3 a4 a5 a6 a7 a8
    = debug_new t (fun () -> new9 t a0 a1 a2 a3 a4 a5 a6 a7 a8)
  let new10 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9
    = debug_new t (fun () -> new10 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
  let new11 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
    = debug_new t (fun () -> new11 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
  let new12 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
    = debug_new t (fun () -> new12 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)

  let get_tuple t pointer =
    debug "get_tuple" [t] pointer <:sexp_of< _ Pointer.t >> <:sexp_of< _ >>
      (fun () -> get_tuple t pointer)
  ;;

  let debug_get name f t pointer =
    debug name [t] pointer <:sexp_of< _ Pointer.t >> <:sexp_of< _ >>
      (fun () -> f t pointer)
  ;;

  let get        t pointer slot = debug_get "get"        get        t pointer slot
  let unsafe_get t pointer slot = debug_get "unsafe_get" unsafe_get t pointer slot

  let debug_set name f t pointer slot a =
    debug name [t] pointer <:sexp_of< _ Pointer.t >> <:sexp_of< unit >>
      (fun () -> f t pointer slot a)
  ;;

  let set        t pointer slot a = debug_set "set"        set        t pointer slot a
  let unsafe_set t pointer slot a = debug_set "unsafe_set" unsafe_set t pointer slot a

end

module Error_check (Pool : S) = struct
  open Pool

  module Slots = Slots

  module Slot = Slot

  module Pointer = struct

    type 'slots t =
      { mutable is_valid : bool;
        pointer : 'slots Pointer.t;
      }
    with sexp_of

    let create pointer = { is_valid = true; pointer }

    let null () = { is_valid = false; pointer = Pointer.null () }

    let phys_compare t1 t2 = Pointer.phys_compare t1.pointer t2.pointer
    let phys_equal   t1 t2 = Pointer.phys_equal   t1.pointer t2.pointer

    let is_null t = Pointer.is_null t.pointer

    let follow t =
      if not t.is_valid then
        failwiths "attempt to use invalid pointer" t <:sexp_of< _ t >>;
      t.pointer;
    ;;

    let invalidate t = t.is_valid <- false

    module Id = Pointer.Id

  end

  type 'slots t = 'slots Pool.t with sexp_of

  let invariant = invariant

  let length = length

  let pointer_is_valid t { Pointer. is_valid; pointer } =
    is_valid && pointer_is_valid t pointer
  ;;

  let id_of_pointer t pointer = id_of_pointer t (Pointer.follow pointer)

  let pointer_of_id_exn_is_supported = pointer_of_id_exn_is_supported

  let pointer_of_id_exn t id =
    let pointer = pointer_of_id_exn t id in
    let is_valid = Pool.pointer_is_valid t pointer in
    { Pointer. is_valid; pointer }
  ;;

  let create = create

  let capacity = capacity
  let grow = grow
  let is_full = is_full

  let get_tuple t p = get_tuple t (Pointer.follow p)

  let get        t p = get        t (Pointer.follow p)
  let unsafe_get t p = unsafe_get t (Pointer.follow p)

  let set        t p slot v = set        t (Pointer.follow p) slot v
  let unsafe_set t p slot v = unsafe_set t (Pointer.follow p) slot v

  let unsafe_free t p =
    unsafe_free t (Pointer.follow p);
    Pointer.invalidate p;
  ;;

  let free t p =
    free t (Pointer.follow p);
    Pointer.invalidate p;
  ;;

  let new1 t a0                         = Pointer.create (Pool.new1 t a0)
  let new2 t a0 a1                      = Pointer.create (Pool.new2 t a0 a1)
  let new3 t a0 a1 a2                   = Pointer.create (Pool.new3 t a0 a1 a2)
  let new4 t a0 a1 a2 a3                = Pointer.create (Pool.new4 t a0 a1 a2 a3)
  let new5 t a0 a1 a2 a3 a4             = Pointer.create (Pool.new5 t a0 a1 a2 a3 a4)
  let new6 t a0 a1 a2 a3 a4 a5          = Pointer.create (Pool.new6 t a0 a1 a2 a3 a4 a5)
  let new7 t a0 a1 a2 a3 a4 a5 a6
    = Pointer.create (Pool.new7 t a0 a1 a2 a3 a4 a5 a6)
  let new8 t a0 a1 a2 a3 a4 a5 a6 a7
    = Pointer.create (Pool.new8 t a0 a1 a2 a3 a4 a5 a6 a7)
  let new9 t a0 a1 a2 a3 a4 a5 a6 a7 a8
    = Pointer.create (Pool.new9 t a0 a1 a2 a3 a4 a5 a6 a7 a8)
  let new10 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9
    = Pointer.create (Pool.new10 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
  let new11 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10
    = Pointer.create (Pool.new11 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
  let new12 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11
    = Pointer.create (Pool.new12 t a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
end
