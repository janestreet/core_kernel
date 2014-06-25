open Sexplib
open Sexplib.Conv
open Core_hashtbl_intf
open With_return

module Binable = Binable0

let failwiths = Error.failwiths

module Hashable = Core_hashtbl_intf.Hashable

let hash_param = Hashable.hash_param
let hash       = Hashable.hash

module Int = struct
  type t = int

  let max (x : t) y = if x > y then x else y
  let min (x : t) y = if x < y then x else y
  include Int_pow2
end

module List = Core_list
module Array = Core_array

let phys_equal = (==)

module Entry : sig
  module Pool : sig
    type ('k, 'd) t with sexp_of

    val invariant : ('k, 'd) t -> unit
    val create : capacity:int -> (_, _) t
    val grow : ?capacity:int -> ('k, 'd) t -> ('k, 'd) t
  end

  type ('k, 'd) t = private int with sexp_of

  val null : unit -> (_, _) t
  val is_null : (_, _) t -> bool
  val create
    : ('k, 'd) Pool.t
    -> next:('k, 'd) t
    -> key:'k
    -> data:'d
    -> ('k, 'd) t

  val free : ('k, 'd) Pool.t -> ('k, 'd) t -> unit

  val next      : ('k, 'd) Pool.t -> ('k, 'd) t -> ('k, 'd) t
  val key       : ('k, 'd) Pool.t -> ('k, 'd) t -> 'k
  val data      : ('k, 'd) Pool.t -> ('k, 'd) t -> 'd

  val set_next  : ('k, 'd) Pool.t -> ('k, 'd) t -> ('k, 'd) t -> unit
  val set_data  : ('k, 'd) Pool.t -> ('k, 'd) t -> 'd -> unit
end = struct
  (* It is OK to use [Pool.Unsafe] because entries are never exposed to user code.  Thus,
     we can convince ourselves solely from looking at the implementation of
     [Pooled_hashtbl] that an entry is never used after it is freed. *)
  module Unsafe = Pool.Unsafe

  module Pointer = Unsafe.Pointer

  type ('k, 'd) fields = (('k,'d) fields Pointer.t,'k,'d) Unsafe.Slots.t3 with sexp_of

  type ('k, 'd) t = ('k, 'd) fields Pointer.t with sexp_of

  let create pool ~next ~key ~data = Unsafe.new3 pool next key data

  let free = Unsafe.free

  let next     p t = Unsafe.get p t Unsafe.Slot.t0
  let key      p t = Unsafe.get p t Unsafe.Slot.t1
  let data     p t = Unsafe.get p t Unsafe.Slot.t2

  let set_next p t = Unsafe.set p t Unsafe.Slot.t0
  let set_data p t = Unsafe.set p t Unsafe.Slot.t2

  module Pool = struct
    type ('k, 'd) t = ('k, 'd) fields Unsafe.t with sexp_of

    let invariant t = Unsafe.invariant ignore t

    let create ~capacity = Unsafe.create Unsafe.Slots.t3 ~capacity

    let grow = Unsafe.grow
  end

  let null = Pointer.null
  let is_null = Pointer.is_null
end


type ('k, 'd) hashtbl = {
  hashable : 'k Hashable.t;
  growth_allowed : bool;
  mutable length : int;
  mutable capacity : int;
  mutable entries : ('k, 'd) Entry.Pool.t;
  mutable table : (('k, 'd) Entry.t) array;
  mutable n_entries : int;
}

type ('k, 'd) t = ('k, 'd) hashtbl

type 'a key = 'a

module type S         = S         with type ('a, 'b) hashtbl = ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t

let sexp_of_key t = t.hashable.Hashable.sexp_of_t ;;

(* We match want to match Core's interface completely, so you can't change the load
   factor. If we care, we can add a new create function, put it back in the record, and
   plumb it through functions like map which call create. *)
let load_factor = 0.85 ;;

let max_table_length = Int_pow2.floor_pow2 Sys.max_array_length ;;

let calculate_table_size size =
  (* Ensure we can fit size elements in the table. *)
  let capacity = Int.ceil_pow2 size in
  let n_entries = int_of_float (ceil (float capacity *. load_factor)) in
  (capacity, Int.max size n_entries)
;;

let create ?(growth_allowed = true) ?(size = 128) ~hashable () =
  let size = Int.min (Int.max 1 size) max_table_length in
  let capacity, n_entries = calculate_table_size size in
  let table = Array.create ~len:capacity (Entry.null ()) in
  let entries = Entry.Pool.create ~capacity:n_entries in
  { hashable; growth_allowed; length = 0; capacity; table; entries; n_entries }
;;

let table_get (t : (('k, 'd) Entry.t) array) h =
  Array.unsafe_get t h
;;

let table_set (t : (('k, 'd) Entry.t) array) h (e : ('k, 'd) Entry.t) =
  Array.unsafe_set t h e
;;

let hash_key t key = t.hashable.Hashable.hash key

let compare_key t k1 k2 = t.hashable.Hashable.compare k1 k2

let slot t key = (hash_key t key) land (t.capacity - 1)

let length t = t.length

let is_empty t = t.length = 0

let clear t =
  for i=0 to t.capacity - 1 do
    let rec free_loop e =
      let next = Entry.next t.entries e in
      Entry.free t.entries e;
      if not (Entry.is_null next) then free_loop next;
    in
    let e = table_get t.table i in
    if not (Entry.is_null e) then begin
      free_loop e;
      table_set t.table i (Entry.null ());
    end;
  done;
  t.length <- 0
;;

let resize t =
  if t.growth_allowed then begin
    let new_capacity, new_n_entries = calculate_table_size (t.capacity + 1) in
    let old_table, old_capacity = t.table, t.capacity in
    t.entries <- Entry.Pool.grow t.entries ~capacity:new_n_entries;
    t.table <- Array.create ~len:new_capacity (Entry.null ());
    t.capacity <- new_capacity;
    t.n_entries <- new_n_entries;

    for i = 0 to old_capacity - 1 do
      let rec copy_entries e =
        if not (Entry.is_null e) then begin
          let key    = Entry.key t.entries e in
          let next_e = Entry.next t.entries e in
          let index = slot t key in
          let next  = table_get t.table index in
          Entry.set_next t.entries e next;
          table_set t.table index e;
          copy_entries next_e
        end
      in
      copy_entries (table_get old_table i)
    done
  end
  else begin
    t.entries <- Entry.Pool.grow t.entries ~capacity:(2 * t.n_entries);
    t.n_entries <- (2 * t.n_entries);
  end
;;

let rec find_entry t ~key ~it =
  if Entry.is_null it then it
  else
    let curr_key = Entry.key t.entries it in
    if compare_key t curr_key key = 0 then it
    else find_entry t ~key ~it:(Entry.next t.entries it)
;;

let mem t key =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  not (Entry.is_null e)
;;

(* we assume here that [Entry.create] will succeed *)
let insert_link_pool_not_full t ~index ~key ~data ~it =
  (* New entry adds to the begining of the list, which is t.table.(index) or `it`. *)
  let e = Entry.create t.entries ~next:it ~key ~data in
  table_set t.table index e;
  t.length <- t.length + 1;
;;

let insert_link t ~index ~key ~data ~it =
  if t.length < t.n_entries then
    insert_link_pool_not_full t ~index ~key ~data ~it
  else begin
    resize t;
    let index = slot t key in
    let it = table_get t.table index in
    insert_link_pool_not_full t ~index ~key ~data ~it
  end
;;

let delete_link t ~index ~prev ~e =
  let next = Entry.next t.entries e in
  if Entry.is_null prev then table_set t.table index next
  else (Entry.set_next t.entries prev next);
  Entry.free t.entries e;
  t.length <- t.length - 1
;;

(** If key is already in t, return the entry it was found at. Otherwise, create an entry,
    set it to data and return the empty entry. *)
let set_or_entry t ~key ~data =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if Entry.is_null e then insert_link t ~index ~key ~data ~it;
  e
;;

let set t ~key ~data =
  let e = set_or_entry t ~key ~data in
  if not (Entry.is_null e) then Entry.set_data t.entries e data
;;

let replace = set ;;

let add t ~key ~data =
  let e = set_or_entry t ~key ~data in
  if Entry.is_null e then `Ok else `Duplicate
;;

let add_exn (type k) t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
    let module T = struct
      type key = k
      let sexp_of_key = sexp_of_key t
      exception Add_key_already_present of key with sexp
    end
    in
    raise (T.Add_key_already_present key)
;;

let find_or_add t key ~default =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if not (Entry.is_null e) then Entry.data t.entries e
  else
    let data = default () in
    insert_link t ~index ~key ~data ~it;
    data
;;

let find t key =
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if Entry.is_null e then None
  else Some (Entry.data t.entries e)
;;

let find_exn t key =
  (* We could call find here, but that returns a boxed option. *)
  let index = slot t key in
  let it = table_get t.table index in
  let e = find_entry t ~key ~it in
  if not (Entry.is_null e) then Entry.data t.entries e
  else raise Not_found
;;

(* This is split in a rather odd way so as to make find_and_remove for a single entry
   chain able to be inlined. *)
let rec remove_key_r t index key e prev =
  if compare_key t (Entry.key t.entries e) key = 0 then begin
    let data = Entry.data t.entries e in
    delete_link t ~index ~prev ~e;
    Some data
  end
  else let next = Entry.next t.entries e in
  if Entry.is_null next then None else remove_key_r t index key next e
;;

let find_and_remove t key =
  let index = slot t key in
  let e = table_get t.table index in
  (* can't reuse find_entry given that we require the prev pointer *)
  if not (Entry.is_null e) then begin
    if compare_key t (Entry.key t.entries e) key = 0 then begin
      let data = Entry.data t.entries e in
      delete_link t ~index ~prev:(Entry.null ()) ~e;
      Some data
    end
    else let next = Entry.next t.entries e in
    if Entry.is_null next then None else remove_key_r t index key next e
  end
  else None
;;

let incr ?(by=1) t key =
  (* Core's implementation actually has a bug here and, if the key is missing, it sets
     it to 1, not by. I believe "by" is correct. *)
  let e = set_or_entry t ~key ~data:by in
  if not (Entry.is_null e) then
    let data = Entry.data t.entries e in
    Entry.set_data t.entries e (data + by)
;;

let change t key f =
  let index = slot t key in
  let it = table_get t.table index in
  let rec change_key e prev =
    if not (Entry.is_null e) then
      let curr_key = Entry.key t.entries e in
      if compare_key t curr_key key = 0 then
        match f (Some (Entry.data t.entries e)) with
        | Some data -> Entry.set_data t.entries e data
        | None -> delete_link t ~index ~prev ~e
      else change_key (Entry.next t.entries e) e
    else
      (* New entry is inserted in the begining of the list (it) *)
      match (f None) with
      | None -> ()
      | Some data -> insert_link t ~index ~key ~data ~it
  in
  change_key it (Entry.null ())
;;


(* Split similar to find and removed. Code duplicated to avoid allocation and
   unroll/inline the single entry case *)
let rec remove_key_r t index key e prev =
  if compare_key t (Entry.key t.entries e) key = 0 then
    delete_link t ~index ~prev ~e
  else let next = Entry.next t.entries e in
    if not (Entry.is_null next) then
      remove_key_r t index key next e
;;

let remove t key =
  let index = slot t key in
  let e = table_get t.table index in
  (* can't reuse find_entry given that we require the prev pointer *)
  if not (Entry.is_null e) then begin
    if compare_key t (Entry.key t.entries e) key = 0 then
      delete_link t ~index ~prev:(Entry.null ()) ~e
    else
      let next = Entry.next t.entries e in
      if not (Entry.is_null next) then
        remove_key_r t index key next e
  end
;;

(* TODO: If we care, these can be optimized to avoid option boxes, allocating closures,
   etc. These are largely copied from core_hashtbl.ml. If we do care about performance
   here, we should, at the least, allow you to determine, given an entry, whether it has
   a key. Then we could just iterate over the Entry_pool and get better cache behavior. *)

let remove_one t key =
  match find t key with
  | None -> ()
  | Some ([] | [_]) -> remove t key
  | Some (_ :: tl) -> replace t ~key ~data:tl
;;

let add_multi t ~key ~data =
  match find t key with
  | None -> replace t ~key ~data:[data]
  | Some l -> replace t ~key ~data:(data :: l)
;;

let remove_multi t key =
  match find t key with
  | None -> ()
  | Some [] | Some [_] -> remove t key
  | Some (_ :: tl) -> replace t ~key ~data:tl
;;

let iter t ~f =
  if t.length = 0 then ()
  else begin
    for i = 0 to t.capacity - 1 do
      let rec loop e =
        if not (Entry.is_null e) then begin
          f ~key:(Entry.key t.entries e)
            ~data:(Entry.data t.entries e);
          loop (Entry.next t.entries e)
        end
      in
      loop (table_get t.table i)
    done
  end
;;

let fold =
  let rec fold_entries t e acc f =
    if (Entry.is_null e) then acc
    else
      fold_entries t
        (Entry.next t.entries e)
        (f ~key:(Entry.key t.entries e) ~data:(Entry.data t.entries e) acc)
        f
  in
  fun t ~init ~f ->
    if length t = 0 then init
    else begin
      let acc = ref init in
      for i = 0 to t.capacity - 1 do
        let e = table_get t.table i in
        if not (Entry.is_null e) then
          acc := fold_entries t e !acc f;
      done;
      !acc
    end
;;

let invariant t =
  let n = Array.length t.table in
  for i = 0 to n - 1 do
    let loop e =
      if Entry.is_null e then ()
      else assert (i = (slot t (Entry.key t.entries e)))
    in
    loop (table_get t.table i)
  done;
  Entry.Pool.invariant t.entries;
  let real_len = fold t ~init:0 ~f:(fun ~key:_ ~data:_ i -> i + 1) in
  assert (real_len = t.length);
  assert (t.length <= t.n_entries);
;;

let sexp_of_t sexp_of_k sexp_of_d t =
  let coll ~key:k ~data:v acc = Sexp.List [sexp_of_k k; sexp_of_d v] :: acc in
  Sexp.List (fold ~f:coll t ~init:[])
;;

let existsi t ~f =
  with_return (fun r ->
    iter t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
    false)
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data) ;;

let for_alli t ~f = not (existsi t ~f:(fun ~key   ~data -> not (f ~key ~data)))
let for_all  t ~f = not (existsi t ~f:(fun ~key:_ ~data -> not (f       data)))

let mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data -> replace new_t ~key ~data:(f ~key ~data));
  new_t
;;

let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data) ;;

let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | Some new_data -> replace new_t ~key ~data:new_data
    | None -> ());
  new_t
;;

let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data) ;;

let filteri t ~f =
  filter_mapi t ~f:(fun ~key ~data -> if f ~key ~data then Some data else None)
;;

let filter t ~f = filteri t ~f:(fun ~key:_ ~data -> f data) ;;

let partition_mapi t ~f =
  let t0 =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let t1 =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | `Fst new_data -> replace t0 ~key ~data:new_data
    | `Snd new_data -> replace t1 ~key ~data:new_data);
  (t0, t1)
;;

let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data) ;;

let partitioni_tf t ~f =
  partition_mapi t ~f:(fun ~key ~data -> if f ~key ~data then `Fst data else `Snd data)
;;

let partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data) ;;

let iter_vals t ~f = iter t ~f:(fun ~key:_ ~data -> f data) ;;

let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size = match size with Some s -> s | None -> List.length rows in
  let res = create ?growth_allowed ~hashable ~size () in
  let dupes = ref [] in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    if mem res key then
      dupes := key :: !dupes
    else
      replace res ~key ~data);
  match !dupes with
  | [] -> `Ok res
  | keys -> `Duplicate_keys (List.dedup ~compare:hashable.Hashable.compare keys)
;;

let create_mapped_multi ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size = match size with Some s -> s | None -> List.length rows in
  let res = create ?growth_allowed ~size ~hashable () in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    add_multi res ~key ~data);
  res
;;

let of_alist ?growth_allowed ?size ~hashable lst =
  match create_mapped ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst with
  | `Ok t -> `Ok t
  | `Duplicate_keys k -> `Duplicate_key (List.hd_exn k)
;;

let of_alist_report_all_dups ?growth_allowed ?size ~hashable lst =
  create_mapped ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let of_alist_or_error ?growth_allowed ?size ~hashable lst =
  match of_alist ?growth_allowed ?size ~hashable lst with
  | `Ok v -> Result.Ok v
  | `Duplicate_key key ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    Or_error.error "Pooled_hashtbl.of_alist_exn: duplicate key" key <:sexp_of< key >>
;;

let of_alist_exn ?growth_allowed ?size ~hashable lst =
  match of_alist_or_error ?growth_allowed ?size ~hashable lst with
  | Result.Ok v -> v
  | Result.Error e -> Error.raise e
;;

let of_alist_multi ?growth_allowed ?size ~hashable lst =
  create_mapped_multi ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let to_alist t = fold ~f:(fun ~key ~data list -> (key, data)::list) ~init:[] t

let validate ~name f t = Validate.alist ~name f (to_alist t)

let keys t = fold t ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)

let data t = fold ~f:(fun ~key:_ ~data list -> data::list) ~init:[] t

let add_to_groups groups ~get_key ~get_data ~combine ~rows =
  List.iter rows ~f:(fun row ->
    let key = get_key row in
    let data = get_data row in
    let data =
      match find groups key with
      | None -> data
      | Some old -> combine old data
    in
    replace groups ~key ~data)
;;

let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine rows =
  let res = create ?growth_allowed ?size ~hashable () in
  add_to_groups res ~get_key ~get_data ~combine ~rows;
  res
;;

let create_with_key ?growth_allowed ?size ~hashable ~get_key rows =
  create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data:(fun x -> x) rows
;;

let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key rows =
  match create_with_key ?growth_allowed ?size ~hashable ~get_key rows with
  | `Ok t -> t
  | `Duplicate_keys keys ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    failwiths "Pooled_hashtbl.create_with_key: duplicate keys" keys <:sexp_of< key list >>
;;

let merge t1 t2 ~f =
  if not (phys_equal t1.hashable t2.hashable)
  then invalid_arg "Pooled_hashtbl.merge: different 'hashable' values";
  let create () =
    create
      ~growth_allowed:t1.growth_allowed
      ~hashable:t1.hashable
      ~size:t1.length
      ()
  in
  let t = create () in
  let unique_keys = create () in
  let record_key ~key ~data:_ = replace unique_keys ~key ~data:() in
  iter t1 ~f:record_key;
  iter t2 ~f:record_key;
  iter unique_keys ~f:(fun ~key ~data:_ ->
    let arg =
      match find t1 key, find t2 key with
      | None, None -> assert false
      | None, Some r -> `Right r
      | Some l, None -> `Left l
      | Some l, Some r -> `Both (l, r)
    in
    match f ~key arg with
    | Some data -> replace t ~key ~data
    | None -> ());
  t
;;

let merge_into ~f ~src ~dst =
  iter src ~f:(fun ~key ~data ->
    match f ~key data (find dst key) with
    | Some data -> replace dst ~key ~data
    | None -> ())

let filteri_inplace t ~f =
  let to_remove =
    fold t ~init:[] ~f:(fun ~key ~data ac ->
      if f key data then ac else key :: ac)
  in
  List.iter to_remove ~f:(fun key -> remove t key);
;;

let filter_inplace t ~f =
  filteri_inplace t ~f:(fun _ data -> f data)
;;

let equal t t' equal =
  length t = length t' &&
  with_return (fun r ->
    iter t ~f:(fun ~key ~data ->
      match find t' key with
      | None -> r.return false
      | Some data' -> if not (equal data data') then r.return false);
    true)
;;

let similar = equal

let copy t =
  let table = Array.create ~len:t.capacity (Entry.null ()) in
  let entries = Entry.Pool.create ~capacity:t.n_entries in
  let copy = {
    hashable = t.hashable;
    growth_allowed = t.growth_allowed;
    length = 0;
    capacity = t.capacity;
    table;
    entries;
    n_entries = t.n_entries;
  }
  in
  iter t ~f:(fun ~key ~data -> add_exn copy ~key ~data);
  copy
;;

module Accessors = struct
  let invariant       = invariant
  let clear           = clear
  let copy            = copy
  let remove          = remove
  let remove_one      = remove_one
  let replace         = replace
  let set             = set
  let add             = add
  let add_exn         = add_exn
  let change          = change
  let add_multi       = add_multi
  let remove_multi    = remove_multi
  let mem             = mem
  let iter            = iter
  let exists          = exists
  let existsi         = existsi
  let for_all         = for_all
  let for_alli        = for_alli
  let fold            = fold
  let length          = length
  let is_empty        = is_empty
  let map             = map
  let mapi            = mapi
  let filter_map      = filter_map
  let filter_mapi     = filter_mapi
  let filter          = filter
  let filteri         = filteri
  let partition_map   = partition_map
  let partition_mapi  = partition_mapi
  let partition_tf    = partition_tf
  let partitioni_tf   = partitioni_tf
  let find_or_add     = find_or_add
  let find            = find
  let find_exn        = find_exn
  let find_and_remove = find_and_remove
  let iter_vals       = iter_vals
  let to_alist        = to_alist
  let validate        = validate
  let merge           = merge
  let merge_into      = merge_into
  let keys            = keys
  let data            = data
  let filter_inplace  = filter_inplace
  let filteri_inplace = filteri_inplace
  let equal           = equal
  let similar         = similar
  let incr            = incr
  let sexp_of_key     = sexp_of_key
end

module type Key         = Key
module type Key_binable = Key_binable

module Creators (Key : sig
  type 'a t

  val hashable : 'a t Hashable.t
end) : sig

  type ('a, 'b) t_ = ('a Key.t, 'b) t

  val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_

  include Creators
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a Key.t
    with type ('key, 'data, 'a) create_options := ('key, 'data, 'a) create_options_without_hashable

end = struct

  let hashable = Key.hashable

  type ('a, 'b) t_ = ('a Key.t, 'b) t

  let create ?growth_allowed ?size () = create ?growth_allowed ?size ~hashable ()

  let of_alist ?growth_allowed ?size l =
    of_alist ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_report_all_dups ?growth_allowed ?size l =
    of_alist_report_all_dups ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_or_error ?growth_allowed ?size l =
    of_alist_or_error ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_exn ?growth_allowed ?size l =
    of_alist_exn ?growth_allowed ~hashable ?size l
  ;;

  let t_of_sexp k_of_sexp d_of_sexp sexp =
    let alist = <:of_sexp< (k * d) list >> sexp in
    of_alist_exn alist ~size:(List.length alist)
  ;;

  let of_alist_multi ?growth_allowed ?size l =
    of_alist_multi ?growth_allowed ~hashable ?size l
  ;;

  let create_mapped ?growth_allowed ?size ~get_key ~get_data l =
    create_mapped ?growth_allowed ~hashable ?size ~get_key ~get_data l
  ;;

  let create_with_key ?growth_allowed ?size ~get_key l =
    create_with_key ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let create_with_key_exn ?growth_allowed ?size ~get_key l =
    create_with_key_exn ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let group ?growth_allowed ?size ~get_key ~get_data ~combine l =
    group ?growth_allowed ~hashable ?size ~get_key ~get_data ~combine l
  ;;
end

module Poly = struct

  type ('a, 'b) t = ('a, 'b) hashtbl

  type 'a key = 'a

  let hashable = Hashable.poly

  include Creators (struct
    type 'a t = 'a
    let hashable = hashable
  end)

  include Accessors

  let sexp_of_t = sexp_of_t

  include Bin_prot.Utils.Make_iterable_binable2 (struct
    type ('a, 'b) z = ('a, 'b) t
    type ('a, 'b) t = ('a, 'b) z
    type ('a, 'b) el = 'a * 'b with bin_io
    type ('a, 'b) acc = ('a, 'b) t

    let module_name = Some "Pooled_hashtbl"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init size = create ~size ()

    let insert t (key, data) _i =
      match find t key with
      | None -> replace t ~key ~data; t
      | Some _ -> failwith "Pooled_hashtbl.bin_read_t_: duplicate key"
    ;;

    let finish = Fn.id
  end)

end

module Make (Key : Key) = struct

  let hashable =
    { Hashable.
      hash = Key.hash;
      compare = Key.compare;
      sexp_of_t = Key.sexp_of_t;
    }
  ;;

  type key = Key.t with sexp_of
  type ('a, 'b) hashtbl = ('a, 'b) t
  type 'a t = (key, 'a) hashtbl
  type 'a key_ = key

  include Creators (struct
    type 'a t = Key.t
    let hashable = hashable
  end)

  include Accessors

  let sexp_of_t sexp_of_v t = Poly.sexp_of_t Key.sexp_of_t sexp_of_v t
  let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp

end

module Make_binable (Key : sig
  include Key
  include Binable.S with type t := t
end) = struct
  include Make (Key)

  include Bin_prot.Utils.Make_iterable_binable1 (struct
    type 'a acc = 'a t
    type 'a t = 'a acc
    type 'a el = Key.t * 'a with bin_io

    let module_name = Some "Pooled_hashtbl"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init size = create ~size ()

    let insert t (key, data) _i =
      match find t key with
      | None -> replace t ~key ~data; t
      | Some _ -> failwiths "Pooled_hashtbl.bin_read_t: duplicate key"
        key <:sexp_of< Key.t >>
    ;;

    let finish = Fn.id
  end)

end

