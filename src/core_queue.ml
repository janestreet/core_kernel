open Common
open Sexplib.Conv
open Int_replace_polymorphic_compare

module Sexp  = Core_sexp
module List  = Core_list
module Array = Core_array
module Int   = Core_int


(* [t] stores the [t.length] queue elements at consecutive increasing indices of [t.elts],
   mod the capacity of [t], which is [Array.length t.elts].  The capacity is required to
   be a power of two (user-requested capacities are rounded up to the nearest power), so
   that mod can quickly be computed using [land t.mask], where [t.mask = capacity t - 1].
   So, queue element [i] is at [t.elts.( (t.front + i) land t.mask )].

   [num_mutations] is used to detect modification during iteration. *)
type 'a t =
  { mutable num_mutations : int
  ; mutable front         : int
  ; mutable mask          : int
  ; mutable length        : int
  ; mutable elts          : 'a array
  }
[@@deriving fields, sexp_of]

let inc_num_mutations t = t.num_mutations <- t.num_mutations + 1

let capacity t = t.mask + 1

let dummy (type a) (_ : a t) = (Obj.magic None : a)

let elts_index t i = (t.front + i) land t.mask

let unsafe_get t i   = Array.unsafe_get t.elts (elts_index t i)
let unsafe_set t i a = Array.unsafe_set t.elts (elts_index t i) a

let check_index_exn t i =
  if i < 0 || i >= t.length
  then failwiths "Queue index out of bounds" (i, `length t.length)
         [%sexp_of: int * [ `length of int ]]
;;

let get t i   = check_index_exn t i; unsafe_get t i
let set t i a =
  check_index_exn t i;
  inc_num_mutations t;
  unsafe_set t i a;
;;

let is_empty t = t.length = 0

let ensure_no_mutation t num_mutations =
  if t.num_mutations <> num_mutations
  then failwiths "mutation of queue during iteration" t [%sexp_of: _ t];
;;

let compare =
  let rec unsafe_compare_from compare_elt pos ~t1 ~t2 ~len1 ~len2 ~mut1 ~mut2 =
    match pos = len1, pos = len2 with
    | true , true  ->  0
    | true , false -> -1
    | false, true  ->  1
    | false, false ->
      let x = compare_elt (unsafe_get t1 pos) (unsafe_get t2 pos) in
      ensure_no_mutation t1 mut1;
      ensure_no_mutation t2 mut2;
      match x with
      | 0 -> unsafe_compare_from compare_elt (pos + 1) ~t1 ~t2 ~len1 ~len2 ~mut1 ~mut2
      | n -> n
  in
  fun compare_elt t1 t2 ->
    if phys_equal t1 t2 then
      0
    else
      unsafe_compare_from compare_elt 0 ~t1 ~t2
        ~len1:(length t1)
        ~len2:(length t2)
        ~mut1:t1.num_mutations
        ~mut2:t2.num_mutations
;;

let equal =
  let rec unsafe_equal_from equal_elt pos ~t1 ~t2 ~mut1 ~mut2 ~len =
    pos = len
    ||
    (let b = equal_elt (unsafe_get t1 pos) (unsafe_get t2 pos) in
     ensure_no_mutation t1 mut1;
     ensure_no_mutation t2 mut2;
     b && unsafe_equal_from equal_elt (pos + 1) ~t1 ~t2 ~mut1 ~mut2 ~len)
  in
  fun equal_elt t1 t2 ->
    phys_equal t1 t2
    ||
    (let len1 = length t1 in
     let len2 = length t2 in
     len1 = len2
     &&
     unsafe_equal_from equal_elt 0 ~t1 ~t2
       ~len:len1
       ~mut1:t1.num_mutations
       ~mut2:t2.num_mutations)
;;

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~num_mutations:ignore
      ~front:(check (fun front ->
        assert (front >= 0);
        assert (front < capacity t)))
      ~mask:(check (fun _ ->
        let capacity = capacity t in
        assert (capacity = Array.length t.elts);
        assert (capacity >= 1);
        assert (Int.is_pow2 capacity)))
      ~length:(check (fun length ->
        assert (length >= 0);
        assert (length <= capacity t)))
      ~elts:(check (fun _ ->
        let num_mutations = t.num_mutations in
        for i = 0 to capacity t - 1 do
          let elt = unsafe_get t i in
          if i < t.length
          then (invariant_a elt; ensure_no_mutation t num_mutations)
          else assert (phys_equal elt (dummy t))
        done)))
;;

let create (type a) ?capacity () : a t =
  let capacity =
    match capacity with
    | None -> 1
    | Some capacity ->
      if capacity < 0
      then failwiths "cannot have queue with negative capacity" capacity
             [%sexp_of: int]
      else if capacity = 0
      then 1
      else Int.ceil_pow2 capacity
  in
  { num_mutations = 0
  ; front         = 0
  ; mask          = capacity - 1
  ; length        = 0
  ; elts          = Array.create ~len:capacity (Obj.magic None : a)
  }
;;

let blit_to_array ~src dst =
  assert (src.length <= Array.length dst);
  let front_len = Int.min src.length (capacity src - src.front) in
  let rest_len  = src.length - front_len in
  Array.blit ~len:front_len ~src:src.elts ~src_pos:src.front ~dst ~dst_pos:0;
  Array.blit ~len:rest_len  ~src:src.elts ~src_pos:0         ~dst ~dst_pos:front_len;
;;

let set_capacity t desired_capacity =
  (* We allow arguments less than 1 to [set_capacity], but translate them to 1 to simplify
     the code that relies on the array length being a power of 2. *)
  inc_num_mutations t;
  let new_capacity = Int.ceil_pow2 (max 1 (max desired_capacity t.length)) in
  if new_capacity <> capacity t then begin
    let dst = Array.create ~len:new_capacity (dummy t) in
    blit_to_array ~src:t dst;
    t.front <- 0;
    t.mask  <- new_capacity - 1;
    t.elts  <- dst;
  end;
;;

let enqueue t a =
  inc_num_mutations t;
  if t.length = capacity t then set_capacity t (2 * t.length);
  unsafe_set t t.length a;
  t.length <- t.length + 1;
;;

let dequeue_nonempty t =
  inc_num_mutations t;
  let elts  = t.elts in
  let front = t.front in
  let res   = elts.( front ) in
  elts.( front ) <- dummy t;
  t.front      <- elts_index t 1;
  t.length     <- t.length - 1;
  res
;;

let dequeue_exn t =
  if is_empty t
  then raise Caml.Queue.Empty
  else dequeue_nonempty t
;;

let dequeue t =
  if is_empty t
  then None
  else Some (dequeue_nonempty t)
;;

let peek_nonempty t = Array.unsafe_get t.elts t.front
let last_nonempty t = unsafe_get t (t.length - 1)

let peek t =
  if is_empty t
  then None
  else Some (peek_nonempty t)
;;

let peek_exn t =
  if is_empty t
  then raise Caml.Queue.Empty
  else peek_nonempty t
;;

let last t =
  if is_empty t
  then None
  else Some (last_nonempty t)
;;

let last_exn t =
  if is_empty t
  then raise Caml.Queue.Empty
  else last_nonempty t
;;

let clear t =
  inc_num_mutations t;
  if length t > 0 then begin
    for i = 0 to t.length - 1 do
      unsafe_set t i (dummy t);
    done;
    t.length <- 0;
    t.front  <- 0;
  end;
;;

let blit_transfer ~src ~dst ?len () =
  inc_num_mutations src;
  inc_num_mutations dst;
  let len =
    match len with
    | None -> length src
    | Some len ->
      if len < 0
      then failwiths "Queue.blit_transfer: negative length" len [%sexp_of: int];
      min len (length src)
  in
  if len > 0 then begin
    set_capacity dst (max (capacity dst) (dst.length + len));
    let dst_start = dst.front + dst.length in
    for i = 0 to len - 1 do
      (* This is significantly faster than simply [enqueue dst (dequeue_nonempty src)] *)
      let src_i = (src.front + i) land src.mask in
      let dst_i = (dst_start + i) land dst.mask in
      Array.unsafe_set dst.elts dst_i (Array.unsafe_get src.elts src_i);
      Array.unsafe_set src.elts src_i (dummy src);
    done;
    dst.length <- dst.length + len;
    src.front  <- (src.front + len) land src.mask;
    src.length <- src.length - len;
  end;
;;

let enqueue_all t l =
  (* Traversing the list up front to compute its length is probably (but not definitely)
     better than doubling the underlying array size several times for large queues. *)
  set_capacity t (Int.max (capacity t) (length t + List.length l));
  List.iter l ~f:(fun x -> enqueue t x)
;;

let fold t ~init ~f =
  if t.length = 0
  then init
  else begin
    let num_mutations = t.num_mutations in
    let r = ref init in
    for i = 0 to t.length - 1 do
      r := f !r (unsafe_get t i);
      ensure_no_mutation t num_mutations;
    done;
    !r
  end;
;;

(* [iter] is implemented directly because implementing it in terms of fold is slower *)
let iter t ~f =
  let num_mutations = t.num_mutations in
  for i = 0 to t.length - 1 do
    f (unsafe_get t i);
    ensure_no_mutation t num_mutations;
  done;
;;

module C =
  Container.Make (struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
  end)

let to_list  = C.to_list
let count    = C.count
let sum      = C.sum
let find     = C.find
let find_map = C.find_map
let exists   = C.exists
let for_all  = C.for_all
let mem      = C.mem
let min_elt  = C.min_elt
let max_elt  = C.max_elt

(* For [concat_map], [filter_map], and [filter], we don't create [t_result] with [t]'s
   capacity because we have no idea how many elements [t_result] will ultimately hold. *)
let concat_map t ~f =
  let t_result = create () in
  iter t ~f:(fun a -> List.iter (f a) ~f:(fun b -> enqueue t_result b));
  t_result
;;

let filter_map t ~f =
  let t_result = create () in
  iter t ~f:(fun a ->
    match f a with
    | None   -> ()
    | Some b -> enqueue t_result b);
  t_result
;;

let filter t ~f =
  let t_result = create () in
  iter t ~f:(fun a -> if f a then enqueue t_result a);
  t_result
;;

let filter_inplace t ~f =
  let t2 = filter t ~f in
  clear t;
  blit_transfer ~src:t2 ~dst:t ();
;;

let copy src =
  let dst = create ~capacity:src.length () in
  blit_to_array ~src dst.elts;
  dst.length <- src.length;
  dst
;;

let of_list l =
  (* Traversing the list up front to compute its length is probably (but not definitely)
     better than doubling the underlying array size several times for large queues. *)
  let t = create ~capacity:(List.length l) () in
  List.iter l ~f:(fun x -> enqueue t x);
  t
;;

(* The queue [t] returned by [create] will have [t.length = 0], [t.front = 0], and
   [capacity t = Int.ceil_pow2 len].  So, we only have to set [t.length] to [len] after
   the blit to maintain all the invariants: [t.length] is equal to the number of elements
   in the queue, [t.front] is the array index of the first element in the queue, and
   [capacity t = Array.length t.elts]. *)
let of_array a =
  let len = Array.length a in
  let t = create ~capacity:len () in
  Array.blit ~len ~src:a ~src_pos:0 ~dst:t.elts ~dst_pos:0;
  t.length <- len;
  t
;;

let to_array t = Array.init t.length ~f:(fun i -> unsafe_get t i)

let map ta ~f =
  let num_mutations = ta.num_mutations in
  let tb = create ~capacity:ta.length () in
  tb.length <- ta.length;
  for i = 0 to ta.length - 1 do
    let b = f (unsafe_get ta i) in
    ensure_no_mutation ta num_mutations;
    Array.unsafe_set tb.elts i b;
  done;
  tb
;;

let singleton x =
  let t = create () in
  enqueue t x;
  t
;;

let sexp_of_t sexp_of_a t = to_list t |> [%sexp_of: a list]

let t_of_sexp a_of_sexp sexp = sexp |> [%of_sexp: a list] |> of_list

include
  Bin_prot.Utils.Make_iterable_binable1 (struct
    type nonrec 'a t = 'a t
    type 'a el       = 'a [@@deriving bin_io]

    let module_name = Some "Core.Queue"
    let length      = length
    let iter        = iter

    let init ~len ~next =
      let t = create ~capacity:len () in
      for i = 0 to len - 1 do
        Array.unsafe_set t.elts i (next ());
      done;
      t.length <- len;
      t
  end)

include
  Binary_searchable.Make1 (struct
    type nonrec 'a t = 'a t

    let get = get
    let length = length

    module For_test = struct
      let of_array a =
        let r = create () in
        (* We enqueue everything twice, and dequeue it once to ensure:
           - that the queue has the same content as the array.
           - that it has, in most cases, an interesting internal structure*)
        for i = 0 to Core_array.length a - 1 do
          enqueue r a.( i )
        done;
        for i = 0 to Core_array.length a - 1 do
          ignore (dequeue_exn r : bool);
          enqueue r a.( i )
        done;
        r
    end
  end)
