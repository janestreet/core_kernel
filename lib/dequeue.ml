open Std_internal

type 'a t = {
  (* [arr] is a cyclic buffer *)
  mutable arr                  : 'a array;
  (* [front_index] and [back_index] are the positions in which new elements may be
     enqueued.  This makes the active part of [arr] the range from [front_index+1] to
     [back_index-1] (modulo the length of [arr] and wrapping around if necessary).  Note
     that this means the active range is maximized when [front_index = back_index], which
     occurs when there are [Array.length arr - 1] active elements. *)
  mutable front_index          : int;
  mutable back_index           : int;
  (* apparent_front_index is what is exposed as the front index externally.  It has no
     real relation to the array -- every enqueue to the front decrements it and every
     dequeue from the front increments it. *)
  mutable apparent_front_index : int;
  mutable length               : int;
  (* We keep arr_length here as a speed hack.  Calling Array.length on arr is actually
     meaningfully slower. *)
  mutable arr_length           : int;
  never_shrink                 : bool;
  dummy                        : 'a;
}

let create ?initial_length ?never_shrink () =
  let never_shrink =
    match never_shrink with
    | None   -> Option.is_some initial_length
    | Some b -> b
  in
  let initial_length = Option.value ~default:7 initial_length in
  if initial_length < 1 then
    invalid_argf "passed non-positive initial_length to Dequeue.create: %d"
      initial_length ();
  let arr_length = initial_length + 1 in
  let dummy = (Obj.magic () : 'a) in
  {
    (* make the initial array length be [initial_length + 1] so we can fit
       [initial_length] elements without growing *)
    arr                  = Array.create ~len:arr_length dummy;
    front_index          = 0;
    back_index           = 1;
    apparent_front_index = 0;
    length               = 0;
    arr_length;
    never_shrink;
    dummy;
  }
;;

let length t = t.length
TEST = length (create ()) = 0

let is_empty t = length t = 0

(* We keep track of the length in a mutable field for speed, but this calculation should
   be correct by construction, and can be used for testing. *)
let _invariant_length t =
  let constructed_length =
    if t.front_index < t.back_index
    then t.back_index - t.front_index - 1
    else t.back_index - t.front_index - 1 + t.arr_length
  in
  assert (length t = constructed_length)
;;

let clear t =
  begin
    if t.never_shrink then
      (* clear the array to allow elements to be garbage collected *)
      Array.replace_all ~f:(fun _ -> t.dummy) t.arr
    else
      t.arr <- Array.create ~len:8 t.dummy
  end;
  t.front_index <- 0;
  t.back_index  <- 1;
  t.length      <- 0;
  t.arr_length  <- Array.length t.arr;
;;

(* The various "when_not_empty" functions return misleading numbers when the dequeue is
   empty.  They are safe to call if it is known that the dequeue is non-empty. *)
let apparent_front_index_when_not_empty t = t.apparent_front_index
let apparent_back_index_when_not_empty t = t.apparent_front_index + length t - 1

let actual_front_index_when_not_empty t =
  if t.front_index = t.arr_length - 1
  then 0
  else t.front_index + 1
;;

let actual_back_index_when_not_empty t =
  if t.back_index = 0
  then t.arr_length - 1
  else t.back_index - 1
;;

let checked t f =
  if is_empty t
  then None
  else Some (f t)
;;

let assert_not_empty t name =
  if is_empty t then
    failwithf "%s: Dequeue.t is empty" name ()
;;

let apparent_front_index t = checked t apparent_front_index_when_not_empty
let apparent_back_index  t = checked t apparent_back_index_when_not_empty

let foldi' t dir ~init ~f =
  if is_empty t
  then init
  else begin
    let apparent_front = apparent_front_index_when_not_empty t in
    let apparent_back  = apparent_back_index_when_not_empty t in
    let actual_front   = actual_front_index_when_not_empty t in
    let actual_back    = actual_back_index_when_not_empty t in
    let rec loop acc ~apparent_i ~real_i ~stop_pos ~step =
      if real_i = stop_pos
      then (acc, apparent_i)
      else
        loop
          (f apparent_i acc t.arr.(real_i))
          ~apparent_i:(apparent_i + step)
          ~real_i:(real_i + step)
          ~stop_pos
          ~step
    in
    (* We want to iterate from actual_front to actual_back (or vice versa), but we may
       need to wrap around the array to do so.  Thus we do the following:
       1.  If the active range is contiguous (i.e. actual_front <= actual_back), then loop
           starting at the appropriate end of the active range until we reach the first
           element outside of it.
       2.  If it is not contiguous (actual_front > actual_back), then first loop from the
           appropriate end of the active range to the end of the array.  Then, loop from
           the opposite end of the array to the opposite end of the active range.
    *)
    match dir with
    | `front_to_back ->
      if actual_front <= actual_back then begin
        let acc, _ =
          loop init
            ~apparent_i:apparent_front
            ~real_i:actual_front ~stop_pos:(actual_back + 1) ~step:1
        in
        acc
      end else begin
        let acc, apparent_i =
          loop init
            ~apparent_i:apparent_front
            ~real_i:actual_front ~stop_pos:t.arr_length ~step:1
        in
        let acc, _ = loop acc ~apparent_i ~real_i:0 ~stop_pos:(actual_back + 1) ~step:1 in
        acc
      end
    | `back_to_front ->
      if actual_front <= actual_back then begin
        let acc, _ =
          loop init
            ~apparent_i:apparent_back
            ~real_i:actual_back ~stop_pos:(actual_front - 1) ~step:(-1)
        in
        acc
      end else begin
        let acc, apparent_i =
          loop init
            ~apparent_i:apparent_back
            ~real_i:actual_back ~stop_pos:(-1) ~step:(-1) in
        let acc, _ =
          loop acc ~apparent_i ~real_i:(t.arr_length - 1)
            ~stop_pos:(actual_front - 1) ~step:(-1)
        in
        acc
      end
  end
;;

let fold'  t dir ~init ~f = foldi' t dir ~init    ~f:(fun _ acc v -> f acc v)
let iteri' t dir       ~f = foldi' t dir ~init:() ~f:(fun i ()  v -> f i   v)
let iter'  t dir       ~f = foldi' t dir ~init:() ~f:(fun _ ()  v -> f     v)
let fold   t     ~init ~f = fold'  t `front_to_back ~init ~f
let foldi  t     ~init ~f = foldi' t `front_to_back ~init ~f
let iteri  t           ~f = iteri' t `front_to_back ~f

(* We have to be careful here, importing all of Container.Make would change the runtime of
   some functions ([length] minimally) silently without changing the semantics.  We get
   around that by importing things explicitly.  *)
module C = Container.Make (struct
  type nonrec 'a t = 'a t
  let fold = fold
end)
let count      = C.count

let iter t ~f =
  if not (is_empty t) then begin
    let actual_front = actual_front_index_when_not_empty t in
    let actual_back  = actual_back_index_when_not_empty t in
    let rec loop ~real_i ~stop_pos =
      if real_i < stop_pos then begin
        f t.arr.(real_i);
        loop ~real_i:(real_i + 1) ~stop_pos
      end
    in
    if actual_front <= actual_back then
      loop ~real_i:actual_front ~stop_pos:(actual_back + 1)
    else begin
      loop ~real_i:actual_front ~stop_pos:t.arr_length;
      loop ~real_i:0 ~stop_pos:(actual_back + 1)
    end
  end
;;

let exists     = C.exists
let mem        = C.mem
let for_all    = C.for_all
let find_map   = C.find_map
let find       = C.find
let to_list    = C.to_list

let blit new_arr t =
  assert (not (is_empty t));
  let actual_front = actual_front_index_when_not_empty t in
  let actual_back  = actual_back_index_when_not_empty t in
  let old_arr = t.arr in
  if actual_front <= actual_back then
    Array.blit ~src:old_arr ~dst:new_arr ~src_pos:actual_front ~dst_pos:0
      ~len:(length t)
  else begin
    let break_pos = Array.length old_arr - actual_front in
    Array.blit ~src:old_arr ~dst:new_arr ~src_pos:actual_front ~dst_pos:0
      ~len:break_pos;
    Array.blit ~src:old_arr ~dst:new_arr ~src_pos:0 ~dst_pos:break_pos
      ~len:(actual_back + 1);
  end;
  (* length depends on t.arr and t.front_index, so this needs to be first *)
  t.back_index  <- length t;
  t.arr         <- new_arr;
  t.arr_length  <- Array.length new_arr;
  t.front_index <- Array.length new_arr - 1;
  (* Since t.front_index = Array.length new_arr - 1, this is asserting that t.back_index
     is a valid index in the array and that the array can support at least one more
     element -- recall, if t.front_index = t.back_index then the array is full.

     Note that this is true if and only if Array.length new_arr > length t + 1.
  *)
  assert (t.front_index > t.back_index)
;;

let maybe_shrink_underlying t =
  if not t.never_shrink
     && t.arr_length > 10
     && t.arr_length / 3 > length t
  then begin
    let new_arr = Array.create ~len:(t.arr_length / 2) t.dummy in
    blit new_arr t;
  end
;;

let grow_underlying t =
  let new_arr = Array.create ~len:(t.arr_length * 2) t.dummy in
  blit new_arr t
;;

let enqueue_back t v =
  if t.front_index = t.back_index then grow_underlying t;
  t.arr.(t.back_index) <- v;
  t.back_index         <- if t.back_index = t.arr_length - 1 then 0 else t.back_index + 1;
  t.length             <- t.length + 1;
;;

let enqueue_front t v =
  if t.front_index = t.back_index then grow_underlying t;
  t.arr.(t.front_index)  <- v;
  t.front_index          <- if t.front_index = 0
                            then t.arr_length - 1
                            else t.front_index - 1;
  t.apparent_front_index <- t.apparent_front_index - 1;
  t.length               <- t.length + 1;
;;

let enqueue t back_or_front v =
  match back_or_front with
  | `back  -> enqueue_back t v
  | `front -> enqueue_front t v
;;

let peek_front_exn t =
  assert_not_empty t "Dequeue.peek_front_exn";
  t.arr.(actual_front_index_when_not_empty t)
;;

let peek_front t = try Some (peek_front_exn t) with _ -> None

let peek_back_exn t =
  assert_not_empty t "Dequeue.peek_back_exn";
  t.arr.(actual_back_index_when_not_empty t)
;;

let peek_back t = try Some (peek_back_exn t) with _ -> None

let peek t back_or_front =
  match back_or_front with
  | `back  -> peek_back t
  | `front -> peek_front t
;;

let dequeue_front_exn t =
  assert_not_empty t "Dequeue.dequeue_front_exn";
  let i = actual_front_index_when_not_empty t in
  let res = t.arr.(i) in
  t.arr.(i)              <- t.dummy;
  t.front_index          <- i;
  t.apparent_front_index <- t.apparent_front_index + 1;
  t.length               <- t.length - 1;
  maybe_shrink_underlying t;
  res
;;

let dequeue_front t = try Some (dequeue_front_exn t) with _ -> None

let dequeue_back_exn t =
  assert_not_empty t "Dequeue.dequeue_back_exn";
  let i = actual_back_index_when_not_empty t in
  let res = t.arr.(i) in
  t.arr.(i)    <- t.dummy;
  t.back_index <- i;
  t.length     <- t.length - 1;
  maybe_shrink_underlying t;
  res
;;

let dequeue_back t = try Some (dequeue_back_exn t) with _ -> None

let dequeue_exn t back_or_front =
  match back_or_front with
  | `front -> dequeue_front_exn t
  | `back  -> dequeue_back_exn t
;;

let dequeue t back_or_front = try Some (dequeue_exn t back_or_front) with _ -> None

let drop_gen ?(n=1) ~dequeue t =
  if n < 0 then
    invalid_argf "Dequeue.drop:  negative input (%d)" n ();
  let rec loop n =
    if n > 0 then
      match dequeue t with
      | None   -> ()
      | Some _ -> loop (n - 1)
  in
  loop n
;;

let drop_front ?n t = drop_gen ?n ~dequeue:dequeue_front t
let drop_back  ?n t = drop_gen ?n ~dequeue:dequeue_back t

let drop ?n t back_or_front =
  match back_or_front with
  | `back  -> drop_back  ?n t
  | `front -> drop_front ?n t
;;

let true_index_exn t i =
  let i_from_zero = i - t.apparent_front_index in
  if i_from_zero < 0 || length t <= i_from_zero then begin
    assert_not_empty t "Dequeue.true_index_exn";
    let apparent_front = apparent_front_index_when_not_empty t in
    let apparent_back  = apparent_back_index_when_not_empty t in
    invalid_argf "invalid index: %i for array with indices (%i,%i)"
      i apparent_front apparent_back ()
  end;
  let true_i = t.front_index + 1 + i_from_zero in
  if true_i >= t.arr_length
  then true_i - t.arr_length
  else true_i
;;

let get t i = t.arr.(true_index_exn t i)

let get_opt t i = try Some (get t i) with _ -> None

let set_exn t i v = t.arr.(true_index_exn t i) <- v

let to_array t =
  match peek_front t with
  | None -> [| |]
  | Some front ->
    let arr = Array.create ~len:(length t) front in
    ignore (fold t ~init:0 ~f:(fun i v -> arr.(i) <- v; i + 1));
    arr
;;

let of_array arr =
  let t = create ~initial_length:(Array.length arr + 1) () in
  Array.iter arr ~f:(fun v -> enqueue_back t v);
  t
;;

include Bin_prot.Utils.Make_iterable_binable1 (struct
  type nonrec 'a t = 'a t
  type 'a el = 'a with bin_io
  type 'a acc = 'a t

  let module_name  = Some "Core.Dequeue"
  let length       = length
  let iter t ~f    = iter t ~f
  let init n       = create ~initial_length:n ()
  let insert t x _ = enqueue_back t x; t
  let finish       = Fn.id
end)

let t_of_sexp f sexp = of_array (Array.t_of_sexp f sexp)
let sexp_of_t f t    = Array.sexp_of_t f (to_array t)

(* re-expose these here under a different name to avoid internal confusion *)
let back_index  = apparent_back_index
let front_index = apparent_front_index

let back_index_exn t =
  assert_not_empty t "Dequeue.back_index_exn";
  apparent_back_index_when_not_empty t
;;

let front_index_exn t =
  assert_not_empty t "Dequeue.front_index_exn";
  apparent_front_index_when_not_empty t
;;

module Binary_searchable = Binary_searchable.Make1 (struct
  type nonrec 'a t = 'a t
  let get t i = get t (front_index_exn t + i)
  let length = length
end)

(* The "stable" indices used in this module make the application of the
   [Binary_searchable] functor awkward.  We need to be sure to translate incoming
   positions from stable space to the expected 0 -> length - 1 space and then we need to
   translate them back on return. *)
let binary_search ?pos ?len t ~compare v =
  let pos =
    match pos with
    | None     -> None
    | Some pos -> Some (pos - t.apparent_front_index)
  in
  match Binary_searchable.binary_search ?pos ?len t ~compare v with
  | None                -> None
  | Some untranslated_i -> Some (t.apparent_front_index + untranslated_i)
;;

TEST_UNIT = begin
  let t = of_array [| 1; 2; 3; 4 |] in
  assert (binary_search t ~compare:Int.compare 2 = Some 1);
  assert (binary_search t ~compare:Int.compare 5 = None);
  assert (binary_search t ~compare:Int.compare 0 = None);
  assert (binary_search t ~pos:2 ~compare:Int.compare 2 = None);
  assert (binary_search t ~pos:2 ~compare:Int.compare 3 = Some 2);
  ignore (dequeue_front t);
  ignore (dequeue_front t);
  assert (binary_search t ~compare:Int.compare 2 = None);
  assert (binary_search t ~compare:Int.compare 3 = Some 2);
  assert (binary_search t ~compare:Int.compare 5 = None);
  assert (binary_search t ~compare:Int.compare 0 = None);
  assert (binary_search t ~pos:2 ~compare:Int.compare 2 = None);
  assert (binary_search t ~pos:2 ~compare:Int.compare 3 = Some 2);
end

TEST_MODULE = struct
  module type Dequeue_intf = sig
    type 'a t

    val create   : unit -> 'a t
    val enqueue  : 'a t -> [ `back | `front ] -> 'a -> unit
    val dequeue  : 'a t -> [ `back | `front ] -> 'a option
    val to_array : 'a t -> 'a array
    val clear    : 'a t -> unit
    val length   : 'a t -> int
    val iter     : 'a t -> f:('a -> unit) -> unit
    val fold'
      : 'a t -> [`front_to_back | `back_to_front] -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  end

  module That_dequeue : Dequeue_intf = struct
    type 'a t = 'a Doubly_linked.t

    let create = Doubly_linked.create

    let enqueue t back_or_front v =
      match back_or_front with
      | `back  -> ignore (Doubly_linked.insert_last t v)
      | `front -> ignore (Doubly_linked.insert_first t v)
    ;;

    let dequeue t back_or_front =
      match back_or_front with
      | `back  -> Doubly_linked.remove_last t
      | `front -> Doubly_linked.remove_first t
    ;;

    let fold' t dir ~init ~f =
      match dir with
      | `front_to_back -> Doubly_linked.fold t ~init ~f
      | `back_to_front -> Doubly_linked.fold_right t ~init ~f:(fun x acc -> f acc x)
    ;;

    let to_array = Doubly_linked.to_array
    let clear    = Doubly_linked.clear
    let iter     = Doubly_linked.iter
    let length   = Doubly_linked.length
  end

  module This_dequeue : Dequeue_intf = struct
    type nonrec 'a t = 'a t

    let create () = create ()
    let enqueue   = enqueue
    let dequeue   = dequeue
    let to_array  = to_array
    let clear     = clear
    let length    = length
    let iter      = iter
    let fold'     = fold'
  end

  let enqueue (t_a, t_b) back_or_front v =
    let start_a = This_dequeue.to_array t_a in
    let start_b = That_dequeue.to_array t_b in
    This_dequeue.enqueue t_a back_or_front v;
    That_dequeue.enqueue t_b back_or_front v;
    let end_a = This_dequeue.to_array t_a in
    let end_b = That_dequeue.to_array t_b in
    if end_a <> end_b then
      failwithf "enqueue transition failure of: %s -> %s vs. %s -> %s"
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_a))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_a))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_b))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_b))
        ()
  ;;

  let dequeue (t_a, t_b) back_or_front =
    let start_a = This_dequeue.to_array t_a in
    let start_b = That_dequeue.to_array t_b in
    let a,b =
      This_dequeue.dequeue t_a back_or_front, That_dequeue.dequeue t_b back_or_front
    in
    let end_a = This_dequeue.to_array t_a in
    let end_b = That_dequeue.to_array t_b in
    if a <> b || end_a <> end_b then
      failwithf "error in dequeue: %s (%s -> %s) <> %s (%s -> %s)"
        (Option.value ~default:"None" (Option.map a ~f:Int.to_string))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_a))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_a))
        (Option.value ~default:"None" (Option.map b ~f:Int.to_string))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t start_b))
        (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t end_b))
        ()
  ;;

  let clear (t_a, t_b) =
    This_dequeue.clear t_a;
    That_dequeue.clear t_b
  ;;

  let create () =
    let t_a = This_dequeue.create () in
    let t_b = That_dequeue.create () in
    (t_a, t_b)
  ;;

  let this_to_string this_t =
    Sexp.to_string (<:sexp_of<int array>> (This_dequeue.to_array this_t))
  ;;

  let that_to_string that_t =
    Sexp.to_string (<:sexp_of<int array>> (That_dequeue.to_array that_t))
  ;;

  let fold_check (t_a, t_b) dir =
    let make_list fold t =
      fold t dir ~init:[] ~f:(fun acc x -> x :: acc)
    in
    let this_l = make_list This_dequeue.fold' t_a in
    let that_l = make_list That_dequeue.fold' t_b in
    if this_l <> that_l then
      failwithf "error in fold:  %s (from %s) <> %s (from %s)"
        (Sexp.to_string (<:sexp_of<int list>> this_l))
        (this_to_string t_a)
        (Sexp.to_string (<:sexp_of<int list>> that_l))
        (that_to_string t_b)
        ()
  ;;

  let iter_check (t_a, t_b) =
    let make_rev_list iter t =
      let r = ref [] in iter t ~f:(fun x -> r := x :: !r); !r
    in
    let this_l = make_rev_list This_dequeue.iter t_a in
    let that_l = make_rev_list That_dequeue.iter t_b in
    if this_l <> that_l then
      failwithf "error in iter:  %s (from %s) <> %s (from %s)"
        (Sexp.to_string (<:sexp_of<int list>> this_l))
        (this_to_string t_a)
        (Sexp.to_string (<:sexp_of<int list>> that_l))
        (that_to_string t_b)
        ()
  ;;

  let length_check (t_a, t_b) =
    let this_len = This_dequeue.length t_a in
    let that_len = That_dequeue.length t_b in
    if this_len <> that_len then
      failwithf "error in length: %i (for %s) <> %i (for %s)"
        this_len (this_to_string t_a)
        that_len (that_to_string t_b)
        ()
  ;;

  let test () =
    let t = create () in

    let rec loop ops =
      if ops = 0 then begin
        let (t_a, t_b) = t in
        let arr_a = This_dequeue.to_array t_a in
        let arr_b = That_dequeue.to_array t_b in
        if arr_a <> arr_b then
          failwithf "dequeue final states not equal: %s vs. %s"
            (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t arr_a))
            (Sexp.to_string (Array.sexp_of_t Int.sexp_of_t arr_b))
            ()
      end else begin
        let r = Random.int 110 in
        begin
          if r < 20 then
            enqueue t `front (Random.int 10_000)
          else if r < 40 then
            enqueue t `back (Random.int 10_000)
          else if r < 50 then
            dequeue t `front
          else if r < 60 then
            dequeue t `back
          else if r < 70 then
            clear t
          else if r < 80 then
            fold_check t `front_to_back
          else if r < 90 then
            fold_check t `back_to_front
          else if r < 100 then
            iter_check t
          else
            length_check t
        end;
        loop (ops - 1)
      end
    in
    loop 1_000
  ;;

  TEST_UNIT = test ()
end
