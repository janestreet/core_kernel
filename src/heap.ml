open Std_internal
module Pointer = Pool.Pointer

(* This pool holds nodes that would be represented more traditionally as:

    type 'a t =
    | Empty
    | Heap of 'a * 'a t list

  We will represent them as a left-child, right-sibling tree in a triplet
  (value * left_child * right_sibling).  The left child and all right siblings
  of the left child form a linked list representing the subheaps of a given heap:

        A
       /
      B -> C -> D -> E -> F
     /         /         /
    G         H->I->J   K->L
*)

module Node : sig
  (* Exposing [private int] is a significant performance improvement, because it allows
     the compiler to skip the write barrier. *)
  type 'a t = private int

  module Pool : sig
    type 'a node = 'a t
    type 'a t

    val create  : min_size:int -> 'a t
    val is_full : 'a t -> bool
    val length  : 'a t -> int
    val grow    : 'a t -> 'a t
    val copy    : 'a t -> 'a node -> ('a -> 'b) -> ('b node * 'b t)
  end

  (** [allocate pool v] allocates a new node from the pool with no child or sibling *)
  val allocate : 'a Pool.t -> 'a -> 'a t

  (** [free pool t] frees [t] for reuse.  It is an error to access [t] after this. *)
  val free : 'a Pool.t -> 'a t -> unit

  (** a special [t] that represents the empty node *)
  val empty    : unit -> 'a t
  val is_empty : 'a t -> bool

  (** [value_exn pool t] return the value of [t], raise if [is_empty t] *)
  val value_exn : 'a Pool.t -> 'a t -> 'a

  (** [add_child pool t ~child] Add a child to [t], preserving existing children as
      siblings of [child]. [t] and [child] should not be empty and [child] should have no
      sibling. *)
  val add_child : 'a Pool.t -> 'a t -> child:'a t -> unit

  val disconnect_sibling : 'a Pool.t -> 'a t -> 'a t

  val disconnect_child : 'a Pool.t -> 'a t -> 'a t

  val child : 'a Pool.t -> 'a t -> 'a t

  val sibling : 'a Pool.t -> 'a t -> 'a t
end = struct
  type 'a node =
    ( 'a
    , 'a node Pointer.t
    , 'a node Pointer.t
    ) Pool.Slots.t3

  type 'a t = 'a node Pointer.t

  let empty    = Pointer.null
  let is_empty = Pointer.is_null

  let value_exn pool t =
    assert (not (is_empty t));
    Pool.get pool t Pool.Slot.t0
  ;;

  let allocate pool value = Pool.new3 pool value (empty ()) (empty ())

  let free pool node =
    Pool.unsafe_free pool node
  ;;

  let sibling pool node = Pool.get pool node Pool.Slot.t2

  let disconnect_sibling pool node =
    let sibling = sibling pool node in
    Pool.set pool node Pool.Slot.t2 (empty ());
    sibling
  ;;

  let child pool node = Pool.get pool node Pool.Slot.t1

  let disconnect_child pool node =
    let child = child pool node in
    Pool.set pool node Pool.Slot.t1 (empty ());
    child
  ;;

  let add_child pool node ~child:new_child =
    (* assertions we would make, but for speed:
      assert (not (is_empty node));
      assert (not (is_empty new_child));
      assert (is_empty (sibling pool new_child));
    *)
    let current_child = disconnect_child pool node in
    (* add [new_child] to the list of [node]'s children (which may be empty) *)
    Pool.set pool new_child Pool.Slot.t2 current_child;
    Pool.set pool node      Pool.Slot.t1 new_child;
  ;;

  module Pool = struct
    type 'a t = 'a node Pool.t
    type nonrec 'a node = 'a node Pointer.t

    let create (type a) ~min_size:capacity : a t =
      Pool.create Pool.Slots.t3 ~capacity
        ~dummy:((Obj.magic () : a), Pointer.null (), Pointer.null ())
    ;;

    let is_full t = Pool.is_full t
    let length  t = Pool.length t
    let grow    t = Pool.grow t

    let copy t start transform =
      let t' = create ~min_size:(Pool.capacity t) in
      let copy_node node to_visit =
        if is_empty node
        then (empty (), to_visit)
        else begin
          let new_node = allocate t' (transform (value_exn t node)) in
          let to_visit =
               (new_node, Pool.Slot.t1, child t node)
            :: (new_node, Pool.Slot.t2, sibling t node)
            :: to_visit
          in
          (new_node, to_visit)
        end
      in
      let rec loop to_visit =
        match to_visit with
        | [] -> ()
        | (node_to_update, slot, node_to_copy) :: rest ->
          let new_node, to_visit = copy_node node_to_copy rest in
          Pool.set t' node_to_update slot new_node;
          loop to_visit
      in
      let new_start, to_visit = copy_node start [] in
      loop to_visit;
      (new_start, t')
    ;;
  end
end

module T = struct
  (* Every node input to a function is assumed to have no sibling and any node output from
     a function will have no sibling.  This is because a node is really the head of a node
     list, but we always want to work with node options, corresponding to the type
     t defined as

     type 'a node = Node of 'a * 'a node list
     type 'a t = 'a node option
  *)

  type 'a t = {
    (* cmp is placed first to short-circuit polymorphic compare *)
    cmp          : 'a -> 'a -> int;
    mutable pool : 'a Node.Pool.t;
    (* invariant:  [heap] never has a sibling *)
    mutable heap : 'a Node.t;
  }

  let create ?(min_size = 1) ~cmp () =
    { cmp;
      pool = Node.Pool.create ~min_size;
      heap = Node.empty ();
    }
  ;;

  let copy t =
    let heap, pool = Node.Pool.copy t.pool t.heap ident in
    {
      cmp = t.cmp;
      pool;
      heap;
    }
  ;;

  let allocate t v =
    if Node.Pool.is_full t.pool then begin
      t.pool <- Node.Pool.grow t.pool;
    end;
    Node.allocate t.pool v
  ;;

  (* translation:
     match heap1, heap2 with
     | None, h | h, None -> h
     | Some (Node (v1, children1)), Some (Node (v2, children2)) ->
       if v1 < v2
       then Some (Node (v1, heap2 :: children1))
       else Some (Node (v2, heap1 :: children2))
  *)
  let merge t heap1 heap2 =
    if Node.is_empty heap1 then
      heap2
    else if Node.is_empty heap2 then
      heap1
    else
      let add_child t heap ~child =
        Node.add_child t.pool heap ~child;
        heap
      in
      let v1 = Node.value_exn t.pool heap1 in
      let v2 = Node.value_exn t.pool heap2 in
      if t.cmp v1 v2 < 0
      then add_child t heap1 ~child:heap2
      else add_child t heap2 ~child:heap1
  ;;

  let top_exn t =
    if Node.is_empty t.heap
    then failwith "Heap.top_exn called on an empty heap"
    else Node.value_exn t.pool t.heap
  ;;

  let top t = try Some (top_exn t) with _ -> None

  let add t v =
    t.heap <- merge t t.heap (allocate t v)
  ;;

  (* [merge_pairs] takes a list of heaps and merges consecutive pairs, reducing the list
     of length n to n/2.  Then it merges the merged pairs into a single heap.  One
     intuition is that this is somewhat like building a single level of a binary tree.

     The output heap does not contain the value that was at the root of the input heap.
  *)
  (* translation:
     match t.heap with
     | Node (_, children) ->
       let rec loop acc = function
         | [] -> acc
         | [head] -> head :: acc
         | head :: next1 :: next2 -> loop (merge head next1 :: acc) next2
       in
       match loop [] children with
       | [] -> None
       | [h] -> Some h
       | x :: xs -> Some (List.fold xs ~init:x ~f:merge)
  *)
  let merge_pairs t =
    let rec loop acc head =
      if Node.is_empty head then
        acc
      else
        let next1 = Node.disconnect_sibling t.pool head in
        if Node.is_empty next1 then
          head :: acc
        else
          let next2 = Node.disconnect_sibling t.pool next1 in
          loop (merge t head next1 :: acc) next2
    in
    let head = Node.disconnect_child t.pool t.heap in
    match loop [] head with
    | []      -> Node.empty ()
    | [h]     -> h
    | x :: xs -> List.fold xs ~init:x ~f:(fun acc heap -> merge t acc heap)
  ;;

  let remove_top t =
    if not (Node.is_empty t.heap) then begin
      let current_heap = t.heap in
      t.heap <- merge_pairs t;
      Node.free t.pool current_heap
    end
  ;;

  let pop_exn t =
    let r = top_exn t in
    remove_top t;
    r
  ;;

  let pop t = try Some (pop_exn t) with _ -> None

  let pop_if t f =
    match top t with
    | None -> None
    | Some v when f v ->
      remove_top t;
      Some v
    | Some _ -> None
  ;;

  (* pairing heaps are not balanced trees, and therefore we can't rely on a balance
     property to stop ourselves from overflowing the stack. *)
  let fold t ~init ~f =
    let pool = t.pool in
    let rec loop acc to_visit =
      match to_visit with
      | [] -> acc
      | node :: rest ->
        if Node.is_empty node then
          loop acc rest
        else begin
          let to_visit = (Node.sibling pool node) :: (Node.child pool node) :: rest in
          loop (f acc (Node.value_exn pool node)) to_visit
        end
    in
    loop init [t.heap]
  ;;

  (* almost identical to fold, copied for speed purposes *)
  let iter t ~f =
    let pool = t.pool in
    let rec loop to_visit =
      match to_visit with
      | [] -> ()
      | node :: rest ->
        if Node.is_empty node then
          loop rest
        else begin
          f (Node.value_exn pool node);
          let to_visit = (Node.sibling pool node) :: (Node.child pool node) :: rest in
          loop to_visit
        end
    in
    loop [t.heap]
  ;;

  module C = Container.Make (struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
  end)

  (* we can do better than the O(n) of [C.length] *)
  let length t = Node.Pool.length t.pool
  let is_empty t = Node.is_empty t.heap

  let mem      = C.mem
  let exists   = C.exists
  let for_all  = C.for_all
  let count    = C.count
  let sum      = C.sum
  let find     = C.find
  let find_map = C.find_map
  let to_list  = C.to_list
  let to_array = C.to_array
  let min_elt  = C.min_elt
  let max_elt  = C.max_elt

  let of_array arr ~cmp =
    let t = create ~min_size:(Array.length arr) ~cmp () in
    Array.iter arr ~f:(fun v -> add t v);
    t
  ;;

  let of_list l ~cmp = of_array (Array.of_list l) ~cmp

  let sexp_of_t f t = Array.sexp_of_t f (to_array t)

  let%test_module _ = (module struct
    let data = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    let t = of_list data ~cmp:Int.compare
    (* pop the zero at the top to force some heap structuring.  This does not touch the
       sum. *)
    let _ = pop t
    let list_sum      = List.fold data ~init:0 ~f:(fun sum v -> sum + v)
    let heap_fold_sum = fold t ~init:0 ~f:(fun sum v -> sum + v)
    let heap_iter_sum =
      let r = ref 0 in
      iter t ~f:(fun v -> r := !r + v);
      !r
    let%test _ = Int.(=) list_sum heap_fold_sum
    let%test _ = Int.(=) list_sum heap_iter_sum
  end)

end

module Removable = struct
  module Elt = struct
    (* We could use an extra word to hold a pointer/representative id of the heap this
       token belongs to to prevent using this token with the wrong heap in the
       remove/update functions below.  It's (currently) deemed not worth the cost in
       memory/speed. *)
    type 'a t = 'a option ref [@@deriving sexp_of]

    let create v    = ref (Some v)
    let value_exn t = Option.value_exn !t
  end

  type 'a t = {
    heap           : 'a Elt.t T.t;
    mutable length : int;
  } [@@deriving sexp_of]

  let remove t token =
    match !token with
    | None   -> ()
    | Some _ ->
      token := None;
      t.length <- t.length - 1
  ;;

  let augment_cmp cmp =
    (fun v1 v2 ->
      match !v1, !v2 with
      | None, None       -> 0
      | None, Some _     -> -1
      | Some _, None     -> 1
      | Some v1, Some v2 -> cmp v1 v2)
  ;;

  let create ?min_size ~cmp () =
    let cmp = augment_cmp cmp in
    { length = 0; heap = T.create ?min_size ~cmp () }
  ;;

  let copy t =
    let current_heap = t.heap in
    let replace_token v = ref !v in
    let heap, pool = Node.Pool.copy current_heap.T.pool current_heap.T.heap replace_token in
    {t with heap = {T.cmp = current_heap.T.cmp; pool; heap}}
  ;;

  let add_removable t v =
    let token = Elt.create v in
    T.add t.heap token;
    t.length <- t.length + 1;
    token
  ;;

  let update t token v =
    remove t token;
    add_removable t v
  ;;

  let add t v = ignore (add_removable t v : _ Elt.t)

  let rec clear_deleted_tokens t =
    if not (Node.is_empty t.heap.T.heap) then
      match !(T.top_exn t.heap) with
      | Some _ -> ()
      | None   ->
        T.remove_top t.heap;
        clear_deleted_tokens t;
  ;;

  let fold t ~init ~f =
    T.fold t.heap ~init ~f:(fun acc token ->
      match !token with
      | None   -> acc
      | Some v -> f acc v)
  ;;

  let iter t ~f =
    T.iter t.heap ~f:(fun token ->
      match !token with
      | None   -> ()
      | Some v -> f v)
  ;;

  let find_elt t ~f =
    T.find t.heap ~f:(fun token ->
      match !token with
      | None   -> false
      | Some v -> f v)
  ;;

  module C = Container.Make (struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
  end)

  let length t = t.length
  let is_empty t = t.length = 0

  let to_array = C.to_array
  let to_list  = C.to_list
  let find_map = C.find_map
  let find     = C.find
  let count    = C.count
  let sum      = C.sum
  let for_all  = C.for_all
  let exists   = C.exists
  let mem      = C.mem
  let min_elt  = C.min_elt
  let max_elt  = C.max_elt

  let of_array arr ~cmp =
    let t = create ~min_size:(Array.length arr) ~cmp () in
    Array.iter arr ~f:(fun v -> add t v);
    t
  ;;

  let of_list l ~cmp = of_array (Array.of_list l) ~cmp

  let top_exn t = clear_deleted_tokens t; Elt.value_exn (T.top_exn t.heap)
  let top     t = try Some (top_exn t) with _ -> None

  let pop_exn t =
    clear_deleted_tokens t;
    let token = T.pop_exn t.heap in
    let v     = Elt.value_exn token in
    remove t token;
    v
  ;;

  let pop t = try Some (pop_exn t) with _ -> None

  let remove_top t =
    clear_deleted_tokens t;
    begin match T.top t.heap with
    | Some _ -> ignore (pop_exn t)
    | None   -> ()
    end;
  ;;

  let pop_if t f =
    clear_deleted_tokens t;
    match T.pop_if t.heap (fun v -> f (Elt.value_exn v)) with
    | None       -> None
    | Some token ->
      let v = Elt.value_exn token in
      remove t token;
      Some v
  ;;
end

include T

let%test_module _ = (module struct
  module type Heap_intf = sig
    type 'a t [@@deriving sexp_of]

    val create     : cmp:('a -> 'a -> int) -> 'a t
    val add        : 'a t -> 'a -> unit
    val pop        : 'a t -> 'a option
    val length     : 'a t -> int
    val top        : 'a t -> 'a option
    val remove_top : 'a t -> unit
    val to_list    : 'a t -> 'a list
  end

  module That_heap : Heap_intf = struct
    type 'a t = {
      cmp          : 'a -> 'a -> int;
      mutable heap : 'a list;
    }

    let sexp_of_t sexp_of_v t = List.sexp_of_t sexp_of_v t.heap

    let create ~cmp = { cmp; heap = [] }
    let add t v = t.heap <- List.sort ~cmp:t.cmp (v :: t.heap)

    let pop t =
      match t.heap with
      | [] -> None
      | x :: xs ->
        t.heap <- xs;
        Some x
    ;;

    let length t     = List.length t.heap
    let top t        = List.hd t.heap
    let remove_top t = match t.heap with [] -> () | _ :: xs -> t.heap <- xs
    let to_list t    = t.heap
  end

  module This_heap : Heap_intf = struct
    type nonrec 'a t = 'a t [@@deriving sexp_of]

    let create ~cmp = create ~cmp ()
    let add         = add
    let pop         = pop
    let length      = length
    let top         = top
    let remove_top  = remove_top
    let to_list     = to_list
  end

  let this_to_string this = Sexp.to_string (This_heap.sexp_of_t Int.sexp_of_t this)
  let that_to_string that = Sexp.to_string (That_heap.sexp_of_t Int.sexp_of_t that)

  let length_check (t_a, t_b) =
    let this_len = This_heap.length t_a in
    let that_len = That_heap.length t_b in
    if this_len <> that_len then
      failwithf "error in length: %i (for %s) <> %i (for %s)"
        this_len (this_to_string t_a)
        that_len (that_to_string t_b) ()
  ;;

  let create () =
    let cmp = Int.compare in
    (This_heap.create ~cmp, That_heap.create ~cmp)
  ;;

  let add (this_t, that_t) v =
    This_heap.add this_t v;
    That_heap.add that_t v;
    length_check (this_t, that_t)
  ;;

  let pop (this_t, that_t) =
    let res1 = This_heap.pop this_t in
    let res2 = That_heap.pop that_t in
    if res1 <> res2 then
      failwithf "pop results differ (%s, %s)"
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res2)) ()
  ;;

  let top (this_t, that_t) =
    let res1 = This_heap.top this_t in
    let res2 = That_heap.top that_t in
    if res1 <> res2 then
      failwithf "top results differ (%s, %s)"
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
        (Option.value ~default:"None" (Option.map ~f:Int.to_string res2)) ()
  ;;

  let remove_top (this_t, that_t) =
    This_heap.remove_top this_t;
    That_heap.remove_top that_t;
    length_check (this_t, that_t)
  ;;

  let internal_check (this_t, that_t) =
    let this_list = List.sort ~cmp:Int.compare (This_heap.to_list this_t) in
    let that_list = List.sort ~cmp:Int.compare (That_heap.to_list that_t) in
    assert (this_list = that_list)
  ;;

  let test_dual_ops () =
    let t = create () in

    let rec loop ops =
      if ops = 0 then ()
      else begin
        let r = Random.int 100 in
        begin
          if r < 40 then
            add t (Random.int 100_000)
          else if r < 70 then
            pop t
          else if r < 80 then
            top t
          else if r < 90 then
            remove_top t
          else
            internal_check t
        end;
        loop (ops - 1)
      end
    in
    loop 1_000
  ;;

  let%test_unit _ = test_dual_ops ()
end)

let test_copy () =
  let sum t = fold t ~init:0 ~f:(fun acc i -> acc + i) in
  let t = create ~cmp:Int.compare () in
  for i = 1 to 100 do
    add t i;
    if i % 10 = 0
    (* We need to pop from time to time to trigger the amortized tree reorganizations.  If
       we don't do this the resulting structure is just a linked list and the copy
       function is not flexed as completely as it should be. *)
    then begin
      ignore (pop t);
      add t i
    end
  done;
  let t' = copy t in
  assert (sum t = sum t');
  assert (to_list t = to_list t');
  add t (-100);
  assert (sum t = sum t' - 100);
;;
let%test_unit _ = test_copy ()

let test_removable_copy () =
  let sum t = Removable.fold t ~init:0 ~f:(fun acc i -> acc + i) in
  let t = Removable.create ~cmp:Int.compare () in
  for i = 1 to 99 do
    Removable.add t i;
    if i % 10 = 0
    (* We need to pop from time to time to trigger the amortized tree reorganizations.  If
       we don't do this the resulting structure is just a linked list and the copy
       function is not flexed as completely as it should be. *)
    then begin
      ignore (Removable.pop t);
      Removable.add t i
    end
  done;
  let token = Removable.add_removable t 100 in
  let t' = Removable.copy t in
  assert (sum t = sum t');
  Removable.remove t token;
  assert (sum t = sum t' - 100);
;;
let%test_unit _ = test_removable_copy ()

let test_removal () =
  let t = Removable.create ~cmp:Int.compare () in
  let tokens = ref [] in
  for i = 1 to 10_000 do
    tokens := Removable.add_removable t i :: !tokens;
  done;
  List.iter !tokens ~f:(fun token ->
    if Removable.Elt.value_exn token % 2 <> 0
    then Removable.remove t token);
  let rec loop count =
    match Removable.pop t with
    | None   -> assert (count = 10_000 / 2);
    | Some v ->
      assert ((1 + count) * 2 = v);
      loop (count + 1)
  in
  loop 0
;;
let%test_unit _ = test_removal ()

let test_ordering () =
  let t = create ~cmp:Int.compare () in
  for _ = 1 to 10_000 do
    add t (Random.int 100_000);
  done;
  let rec loop last =
    match pop t with
    | None -> ()
    | Some v ->
      assert (v >= last);
      loop v
  in
  loop (-1)
;;
let%test_unit _ = test_ordering ()

let%test_unit _ = ignore (of_array [| |] ~cmp:Int.compare)

let%bench_fun "pop_insert_with_existing_heap" [@indexed initial_size = [1; 100; 10_000; 1_000_000]] =
  let a = Array.init initial_size ~f:(fun _ -> Random.int 100_000) in
  let h1 = of_array ~cmp:Int.compare a in
  (fun () ->
     let e = pop_exn h1 in
     add h1 e;
  )
