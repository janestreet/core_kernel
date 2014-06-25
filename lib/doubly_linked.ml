module List = Core_list

(* INVARIANT: This exception is raised if a list is mutated during a pending iteration.

   This invariant is guaranteed by the Header and Elt modules in conjunction.  All
   downstream code in this module need not be concerned with this invariant.
*)
exception Attempt_to_mutate_list_during_iteration

let phys_equal = (==)

module Header : sig
  type t
  val create : unit -> t
  val length : t -> int
  val equal : t -> t -> bool
  val incr_length : by:int -> t -> unit
  val check_no_pending_iterations : t -> unit
  val with_iteration : t -> (unit -> 'a) -> 'a
  val merge : t -> t -> [ `Same_already | `Merged ]
end = struct

  type s = {
    mutable length : int;
    mutable pending_iterations : int;
  }

  type t = s Union_find.t

  let create () = Union_find.create { length = 1; pending_iterations = 0; }

  let equal (t1 : t) t2 = Union_find.same_class t1 t2

  let length t = (Union_find.get t).length

  let union_find_get__check_no_pending_iterations t =
    let s = Union_find.get t in
    if s.pending_iterations > 0
    then raise Attempt_to_mutate_list_during_iteration
    else s

  let check_no_pending_iterations t =
    ignore (union_find_get__check_no_pending_iterations t : s)

  let incr_length ~by:n t =
    let s = union_find_get__check_no_pending_iterations t in
    s.length <- s.length + n

  let with_iteration t f =
    let s = Union_find.get t in
    s.pending_iterations <- s.pending_iterations + 1;
    let res = Result.try_with f in
    s.pending_iterations <- s.pending_iterations - 1;
    match res with
    | Result.Ok v -> v
    | Result.Error e -> raise e

  let merge (t1 : t) t2 =
    if Union_find.same_class t1 t2 then `Same_already else begin
      let n1 = (union_find_get__check_no_pending_iterations t1).length in
      let n2 = (union_find_get__check_no_pending_iterations t2).length in
      with_iteration t1 (fun () ->
        with_iteration t2 (fun () ->
          Union_find.union t1 t2;
          Union_find.set t1 {
            length = n1 + n2;
            pending_iterations = 0;
          }));
      `Merged
    end

end

module Elt : sig
  type 'a t with sexp_of
  val header : 'a t -> Header.t
  val equal : 'a t -> 'a t -> bool
  val create : 'a -> 'a t
  val value : 'a t -> 'a
  val unlink : 'a t -> unit
  val split_or_splice_before : 'a t -> 'a t -> unit
  val split_or_splice_after  : 'a t -> 'a t -> unit
  val insert_after : 'a t -> 'a -> 'a t
  val insert_before : 'a t -> 'a -> 'a t
  val unlink_before : 'a t -> 'a t
  val next : 'a t -> 'a t
  val prev : 'a t -> 'a t
end = struct

  type 'a t = {
    value : 'a;
    mutable prev : 'a t;
    mutable next : 'a t;
    mutable header : Header.t;
  }

  let equal = phys_equal

  let next t = t.next
  let prev t = t.prev
  let header t = t.header

  let create_aux v header =
    let rec t = {
      value = v;
      prev = t;
      next = t;
      header = header;
    } in
    t

  let is_singleton t = equal t t.prev

  let sexp_of_t sexp_of_a t = sexp_of_a t.value

  let create v = create_aux v (Header.create ())

  let value t = t.value

  (*
    [split_or_splice] is sufficient as the lone primitive for
    accomplishing all pointer updates on cyclic loops of list nodes.
    It takes two "gaps" between adjacent linked list nodes.  If the gaps
    point into the same list, the result is that it will be split into
    two lists afterwards.  If the gaps point into different lists, the
    result is that they will be spliced together into one list afterwards.

      Before                      After
          -----+        +-----         -----+               +-----
             A |  <-->  | B               A |  <---   --->  | B
          -----+        +-----         -----+      \ /      +-----
                                                    X
          -----+        +-----         -----+      / \      +-----
             C |  <-->  | D               C |  <---   --->  | D
          -----+        +-----         -----+               +-----
  *)

  let unsafe_split_or_splice ~prev1:a ~next1:b ~prev2:c ~next2:d =
    a.next <- d; d.prev <- a;
    c.next <- b; b.prev <- c

  let unsafe_split_or_splice_after t1 t2 =
    unsafe_split_or_splice
     ~next1:t1.next
     ~prev1:t1.next.prev
     ~next2:t2.next
     ~prev2:t2.next.prev

  let unsafe_split_or_splice_before t1 t2 =
    unsafe_split_or_splice
     ~prev1:t1.prev
     ~next1:t1.prev.next
     ~prev2:t2.prev
     ~next2:t2.prev.next

  let check_two_nodes_no_pending_iterations t1 t2 =
    Header.check_no_pending_iterations t1.header;
    if not (Header.equal t1.header t2.header) then
      Header.check_no_pending_iterations t2.header

  (* We redefine safe versions for export *)
  let split_or_splice_after t1 t2 =
    check_two_nodes_no_pending_iterations t1 t2;
    unsafe_split_or_splice_after t1 t2

  let split_or_splice_before t1 t2 =
    check_two_nodes_no_pending_iterations t1 t2;
    unsafe_split_or_splice_before t1 t2

  let insert_before t v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_before t node;
    node

  let insert_after t v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_after t node;
    node

  let unlink_before t =
    let node = t.prev in
    if is_singleton node then node else begin
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_before t node;
      node.header <- Header.create ();
      node
    end

  let unlink_after t =
    let node = t.next in
    if is_singleton node then node else begin
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_after t node;
      node.header <- Header.create ();
      node
    end

  let unlink t = ignore (unlink_after t.prev)

end

type 'a t = 'a Elt.t option ref

let invariant t =
  match !t with
  | None -> ()
  | Some head ->
    let header = Elt.header head in
    let rec loop n elt =
      let next_elt = Elt.next elt in
      let prev_elt = Elt.prev elt in
      assert (Elt.equal elt (Elt.prev next_elt));
      assert (Elt.equal elt (Elt.next prev_elt));
      assert (Header.equal (Elt.header elt) header);
      if Elt.equal next_elt head then n else loop (n + 1) next_elt
    in
    let len = loop 1 head in
    assert (len = Header.length header)

let create (type a) () : a t = ref None

let equal (t : _ t) t' = phys_equal t t'

let of_list = function
  | [] -> create ()
  | x :: xs ->
    let first = Elt.create x in
    let _last = List.fold xs ~init:first ~f:Elt.insert_after in
    ref (Some first)

let fold_elt t ~init ~f =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration (Elt.header first) (fun () ->
      let rec loop acc elt =
        let acc = f acc elt in
        let next = Elt.next elt in
        if phys_equal next first then acc else loop acc next
      in
      loop init first)
;;

let iter_elt t ~f = fold_elt t ~init:() ~f:(fun () elt -> f elt)

TEST_UNIT =
  List.iter
    [ [];
      [ 1 ];
      [ 2; 3 ];
    ]
    ~f:(fun l ->
      let sum = ref 0 in
      iter_elt (of_list l) ~f:(fun elt -> sum := !sum + Elt.value elt);
      assert (!sum = List.fold l ~init:0 ~f:(+)))
;;

open With_return

let find_elt t ~f =
  with_return (fun r ->
    fold_elt t ~init:() ~f:(fun () elt ->
      if f (Elt.value elt) then r.return (Some elt));
    None)

(* this function is lambda lifted for performance, to make direct recursive calls instead
   of calls through its closure. It also avoids the initial closure allocation. *)
let rec iter_loop first f elt =
  f (Elt.value elt);
  let next = Elt.next elt in
  if not (phys_equal next first) then iter_loop first f next

let iter t ~f =
  match !t with
  | None -> ()
  | Some first ->
    Header.with_iteration (Elt.header first) (fun () -> iter_loop first f first)

module C = Container.Make (struct
  type 'a t_ = 'a t
  type 'a t = 'a t_
  let fold t ~init ~f = fold_elt t ~init ~f:(fun acc elt -> f acc (Elt.value elt))
  let iter = Some iter
end)

let count    = C.count
let exists   = C.exists
let find     = C.find
let find_map = C.find_map
let fold     = C.fold
let for_all  = C.for_all
let mem      = C.mem
let to_array = C.to_array

let unchecked_iter t ~f =
  match !t with
  | None -> ()
  | Some first ->
    let rec loop t f elt =
      f (Elt.value elt);
      let next = Elt.next elt in
      match !t with (* the first element of the bag may have been changed by [f] *)
      | None -> ()
      | Some first -> if not (phys_equal first next) then loop t f next
    in
    loop t f first

let is_empty t = Option.is_none !t (* more efficient than what Container.Make returns *)

let fold_right t ~init ~f =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration (Elt.header first) (fun () ->
      let rec loop acc elt =
        let prev = Elt.prev elt in
        let acc = f (Elt.value prev) acc in
        if phys_equal prev first
        then acc
        else loop acc prev
      in
      loop init first
    )

let to_list t = fold_right t ~init:[] ~f:(fun x tl -> x :: tl)

let length t =
  match !t with
  | None -> 0
  | Some first -> Header.length (Elt.header first)

let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
let t_of_sexp a_of_sexp s = of_list (List.t_of_sexp a_of_sexp s)

let copy t = of_list (to_list t)

let clear t = (t := None)

exception Transfer_src_and_dst_are_same_list

let transfer ~src ~dst =
  if phys_equal src dst then raise Transfer_src_and_dst_are_same_list;
  match !src with
  | None -> ()
  | Some src_head ->
    match !dst with
    | None ->
      dst := Some src_head;
      clear src
    | Some dst_head ->
      match Header.merge (Elt.header src_head) (Elt.header dst_head) with
      | `Same_already ->
        raise Transfer_src_and_dst_are_same_list
      | `Merged ->
        Elt.split_or_splice_before dst_head src_head;
        clear src

let filter_inplace t ~f =
  let to_remove =
    List.rev
      (fold_elt t ~init:[] ~f:(fun elts elt ->
        if f (Elt.value elt) then elts else elt :: elts))
  in
  List.iter to_remove ~f:(fun elt ->
    begin
      match !t with
      | None -> ()
      | Some head ->
        if Elt.equal head elt then begin
          let next_elt = Elt.next elt in
          t := if Elt.equal head next_elt then None else Some next_elt
        end
    end;
    Elt.unlink elt)

exception Elt_does_not_belong_to_list

let first_elt t = !t
let last_elt t = Option.map ~f:Elt.prev !t

let first t = Option.map ~f:Elt.value (first_elt t)
let last  t = Option.map ~f:Elt.value (last_elt  t)

let is_first t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.equal elt first
    else
      raise Elt_does_not_belong_to_list

let is_last t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt) then begin
      let last = Elt.prev first in
      Elt.equal elt last
    end else
      raise Elt_does_not_belong_to_list

let prev t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first then
      None
    else if Header.equal (Elt.header first) (Elt.header elt) then
      Some (Elt.prev elt)
    else
      raise Elt_does_not_belong_to_list

let next t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    let last = Elt.prev first in
    if Elt.equal elt last then
      None
    else if Header.equal (Elt.header first) (Elt.header elt) then
      Some (Elt.next elt)
    else
      raise Elt_does_not_belong_to_list

let insert_after t elt v =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.insert_after elt v
    else
      raise Elt_does_not_belong_to_list

let insert_before t elt v =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first then begin
      let new_elt = Elt.insert_before first v in
      t := Some new_elt;
      new_elt
    end else if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.insert_before elt v
    else
      raise Elt_does_not_belong_to_list

let insert_empty t v =
  let new_elt = Elt.create v in
  t := Some new_elt;
  new_elt

let insert_last t v =
  match !t with
  | None -> insert_empty t v
  | Some first -> Elt.insert_before first v

let insert_first t v =
  match !t with
  | None -> insert_empty t v
  | Some first ->
    let new_elt = Elt.insert_before first v in
    t := Some new_elt;
    new_elt

let remove_last t =
  match !t with
  | None -> None
  | Some first ->
    let last = Elt.unlink_before first in
    if Elt.equal first last then t := None;
    Some (Elt.value last)

let remove_first t =
  match !t with
  | None -> None
  | Some first ->
    let second = Elt.next first in
    ignore (Elt.unlink first);
    t := if Elt.equal first second then None else Some second;
    Some (Elt.value first)

let remove t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first then
      ignore (remove_first t)
    else if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.unlink elt
    else
      raise Elt_does_not_belong_to_list

exception Invalid_move__elt_equals_anchor

let move_before t elt ~anchor =
  if Elt.equal anchor elt then
    raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt) then
    match !t with
    | None -> raise Elt_does_not_belong_to_list
    | Some first ->
      if Header.equal (Elt.header first) (Elt.header elt) then begin
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        let first =
          if Elt.equal first elt then begin
            t := Some after_elt;
            after_elt
          end else
            first
        in
        (* splice [elt] in before [anchor] *)
        Elt.split_or_splice_before anchor elt;
        if Elt.equal first anchor then t := Some elt;
      end else
        raise Elt_does_not_belong_to_list
  else
    raise Elt_does_not_belong_to_list

let move_to_front t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if not (Elt.equal elt first) then move_before t elt ~anchor:first

let move_after t elt ~anchor =
  if Elt.equal anchor elt then
    raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt) then
    match !t with
    | None -> raise Elt_does_not_belong_to_list
    | Some first ->
      if Header.equal (Elt.header first) (Elt.header elt) then begin
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        if Elt.equal first elt then t := Some after_elt;
        (* splice [elt] in after [anchor] *)
        Elt.split_or_splice_after anchor elt
      end else
        raise Elt_does_not_belong_to_list
  else
    raise Elt_does_not_belong_to_list

let move_to_back t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    let last = Elt.prev first in
    if not (Elt.equal elt last) then move_after t elt ~anchor:last

TEST_MODULE "move functions" = struct

  let n = 5

  let test k expected =
    let t = create () in
    let a = Array.init n (fun i -> insert_last t i) in
    k t a;
    invariant t;
    assert (length t = n);
    let observed = to_list t in
    if observed <> expected then begin
      let open Sexplib.Conv in
      Error.failwiths "mismatch"
        (`Expected expected, `Observed observed)
        <:sexp_of< [`Expected of int list] *
                   [`Observed of int list] >>
    end

  TEST_UNIT = test (fun _ _ -> ()) [0; 1; 2; 3; 4]

  TEST_UNIT = test (fun t a -> move_to_front t a.(4)) [4; 0; 1; 2; 3]
  TEST_UNIT = test (fun t a -> move_to_front t a.(3)) [3; 0; 1; 2; 4]
  TEST_UNIT = test (fun t a -> move_to_front t a.(2)) [2; 0; 1; 3; 4]
  TEST_UNIT = test (fun t a -> move_to_front t a.(1)) [1; 0; 2; 3; 4]
  TEST_UNIT = test (fun t a -> move_to_front t a.(0)) [0; 1; 2; 3; 4]

  TEST_UNIT = test (fun t a -> move_to_back  t a.(0)) [1; 2; 3; 4; 0]
  TEST_UNIT = test (fun t a -> move_to_back  t a.(1)) [0; 2; 3; 4; 1]
  TEST_UNIT = test (fun t a -> move_to_back  t a.(2)) [0; 1; 3; 4; 2]
  TEST_UNIT = test (fun t a -> move_to_back  t a.(3)) [0; 1; 2; 4; 3]
  TEST_UNIT = test (fun t a -> move_to_back  t a.(4)) [0; 1; 2; 3; 4]

  TEST_UNIT = test (fun t a -> move_before t a.(2) ~anchor:a.(1)) [0; 2; 1; 3; 4]
  TEST_UNIT = test (fun t a -> move_before t a.(2) ~anchor:a.(0)) [2; 0; 1; 3; 4]
  TEST_UNIT = test (fun t a -> move_before t a.(1) ~anchor:a.(0)) [1; 0; 2; 3; 4]
  TEST_UNIT = test (fun t a -> move_before t a.(0) ~anchor:a.(2)) [1; 0; 2; 3; 4]
  TEST_UNIT = test (fun t a -> move_before t a.(0) ~anchor:a.(1)) [0; 1; 2; 3; 4]
  TEST_UNIT = test (fun t a -> move_before t a.(3) ~anchor:a.(2)) [0; 1; 3; 2; 4]
  TEST_UNIT = test (fun t a -> move_before t a.(2) ~anchor:a.(3)) [0; 1; 2; 3; 4]

  TEST_UNIT = test (fun t a -> move_after  t a.(1) ~anchor:a.(3)) [0; 2; 3; 1; 4]
  TEST_UNIT = test (fun t a -> move_after  t a.(0) ~anchor:a.(2)) [1; 2; 0; 3; 4]
  TEST_UNIT = test (fun t a -> move_after  t a.(1) ~anchor:a.(4)) [0; 2; 3; 4; 1]
  TEST_UNIT = test (fun t a -> move_after  t a.(3) ~anchor:a.(2)) [0; 1; 2; 3; 4]
  TEST_UNIT = test (fun t a -> move_after  t a.(2) ~anchor:a.(3)) [0; 1; 3; 2; 4]
end

TEST =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 15 in
  try
    remove t2 elt; false
  with
    Elt_does_not_belong_to_list -> true

TEST =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 14 in
  let _   = insert_first t2 13 in
  try
    remove t2 elt; false
  with
    Elt_does_not_belong_to_list -> true

TEST_MODULE "unchecked_iter" = struct
  let b = of_list [0; 1; 2; 3; 4]
  let element b n =
    Option.value_exn (find_elt b ~f:(fun value -> value = n))
  let remove b n =
    remove b (element b n)
  let insert_after b n_find n_add =
    ignore (insert_after b (element b n_find) n_add)
  let to_list f =
    let r = ref [] in
    let b = copy b in
    unchecked_iter b ~f:(fun n ->
      r := n :: !r;
      f b n;
    );
    List.rev !r
  TEST = to_list (fun _ _ -> ()) = [0; 1; 2; 3; 4]
  TEST = to_list (fun b x -> if x = 0 then remove b 1) = [0; 2; 3; 4]
  TEST = to_list (fun b x -> if x = 1 then remove b 0) = [0; 1; 2; 3; 4]
  TEST = to_list (fun b x -> if x = 2 then remove b 1) = [0; 1; 2; 3; 4]
  TEST = to_list (fun b x -> if x = 2 then begin remove b 4; remove b 3; end) = [0; 1; 2]
  TEST = to_list (fun b x -> if x = 2 then insert_after b 1 5) = [0; 1; 2; 3; 4]
  TEST = to_list (fun b x -> if x = 2 then insert_after b 2 5) = [0; 1; 2; 5; 3; 4]
  TEST = to_list (fun b x -> if x = 2 then insert_after b 3 5) = [0; 1; 2; 3; 5; 4]
end
