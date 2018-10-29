open! Import

open! Polymorphic_compare

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
  (* Unfortunate, but by specializing [with_iteration] for different arities, a large
     amount of allocation during folds and iterations is avoided.

     The original type of [with_iteration] was
     [val with_iteration : t -> (unit -> 'a) -> 'a]

     The difference between
     {[
       let x = e in
       let f () = g x in
       f ()
     ]}
     and
     {[
       let x = e in
       let f x = g x in
       f x
     ]}
     is that in the first case the closure for [f] contains a pointer to [x],
     and in the second case it doesn't. A closure without pointers to enclosing
     environment is implemented as a naked function pointer, so we don't
     allocate at all.

     For the same reason we make sure not to call [Result.try_with (fun () -> ...)]
     inside [with_iteration] and do an explicit match statement instead.  *)
  val with_iteration_2 : t -> 'a -> 'b -> ('a -> 'b -> 'c) -> 'c
  val with_iteration_3 : t -> 'a -> 'b -> 'c -> ('a -> 'b -> 'c -> 'd) -> 'd
  val with_iteration_4 : t -> 'a -> 'b -> 'c -> 'd -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e
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

  (* Care is taken not to allocate in [with_iteration_*], since it is called every second
     by [every_second] in [writer0.ml] *)

  let incr_pending_iters s = s.pending_iterations <- s.pending_iterations + 1
  let decr_pending_iters s = s.pending_iterations <- s.pending_iterations - 1

  let with_iteration_2 t a b f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let with_iteration_3 t a b c f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b c with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let with_iteration_4 t a b c d f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b c d with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let merge (t1 : t) t2 =
    if Union_find.same_class t1 t2 then `Same_already else begin
      let n1 = (union_find_get__check_no_pending_iterations t1).length in
      let n2 = (union_find_get__check_no_pending_iterations t2).length in
      with_iteration_4 t1 t1 t2 n1 n2 (fun t1 t2 n1 n2 ->
        with_iteration_4 t2 t1 t2 n1 n2 (fun t1 t2 n1 n2 ->
          Union_find.union t1 t2;
          Union_find.set t1 {
            length = n1 + n2;
            pending_iterations = 0;
          }));
      `Merged
    end

end

type ('a, _) mk =
  | Empty : ('a, [ `empty ]) mk
  | Elt :
      {
        value : 'a;
        mutable prev : 'a elt;
        mutable next : 'a elt;
        mutable header : Header.t;
      } -> ('a, [ `elt ]) mk
and 'a elt = ('a, [ `elt ]) mk

module Elt : sig
  type 'a t = 'a elt [@@deriving sexp_of]
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

  type 'a t = 'a elt

  let equal = phys_equal

  let next (Elt t) = t.next
  let prev (Elt t) = t.prev
  let header (Elt t) = t.header

  let create_aux v header =
    let rec t = Elt { value = v; prev = t; next = t; header = header; } in
    t

  let is_singleton (Elt { prev } as t) = equal t prev

  let sexp_of_t sexp_of_a (Elt t) = sexp_of_a t.value

  let create v = create_aux v (Header.create ())

  let value (Elt t) = t.value

  (*
     [split_or_splice] is sufficient as the lone primitive for
     accomplishing all pointer updates on cyclic loops of list nodes.
     It takes two "gaps" between adjacent linked list nodes.  If the gaps
     point into the same list, the result is that it will be split into
     two lists afterwards.  If the gaps point into different lists, the
     result is that they will be spliced together into one list afterwards.

     {v
       Before                      After
           -----+        +-----         -----+               +-----
              A |  <-->  | B               A |  <---   --->  | B
           -----+        +-----         -----+      \ /      +-----
                                                     X
           -----+        +-----         -----+      / \      +-----
              C |  <-->  | D               C |  <---   --->  | D
           -----+        +-----         -----+               +-----
     v} *)

  let unsafe_split_or_splice
        ~prev1:(Elt a as elt_a)
        ~next1:(Elt b as elt_b)
        ~prev2:(Elt c as elt_c)
        ~next2:(Elt d as elt_d) =
    a.next <- elt_d; d.prev <- elt_a;
    c.next <- elt_b; b.prev <- elt_c

  let unsafe_split_or_splice_after (Elt t1) (Elt t2) =
    let Elt { prev = prev1 } as next1 = t1.next in
    let Elt { prev = prev2 } as next2 = t2.next in
    unsafe_split_or_splice ~next1 ~prev1 ~next2 ~prev2

  let unsafe_split_or_splice_before (Elt t1) (Elt t2) =
    let Elt { next = next1 } as prev1 = t1.prev in
    let Elt { next = next2 } as prev2 = t2.prev in
    unsafe_split_or_splice ~next1 ~prev1 ~next2 ~prev2

  let check_two_nodes_no_pending_iterations (Elt t1) (Elt t2) =
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

  let insert_before (Elt t as elt) v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_before elt node;
    node

  let insert_after (Elt t as elt) v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_after elt node;
    node

  let dummy_header = Header.create ()

  let unlink_before (Elt t as elt) =
    let Elt node as elt_node = t.prev in
    if is_singleton elt_node then elt_node else begin
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_before elt elt_node;
      node.header <- dummy_header;
      elt_node
    end

  let unlink_after (Elt t as elt) =
    let Elt node as elt_node = t.next in
    if is_singleton elt_node then elt_node else begin
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_after elt elt_node;
      node.header <- dummy_header;
      elt_node
    end

  let unlink (Elt t) = ignore (unlink_after t.prev)

end

type 'a dl = DL : ('a, _) mk -> 'a dl  [@@unboxed]
type 'a t = 'a dl ref

let invariant invariant_a t =
  match !t with
  | DL Empty -> ()
  | DL (Elt _ as head) ->
    let header = Elt.header head in
    let rec loop n elt =
      let next_elt = Elt.next elt in
      let prev_elt = Elt.prev elt in
      assert (Elt.equal elt (Elt.prev next_elt));
      assert (Elt.equal elt (Elt.next prev_elt));
      assert (Header.equal (Elt.header elt) header);
      invariant_a (Elt.value elt);
      if Elt.equal next_elt head then n else loop (n + 1) next_elt
    in
    let len = loop 1 head in
    assert (len = Header.length header)

let create (type a) () : a t = ref (DL Empty)

let equal (t : _ t) t' = phys_equal t t'

let of_list = function
  | [] -> create ()
  | x :: xs ->
    let first = Elt.create x in
    let _last = List.fold xs ~init:first ~f:Elt.insert_after in
    ref (DL first)

let of_array = function
  | [||] -> create ()
  | arr ->
    let first = Elt.create arr.(0) in
    let rec loop arr elt i =
      if i < Array.length arr then
        loop arr (Elt.insert_after elt arr.(i)) (i + 1)
    in
    loop arr first 1;
    ref (Some first)

let fold_elt t ~init ~f =
  match !t with
  | DL Empty -> init
  | DL (Elt _ as first) ->
    Header.with_iteration_3
      (Elt.header first)
      f init first
      (fun f init first ->
         let rec loop f acc first elt =
           let acc = f acc elt in
           let next = Elt.next elt in
           if phys_equal next first then acc else loop f acc first next
         in
         loop f init first first)
;;

let fold_elt_1 t ~init ~f a =
  match !t with
  | DL Empty -> init
  | DL (Elt _ as first) ->
    Header.with_iteration_4
      (Elt.header first)
      f a init first
      (fun f a init first ->
         let rec loop f a acc first elt =
           let acc = f a acc elt in
           let next = Elt.next elt in
           if phys_equal next first then acc else loop f a acc first next
         in
         loop f a init first first)
;;

let iter_elt t ~f = fold_elt_1 t ~init:() ~f:(fun f () elt -> f elt) f

open With_return

let find_elt t ~f =
  with_return (fun r ->
    fold_elt_1 t f ~init:() ~f:(fun f () elt ->
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
  | DL Empty -> ()
  | DL (Elt _ as first) ->
    Header.with_iteration_2
      (Elt.header first)
      first f
      (fun first f -> iter_loop first f first)

module C = Container.Make (struct
    type 'a t_ = 'a t
    type 'a t = 'a t_
    let fold t ~init ~f = fold_elt_1 t ~init f ~f:(fun f acc elt -> f acc (Elt.value elt))
    let iter = `Custom iter
  end)

let count       = C.count
let sum         = C.sum
let exists      = C.exists
let find        = C.find
let find_map    = C.find_map
let fold        = C.fold
let for_all     = C.for_all
let mem         = C.mem
let to_array    = C.to_array
let min_elt     = C.min_elt
let max_elt     = C.max_elt
let fold_result = C.fold_result
let fold_until  = C.fold_until

let unchecked_iter t ~f =
  match !t with
  | DL Empty -> ()
  | DL (Elt _ as first) ->
    let rec loop t f elt =
      f (Elt.value elt);
      let next = Elt.next elt in
      match !t with (* the first element of the bag may have been changed by [f] *)
      | DL Empty -> ()
      | DL (Elt _ as first) -> if not (phys_equal first next) then loop t f next
    in
    loop t f first

let is_empty t = !t = DL Empty (* more efficient than what Container.Make returns *)

let fold_right t ~init ~f =
  match !t with
  | DL Empty -> init
  | DL (Elt _ as first) ->
    Header.with_iteration_3
      (Elt.header first)
      f init first
      (fun f init first ->
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
  | DL Empty -> 0
  | DL (Elt _ as first) -> Header.length (Elt.header first)

let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
let t_of_sexp a_of_sexp s = of_list (List.t_of_sexp a_of_sexp s)

let copy t = of_list (to_list t)

let clear t = (t := DL Empty)

exception Transfer_src_and_dst_are_same_list

let transfer ~src ~dst =
  if phys_equal src dst then raise Transfer_src_and_dst_are_same_list;
  match !src with
  | DL Empty -> ()
  | DL (Elt _ as src_head) ->
    match !dst with
    | DL Empty ->
      dst := DL src_head;
      clear src
    | DL (Elt _ as dst_head) ->
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
      | DL Empty -> ()
      | DL (Elt _ as head) ->
        if Elt.equal head elt then begin
          let next_elt = Elt.next elt in
          t := if Elt.equal head next_elt then DL Empty else DL next_elt
        end
    end;
    Elt.unlink elt)

exception Elt_does_not_belong_to_list

let first_elt t = match !t with
  | DL Empty -> None
  | DL (Elt _ as first) -> Some first

let last_elt t = match !t with
  | DL Empty -> None
  | DL (Elt first_elt) -> Some first_elt.prev

let first t = match !t with
  | DL Empty -> None
  | DL (Elt first_elt) -> Some first_elt.value

let last t = match !t with
  | DL Empty -> None
  | DL (Elt { prev = Elt last_elt }) -> Some last_elt.value

let is_first t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.equal elt first
    else
      raise Elt_does_not_belong_to_list

let is_last t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    if Header.equal (Elt.header first) (Elt.header elt) then begin
      let last = Elt.prev first in
      Elt.equal elt last
    end else
      raise Elt_does_not_belong_to_list

let mem_elt t elt =
  match !t with
  | DL Empty -> false
  | DL (Elt _ as first) -> Header.equal (Elt.header first) (Elt.header elt)

let prev t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    if Elt.equal elt first then
      None
    else if Header.equal (Elt.header first) (Elt.header elt) then
      Some (Elt.prev elt)
    else
      raise Elt_does_not_belong_to_list

let next t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    let last = Elt.prev first in
    if Elt.equal elt last then
      None
    else if Header.equal (Elt.header first) (Elt.header elt) then
      Some (Elt.next elt)
    else
      raise Elt_does_not_belong_to_list

let insert_after t elt v =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.insert_after elt v
    else
      raise Elt_does_not_belong_to_list

let insert_before t elt v =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    if Elt.equal elt first then begin
      let new_elt = Elt.insert_before first v in
      t := DL new_elt;
      new_elt
    end else if Header.equal (Elt.header first) (Elt.header elt) then
      Elt.insert_before elt v
    else
      raise Elt_does_not_belong_to_list

let insert_empty t v =
  let new_elt = Elt.create v in
  t := DL new_elt;
  new_elt

let insert_last t v =
  match !t with
  | DL Empty -> insert_empty t v
  | DL (Elt _ as first) -> Elt.insert_before first v

let insert_first t v =
  match !t with
  | DL Empty -> insert_empty t v
  | DL (Elt _ as first) ->
    let new_elt = Elt.insert_before first v in
    t := DL new_elt;
    new_elt

let remove_last t =
  match !t with
  | DL Empty -> None
  | DL (Elt _ as first) ->
    let last = Elt.unlink_before first in
    if Elt.equal first last then t := DL Empty;
    Some (Elt.value last)

let remove_first t =
  match !t with
  | DL Empty -> None
  | DL (Elt _ as first) ->
    let second = Elt.next first in
    ignore (Elt.unlink first);
    t := if Elt.equal first second then DL Empty else DL second;
    Some (Elt.value first)

let remove t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
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
    | DL Empty -> raise Elt_does_not_belong_to_list
    | DL (Elt _ as first) ->
      if Header.equal (Elt.header first) (Elt.header elt) then begin
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        let first =
          if Elt.equal first elt then begin
            t := DL after_elt;
            after_elt
          end else
            first
        in
        (* splice [elt] in before [anchor] *)
        Elt.split_or_splice_before anchor elt;
        if Elt.equal first anchor then t := DL elt;
      end else
        raise Elt_does_not_belong_to_list
  else
    raise Elt_does_not_belong_to_list

let move_to_front t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    if not (Elt.equal elt first) then move_before t elt ~anchor:first

let move_after t elt ~anchor =
  if Elt.equal anchor elt then
    raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt) then
    match !t with
    | DL Empty -> raise Elt_does_not_belong_to_list
    | DL (Elt _ as first) ->
      if Header.equal (Elt.header first) (Elt.header elt) then begin
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        if Elt.equal first elt then t := DL after_elt;
        (* splice [elt] in after [anchor] *)
        Elt.split_or_splice_after anchor elt
      end else
        raise Elt_does_not_belong_to_list
  else
    raise Elt_does_not_belong_to_list

let move_to_back t elt =
  match !t with
  | DL Empty -> raise Elt_does_not_belong_to_list
  | DL (Elt _ as first) ->
    let last = Elt.prev first in
    if not (Elt.equal elt last) then move_after t elt ~anchor:last

let to_sequence t = to_list t |> Sequence.of_list
