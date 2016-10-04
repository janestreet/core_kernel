(* This code is based on the MLton library set/disjoint.fun, which has the
   following copyright notice.
*)
(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

open! Import

include struct
  module List = Core_list
  let phys_equal = (==)
end

(*
   {{
          Root
            |
          Inner
       / .. | .. \
    Inner Inner Inner
     /|\   /|\   /|\
     ...   ...   ...
   |}

  We construct the `inverted' tree in the ML representation.
  The direction of the edges is UPWARDS.
  Starting with any ['a t] we can step directly to its parent.
  But we can't (and don't need to) start from the root and step to its children.
*)

(*
  [rank] is an upper bound on the depth of any node in the up-tree.

  Imagine an unlucky sequence of operations in which you create N
  individual [t]-values and then union them together in such a way
  that you always pick the root of each tree to union together, so that
  no path compression takes place.  If you don't take care to somehow
  balance the resulting up-tree, it is possible that you end up with one
  big long chain of N links, and then calling [representative] on the
  deepest node takes Theta(N) time.  With the balancing scheme of never
  increasing the rank of a node unnecessarily, it would take O(log N).
*)

(* NOTE: This does not work yet due to an OCaml 4.04 beta bug.  Should be
   fixed before the release of OCaml 4.04. *)
(* type ('a, 'kind) tree = *)
(*   | Root : { mutable value : 'a; mutable rank : int } -> ('a, [ `root ]) tree *)
(*   | Inner : { mutable parent : 'a node } -> ('a, [ `inner ]) tree *)
(*  *)
(* and 'a node = Node : ('a, _) tree -> 'a node  [@@ocaml.unboxed] *)
(*  *)
(* type 'a t = ('a, [ `inner ]) tree *)

type ('a, 'kind, 'parent) tree =
  | Root : { mutable value : 'a; mutable rank : int } ->
    ('a, [ `root ], 'parent) tree
  | Inner : { mutable parent : 'parent } -> ('a, [ `inner ], 'parent) tree

type 'a node = Node : ('a, _, 'a node) tree -> 'a node  [@@ocaml.unboxed]

type 'a t = ('a, [ `inner ], 'a node) tree

let invariant t =
  let rec loop (Inner inner) depth =
    match inner.parent with
    | Node (Inner _ as parent) -> loop parent (depth + 1)
    | Node (Root r) -> assert (depth <= r.rank)
  in
  loop t 0

let create v = Inner { parent = Node (Root { value = v; rank = 0 }) }

(* NOTE: does not use tail-recursion like previous implementation, because
   the depth should never exceed O(log N) anyway.  It's faster this way. *)
let rec compress ~repr:(Inner inner as repr) = function
  | Node (Root _ as root) -> repr, root
  | Node (Inner next_inner as repr) ->
      let repr, _ as res = compress ~repr next_inner.parent in
      inner.parent <- Node repr;
      res

let compress_inner (Inner inner as repr) = compress ~repr inner.parent

let get_root (Inner inner) =
  match inner.parent with
  | Node (Root _ as root) -> root  (* Avoids compression call *)
  | Node (Inner _ as repr) ->
      let repr, root = compress_inner repr in
      inner.parent <- Node repr;
      root

let get t = let Root r = get_root t in r.value

let set t x = let Root r = get_root t in r.value <- x

let same_class t1 t2 = phys_equal (get_root t1) (get_root t2)

let union t1 t2 =
  let Inner inner1 as repr1, (Root r1 as root1) = compress_inner t1 in
  let Inner inner2 as repr2, (Root r2 as root2) = compress_inner t2 in
  if phys_equal root1 root2 then
    ()
  else
    let n1 = r1.rank in
    let n2 = r2.rank in
    if n1 < n2 then
      inner1.parent <- Node repr2
    else begin
      inner2.parent <- Node repr1;
      if n1 = n2 then r1.rank <- r1.rank + 1;
    end

let%test_module _ = (module struct

  let is_compressed (Inner t) =
    invariant t;
    match t.parent with
    | Root _ -> true
    | Inner t ->
      match t.parent with
      | Root _ -> true
      | Inner _ -> false
  ;;

  (* invariant checking wrapper functions *)

  let create x =
    let t = create x in
    assert (is_compressed t);
    t

  let union t1 t2 =
    union t1 t2;
    invariant t1;
    invariant t2;
    assert (is_compressed t1 || is_compressed t2);
  ;;

  let get t =
    let x = get t in
    assert (is_compressed t);
    x
  ;;

  let set t x =
    set t x;
    assert (is_compressed t);
  ;;

  let same_class t1 t2 =
    let b = same_class t1 t2 in
    assert (is_compressed t1);
    assert (is_compressed t2);
    b
  ;;

  let%test_unit "union" =
    let a = create 1 in
    let b = create 2 in
    assert (not (same_class a b));
    union a b;
    assert (same_class a b);
    let c = create 3 in
    assert (not (same_class a c));
    assert (not (same_class b c));
    union b c;
    assert (same_class a c);
    assert (same_class b c);
    let d = create 1 in
    let e = create 2 in
    let f = create 3 in
    union d e;
    union d f;
    assert (same_class d e);
    assert (same_class d f);
    assert (same_class e f)
  ;;

  let%test_unit "union" =
    let a = create 1 in
    let b = create 2 in
    union a b;
    let c = create 1 in
    let d = create 2 in
    union c d;
    union b d;
    assert (same_class a c)
  ;;

  let%test_unit "set/get" =
    let a = create 1 in
    let b = create 2 in
    assert (get a = 1);
    assert (get b = 2);
    union a b;
    set a 3;
    assert (get a = 3);
    assert (get b = 3)
  ;;

  let%test_unit "compressed" =
    let n = 1000 in
    let ts = List.init n ~f:create in
    let t = List.reduce_exn ts ~f:(fun a b -> union a b; b) in
    let max_rank = List.fold ts ~init:0 ~f:(fun acc t -> max acc (root t).rank) in
    assert (max_rank = 1);
    set t 42;
    assert (List.for_all ts ~f:(fun t' -> same_class t t' && get t' = 42))
  ;;

  let%test_unit "balanced" =
    let module Array = Core_array in
    let log2 n = int_of_float (ceil (log (float_of_int n) /. log 2.)) in
    let n = 1000 in
    let ts = Array.init n ~f:create in
    let rec sub i j =
      if i = j then ts.(i) else begin
        let k = (i + j) / 2 in
        let a = sub i k in
        if k+1 > j then a else begin
          let b = sub (k+1) j in
          union a b;
          a
        end
      end
    in
    let t = sub 0 (pred n) in
    Array.iter ts ~f:invariant;
    assert (Array.exists ts ~f:(fun t -> not (is_compressed t)));
    let max_rank = Array.fold ts ~init:0 ~f:(fun acc t -> max acc (root t).rank) in
    assert (max_rank <= log2 n);
    set t 42;
    assert (Array.for_all ts ~f:(fun t' -> same_class t t' && get t' = 42));
    assert (Array.for_all ts ~f:is_compressed)
  ;;
end)
