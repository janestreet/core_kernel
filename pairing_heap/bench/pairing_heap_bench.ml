open! Core
open Iarray.O
open! Pairing_heap

let cmp = Int.compare

(* Fixed seeds so that runs are comparable across revisions. *)
let random_keys ~seed n =
  let random = Random.State.make [| seed |] in
  Iarray.init n ~f:(fun _ -> Random.State.int random 1_000_000)
;;

let of_keys keys =
  let t = create ~cmp () in
  Iarray.iter keys ~f:(add t);
  t
;;

let drain t =
  while not (is_empty t) do
    ignore (pop_exn t : int)
  done
;;

let%bench_fun ("pop_insert_with_existing_heap"
  [@indexed initial_size = [ 1; 100; 10_000; 1_000_000 ]])
  =
  let a = Array.init initial_size ~f:(fun _ -> Random.int 100_000) in
  let h1 = of_array ~cmp:Int.compare a in
  fun () ->
    let e = pop_exn h1 in
    add h1 e
;;

(* Unlike the benchmark above, the popped element is replaced by a random one, so the
   root's children get merged rather than the same element bouncing in and out. *)
let%bench_fun ("pop_add_random" [@indexed n = [ 1_000; 100_000; 1_000_000 ]]) =
  let t = of_keys (random_keys ~seed:n n) in
  let n_keys = 1 lsl 10 in
  let new_keys = random_keys ~seed:(n + 1) n_keys in
  let i = ref 0 in
  fun () ->
    ignore (pop_exn t : int);
    add t new_keys.:(!i land (n_keys - 1));
    incr i
;;

let%bench_fun ("heap_sort" [@indexed n = [ 1_000; 100_000 ]]) =
  let keys = random_keys ~seed:n n in
  fun () ->
    let t = create ~cmp () in
    Iarray.iter keys ~f:(add t);
    drain t
;;

let%bench_fun ("add_removable_remove_half_drain" [@indexed n = [ 1_000; 100_000 ]]) =
  let keys = random_keys ~seed:n n in
  fun () ->
    let t = create ~cmp () in
    let tokens = Iarray.map keys ~f:(add_removable t) in
    Iarray.iteri tokens ~f:(fun i token -> if i land 1 = 0 then remove t token);
    drain t
;;

let%bench_fun ("update" [@indexed n = [ 1_000; 100_000; 1_000_000 ]]) =
  let t = create ~cmp () in
  let tokens =
    random_keys ~seed:n n |> Iarray.to_array |> Array.map ~f:(add_removable t)
  in
  let n_keys = 1 lsl 10 in
  let new_keys = random_keys ~seed:(n + 1) n_keys in
  let i = ref 0 in
  fun () ->
    let j = !i mod n in
    tokens.(j) <- update t tokens.(j) new_keys.:(!i land (n_keys - 1));
    incr i
;;

let%bench_fun ("copy" [@indexed n = [ 1_000; 100_000 ]]) =
  let t = of_keys (random_keys ~seed:n n) in
  fun () -> copy t
;;

(* A heap that has seen a lot of add/remove traffic, so that its nodes are laid out in the
   pool in an order unrelated to the tree; then drain it and refill it. *)
let%bench_fun ("drain_churned_and_refill" [@indexed n = [ 1_000; 100_000 ]]) =
  let random = Random.State.make [| n |] in
  let t = of_keys (Iarray.init n ~f:(fun _ -> Random.State.int random 1_000_000)) in
  for _ = 1 to 4 * n do
    ignore (pop_exn t : int);
    add t (Random.State.int random 1_000_000)
  done;
  let keys = to_array t in
  fun () ->
    drain t;
    Array.iter keys ~f:(add t)
;;
