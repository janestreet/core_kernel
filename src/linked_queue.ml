open! Import

module Array = Core_array
module Binable = Binable0
module List = Core_list
module Queue = Caml.Queue

type 'a t = 'a Queue.t

let create = Queue.create

let enqueue t x = Queue.push x t

let is_empty = Queue.is_empty

let dequeue t = if is_empty t then None else Some (Queue.pop t)

let dequeue_exn = Queue.pop

let peek t = if is_empty t then None else Some (Queue.peek t)

let peek_exn = Queue.peek

let clear = Queue.clear

let copy = Queue.copy

let length = Queue.length

let iter t ~f = Queue.iter f t

let fold t ~init ~f = Queue.fold f init t

module C =
  Indexed_container.Make (struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
    let foldi = `Define_using_fold
    let iteri = `Define_using_fold
  end)

let count       = C.count
let exists      = C.exists
let find        = C.find
let find_map    = C.find_map
let fold_result = C.fold_result
let fold_until  = C.fold_until
let for_all     = C.for_all
let max_elt     = C.max_elt
let mem         = C.mem
let min_elt     = C.min_elt
let sum         = C.sum
let to_list     = C.to_list

let counti    = C.counti
let existsi   = C.existsi
let find_mapi = C.find_mapi
let findi     = C.findi
let foldi     = C.foldi
let for_alli  = C.for_alli
let iteri     = C.iteri

let transfer ~src ~dst = Queue.transfer src dst

let concat_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    List.iter (f a) ~f:(fun b -> enqueue res b));
  res
;;

let concat_mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a ->
    List.iter (f i a) ~f:(fun b -> enqueue res b));
  res
;;

let filter_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    match f a with
    | None -> ()
    | Some b -> enqueue res b);
  res;
;;

let filter_mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a ->
    match f i a with
    | None -> ()
    | Some b -> enqueue res b);
  res;
;;

let filter t ~f =
  let res = create () in
  iter t ~f:(fun a -> if f a then enqueue res a);
  res;
;;

let filteri t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> if f i a then enqueue res a);
  res;
;;

let map t ~f =
  let res = create () in
  iter t ~f:(fun a -> enqueue res (f a));
  res;
;;

let mapi t ~f =
  let res = create () in
  iteri t ~f:(fun i a -> enqueue res (f i a));
  res;
;;

let filter_inplace q ~f =
  let q' = filter q ~f in
  clear q;
  transfer ~src:q' ~dst:q;
;;

let filteri_inplace q ~f =
  let q' = filteri q ~f in
  clear q;
  transfer ~src:q' ~dst:q;
;;

let enqueue_all t list =
  List.iter list ~f:(fun x -> enqueue t x)
;;

let of_list list =
  let t = create () in
  List.iter list ~f:(fun x -> enqueue t x);
  t
;;

let of_array array =
  let t = create () in
  Array.iter array ~f:(fun x -> enqueue t x);
  t
;;

let init len ~f =
  let t = create () in
  for i = 0 to len - 1 do
    enqueue t (f i)
  done;
  t
;;

let to_array t =
  match length t with
  | 0 -> [||]
  | len ->
    let arr = Array.create ~len (peek_exn t) in
    let i = ref 0 in
    iter t ~f:(fun v ->
      arr.(!i) <- v;
      incr i);
    arr
;;

let partial_iter t ~f =
  with_return (fun r ->
    iter t ~f:(fun x ->
      match f x with
      | `Continue -> ()
      | `Stop -> r.return ()))
;;

let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)
let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

let singleton a =
  let t = create () in
  enqueue t a;
  t
;;

include
  Bin_prot.Utils.Make_iterable_binable1 (struct

    type 'a t = 'a Queue.t
    type 'a el = 'a [@@deriving bin_io]

    let caller_identity = Bin_prot.Shape.Uuid.of_string "800df9a0-4992-11e6-881d-ffe1a5c8aced"

    let module_name = Some "Core_kernel.Std.Linked_queue"

    let length = length

    let iter = iter

    (* Bin_prot reads the elements in the same order they were written out, as determined
       by [iter].  So, we can ignore the index and just enqueue each element as it is read
       in. *)
    let init ~len ~next =
      let t = create () in
      for _ = 1 to len do
        enqueue t (next ())
      done;
      t
  end)

let%test_module _ = (module struct
                      let m =
                        let module M  = struct
                          type 'a u = 'a t [@@deriving bin_io]
                          type t = int u [@@deriving bin_io]
                        end
                        in
                        (module M : Binable.S with type t = M.t)
                      ;;

                      let test list =
                        let t = of_list list in
                        let bigstring = Binable.to_bigstring m t in
                        let list' = to_list (Binable.of_bigstring m bigstring) in
                        [%compare.equal: int list] list list'
                      ;;

                      let%test _ = test []
                      let%test _ = test [ 1 ]
                      let%test _ = test [ 1; 2; 3 ]
                      let%test _ = test (List.init 10_000 ~f:Fn.id)

                    end)
