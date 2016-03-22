open Std_internal
open Fieldslib

(* This implementation is similar to [Deque] in that it uses an array of ['a] and
   a mutable [int] to indicate what in the array is used.  We choose to implement [Stack]
   directly rather than on top of [Deque] for performance reasons.  E.g. a simple
   microbenchmark shows that push/pop is about 20% faster. *)
type 'a t =
  { (* [dummy] is a value that we create via [Obj.magic] and use for empty slots in
       [elts].  It is intended that [dummy] is never returned to user code. *)
    dummy : 'a;
    mutable length : int;
    mutable elts : 'a array;
  }
[@@deriving fields, sexp_of]

let sexp_of_t_internal = sexp_of_t
let sexp_of_t = `Rebound_later
let _ = sexp_of_t

let capacity t = Array.length t.elts

let invariant invariant_a t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~dummy:ignore
      ~length:(check (fun length ->
        assert (0 <= length && length <= Array.length t.elts)))
      ~elts:(check (fun elts ->
        for i = 0 to t.length - 1 do
          invariant_a elts.(i);
        done;
        (* We maintain the invariant that unused elements equal [t.dummy] to avoid a space
           leak. *)
        for i = t.length to Array.length elts - 1 do
          assert (phys_equal elts.(i) t.dummy)
        done;
      ));
  with exn ->
    failwiths "Stack.invariant failed" (exn, t) [%sexp_of: exn * _ t_internal]
;;

let create (type a) () : a t =
  let dummy = (Obj.magic () : a) in
  { dummy;
    length = 0;
    elts = [||];
  }
;;

let length t = t.length

let is_empty t = length t = 0

(* The order in which elements are visited has been chosen so as to be backwards
   compatible with both [Linked_stack] and [Caml.Stack] *)
let fold t ~init ~f =
  let r = ref init in
  for i = t.length - 1 downto 0 do
    r := f !r t.elts.(i)
  done;
  !r
;;

let iter t ~f =
  for i = t.length - 1 downto 0 do
    f t.elts.(i)
  done;
;;

module C =
  Container.Make (struct
    type nonrec 'a t = 'a t
    let fold = fold
    let iter = `Custom iter
  end)

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

let of_list (type a) (l : a list) =
  if List.is_empty l then
    create ()
  else begin
    let dummy = (Obj.magic () : a) in
    let length = List.length l in
    let elts = Array.create ~len:(2 * length) dummy in
    let r = ref l in
    for i = length - 1 downto 0 do
      match !r with
      | [] -> assert false
      | a :: l -> elts.(i) <- a; r := l
    done;
    { dummy; length; elts }
  end
;;

let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)

let t_of_sexp a_of_sexp sexp = of_list ([%of_sexp: a list] sexp)

include
  Bin_prot.Utils.Make_binable1 (struct
    type nonrec 'a t = 'a t
    module Binable = Core_list
    let to_binable = to_list
    let of_binable = of_list
  end)

let resize t size =
  let arr = Array.create ~len:size t.dummy in
  Array.blit ~src:t.elts ~dst:arr ~src_pos:0 ~dst_pos:0 ~len:t.length;
  t.elts <- arr
;;

let set_capacity t new_capacity =
  let new_capacity = max new_capacity (length t) in
  if new_capacity <> capacity t then resize t new_capacity;
;;

let push t a =
  if t.length = Array.length t.elts then resize t (2 * (t.length + 1));
  t.elts.(t.length) <- a;
  t.length <- t.length + 1;
;;

let pop_nonempty t =
  let i = t.length - 1 in
  let result = t.elts.(i) in
  t.elts.(i) <- t.dummy;
  t.length <- i;
  result
;;

let pop_error = Error.of_string "Stack.pop of empty stack"

let pop t =
  if is_empty t
  then None
  else Some (pop_nonempty t)
;;

let pop_exn t =
  if is_empty t
  then Error.raise pop_error
  else pop_nonempty t
;;

let top_nonempty t = t.elts.(t.length - 1)

let top_error = Error.of_string "Stack.top of empty stack"

let top t =
  if is_empty t
  then None
  else Some (top_nonempty t)
;;

let top_exn t =
  if is_empty t
  then Error.raise top_error
  else top_nonempty t;
;;

let copy { dummy; length; elts } =
  { dummy;
    length;
    elts = Array.copy elts;
  }
;;

let clear t =
  if t.length > 0 then begin
    for i = 0 to t.length - 1 do
      t.elts.(i) <- t.dummy;
    done;
    t.length <- 0;
  end;
;;

let until_empty t f =
  let rec loop () = if t.length > 0 then (f (pop_nonempty t); loop ()) in
  loop ()
;;

let singleton x =
  let t = create () in
  push t x;
  t
;;
