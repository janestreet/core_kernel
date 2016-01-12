open Int_replace_polymorphic_compare

module Sexp = Sexplib.Sexp
module String = Caml.StringLabels
module Array = Core_array

let phys_equal = Caml.(==)

let does_raise = Exn.does_raise

(* We maintain the property that all values of type [t] do not have the tag
   [double_array_tag].  Some functions below assume this in order to avoid testing the
   tag, and will segfault if this property doesn't hold. *)
type t = Obj.t array

let invariant t =
  assert (Obj.tag (Obj.repr t) <> Obj.double_array_tag);
;;

let length = Array.length

let sexp_of_t t =
  Sexp.Atom (String.concat ~sep:""
               [ "<Obj_array.t of length ";
                 string_of_int (length t);
                 ">"
               ])
;;

let zero_obj = Obj.repr (0 : int)

let create ~len = Array.create ~len zero_obj

let empty = [||]

let get t i =
  (* Make the compiler believe [a] is an integer array so it does not check if [a] is
     tagged with [Double_array_tag]. *)
  Obj.repr (Array.get (Obj.magic (t : t) : int array) i : int)
;;

let unsafe_get t i =
  (* Make the compiler believe [a] is an integer array so it does not check if [a] is
     tagged with [Double_array_tag]. *)
  Obj.repr (Array.unsafe_get (Obj.magic (t : t) : int array) i : int)
;;

(* For [set] and [unsafe_set], if a pointer is involved, we first do a physical-equality
   test to see if the pointer is changing.  If not, we don't need to do the [set], which
   saves a call to [caml_modify].  We think this physical-equality test is worth it
   because it is very cheap (both values are already available from the [is_int] test)
   and because [caml_modify] is expensive. *)

let set t i obj =
  (* We use [get] first but then we use [Array.unsafe_set] since we know that [i] is
     valid. *)
  let old_obj = get t i in
  if Obj.is_int old_obj && Obj.is_int obj
  then
    (* It is OK to skip [caml_modify] if both the old and new values are integers. *)
    Array.unsafe_set (Obj.magic (t : t) : int array) i (Obj.obj obj : int)
  else if not (phys_equal old_obj obj)
  then Array.unsafe_set t i obj
;;

let unsafe_set t i obj =
  let old_obj = unsafe_get t i in
  if Obj.is_int old_obj && Obj.is_int obj
  then
    (* It is OK to skip [caml_modify] if both the old and new values are integers. *)
    Array.unsafe_set (Obj.magic (t : t) : int array) i (Obj.obj obj : int)
  else if not (phys_equal old_obj obj)
  then Array.unsafe_set t i obj
;;

let singleton obj =
  let t = create ~len:1 in
  unsafe_set t 0 obj;
  t;
;;

let unsafe_set_int_assuming_currently_int t i int =
  Array.unsafe_set (Obj.magic (t : t) : int array) i int
;;

(* Pre-condition: t.(i) is an integer. *)
let unsafe_set_assuming_currently_int t i obj =
  if Obj.is_int obj
  then unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else
    (* [t.(i)] is an integer and [obj] is not, so we do not need to check if they are
       equal. *)
    Array.unsafe_set t i obj
;;

let unsafe_clear_if_pointer t i =
  let old_obj = unsafe_get t i in
  if not (Obj.is_int old_obj) then Array.unsafe_set t i (Obj.repr 0);
;;

(** [unsafe_blit] is like [Array.blit], except it uses our own for-loop to avoid
    caml_modify when possible.  Its performance is still not comparable to a memcpy. *)
let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  (* When [phys_equal src dst], we need to check whether [dst_pos < src_pos] and have the
     for loop go in the right direction so that we don't overwrite data that we still need
     to read.  When [not (phys_equal src dst)], doing this is harmless.  From a
     memory-performance perspective, it doesn't matter whether one loops up or down.
     Constant-stride access, forward or backward, should be indistinguishable (at least on
     an intel i7).  So, we don't do a check for [phys_equal src dst] and always loop up in
     that case. *)
  if dst_pos < src_pos
  then
    for i = 0 to len - 1 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done
  else
    for i = len - 1 downto 0 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done;
;;

include
  Blit.Make
    (struct
      type t = Obj.t
      let equal = phys_equal
      let of_bool b = Obj.repr (if b then 1 else 2 : int)
    end)
    (struct
      type nonrec t = t [@@deriving sexp_of]
      let create = create
      let get = get
      let set = set
      let length = length
      let unsafe_blit = unsafe_blit
    end)
;;

let copy src =
  let dst = create ~len:(length src) in
  blito ~src ~dst ();
  dst
;;

let truncate t ~len = Obj.truncate (Obj.repr (t : t)) len

(* [create] *)
let%test_unit _ =
  let t = create ~len:0 in
  assert (length t = 0)
;;

(* [empty] *)
let%test _ = length empty = 0

let%test _ = does_raise (fun () -> get empty 0)

(* [singleton] *)
let%test _ = length (singleton zero_obj) = 1

let%test _ = phys_equal (get (singleton zero_obj) 0) zero_obj

let%test _ = does_raise (fun () -> get (singleton zero_obj) 1)

let%test_unit _ =
  let f = 13. in
  let t = singleton (Obj.repr f) in
  invariant t;
  assert (Polymorphic_compare.equal (Obj.repr f) (get t 0))
;;

(* [get], [unsafe_get], [set], [unsafe_set], [unsafe_set_assuming_currently_int] *)
let%test_unit _ =
  let t = create ~len:1 in
  assert (length t = 1);
  assert (phys_equal (get t 0) zero_obj);
  assert (phys_equal (unsafe_get t 0) zero_obj);
  let one_obj = Obj.repr (1 : int) in
  let check_get expect =
    assert (phys_equal (get t 0) expect);
    assert (phys_equal (unsafe_get t 0) expect);
  in
  set t 0 one_obj;
  check_get one_obj;
  unsafe_set t 0 zero_obj;
  check_get zero_obj;
  unsafe_set_assuming_currently_int t 0 one_obj;
  check_get one_obj
;;

(* [truncate] *)
let%test _ = does_raise (fun () -> truncate empty ~len:0)
let%test _ = does_raise (fun () -> truncate empty ~len:1)
let%test _ = does_raise (fun () -> truncate empty ~len:(-1))
let%test _ = does_raise (fun () -> truncate (create ~len:1) ~len:0)
let%test _ = does_raise (fun () -> truncate (create ~len:1) ~len:2)

let%test_unit _ =
  let t = create ~len:1 in
  truncate t ~len:1;
  assert (length t = 1)
;;

let%test_unit _ =
  let t = create ~len:3 in
  truncate t ~len:2;
  assert (length t = 2);
  truncate t ~len:1;
  assert (length t = 1)
;;
