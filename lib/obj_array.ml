open Std_internal
open Import

let debug = false

type t = Obj.t array

let length = Array.length

let sexp_of_t t =
  Sexp.Atom (concat [ "<Obj_array.t of length ";
                      Int.to_string (length t);
                      ">"
                    ])
;;

let zero_obj = Obj.repr (0 : int)

let create ~len = Array.create ~len zero_obj

let singleton obj = Array.create ~len:1 obj

let empty = [||]

(* In the following functions [t] must not have the tag [Double_array_tag].  There is no
   other assumption about [t] needed for safety. *)

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
  if Obj.is_int old_obj && Obj.is_int obj then
    (* It is OK to skip [caml_modify] if both the old and new values are integers. *)
    Array.unsafe_set (Obj.magic (t : t) : int array) i (Obj.obj obj : int)
  else if not (phys_equal old_obj obj) then
    Array.unsafe_set t i obj
;;

let unsafe_set t i obj =
  let old_obj = unsafe_get t i in
  if Obj.is_int old_obj && Obj.is_int obj then
    (* It is OK to skip [caml_modify] if both the old and new values are integers. *)
    Array.unsafe_set (Obj.magic (t : t) : int array) i (Obj.obj obj : int)
  else if not (phys_equal old_obj obj) then
    Array.unsafe_set t i obj
;;


let unsafe_set_int_assuming_currently_int t i int =
  Array.unsafe_set (Obj.magic (t : t) : int array) i int
;;

(* Pre-condition: t.(i) is an integer. *)
let unsafe_set_assuming_currently_int t i obj =
  if Obj.is_int obj then
    unsafe_set_int_assuming_currently_int t i (Obj.obj obj : int)
  else
    (* [t.(i)] is an integer and [obj] is not, so we do not need to check if they are
       equal. *)
    Array.unsafe_set t i obj
;;

module Blit_arg = struct
  type nonrec t =
    { src : t;
      src_pos : int;
      dst : t;
      dst_pos : int;
      len : int
    }
  with sexp_of
end

let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
  if debug then
    log "unsafe_blit" { Blit_arg. src; src_pos; dst; dst_pos; len }
      (<:sexp_of< Blit_arg.t >>);
  (* When [phys_equal src dst], we need to check whether [dst_pos < src_pos] and have the
     for loop go in the right direction so that we don't overwrite data that we still need
     to read.  When [not (phys_equal src dst)], doing this is harmless.  From a
     memory-performance perspective, it doesn't matter whether one loops up or down.
     Constant-stride access, forward or backward, should be indistinguishable (at least on
     an intel i7).  So, we don't do a check for [phys_equal src dst] and always loop up in
     that case. *)
  if dst_pos < src_pos then
    for i = 0 to len - 1 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done
  else
    for i = len - 1 downto 0 do
      unsafe_set dst (dst_pos + i) (unsafe_get src (src_pos + i))
    done;
;;

let copy t = Array.copy t

let blit ~src ~src_pos ~dst ~dst_pos ~len =
  if len < 0
  || src_pos < 0
  || len > length src - src_pos
  || dst_pos < 0
  || len > length dst - dst_pos
  then
    failwiths "Obj_array.blit got invalid arguments"
      { Blit_arg. src; src_pos; dst; dst_pos; len }
      (<:sexp_of< Blit_arg.t >>);
  unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len;
;;

let sub t ~pos ~len =
  let t' = create ~len in
  blit ~src:t ~src_pos:pos ~dst:t' ~dst_pos:0 ~len;
  t'
;;

let truncate t ~len = Obj.truncate (Obj.repr (t : t)) len

(* [create] *)
TEST_UNIT =
  let t = create ~len:0 in
  assert (length t = 0);
;;

(* [empty] *)
TEST = length empty = 0

TEST = does_fail (fun () -> get empty 0)

(* [singleton] *)
TEST = length (singleton zero_obj) = 1

TEST = phys_equal (get (singleton zero_obj) 0) zero_obj

TEST = does_fail (fun () -> get (singleton zero_obj) 1)

(* [sub] *)
TEST = length (sub empty ~pos:0 ~len:0) = 0

TEST = does_fail (fun () -> sub empty ~pos:0 ~len:1)

TEST = does_fail (fun () -> sub empty ~pos:1 ~len:0)

TEST_UNIT =
  let t = sub (create ~len:4) ~pos:1 ~len:2 in
  assert (length t = 2);
  for i = 0 to 1 do
    assert (phys_equal (get t i) zero_obj);
  done;
;;

(* [get], [unsafe_get], [set], [unsafe_set], [unsafe_set_assuming_currently_int] *)
TEST_UNIT =
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
  check_get one_obj;
;;

(* [blit] *)

TEST_MODULE = struct
  let test ~src ~dst tests =
    List.iter tests
      ~f:(fun (src_pos, dst_pos, len, expect_ok) ->
        assert (
          Bool.equal
            expect_ok
            (Result.is_ok (Result.try_with (fun () ->
                             blit ~src ~src_pos ~dst ~dst_pos ~len)))))
  ;;

  TEST_UNIT =
    test ~src:empty ~dst:empty
      [ 0, 0, -1, false;
        0, 0, 0, true;
        0, 0, 1, false;
        1, 0, 0, false;
        0, 1, 0, false;
        -1, 0, 0, false;
        0, -1, 0, false;
        0, 0, Int.min_value, false;
        0, 0, Int.max_value, false;
      ]
  ;;

  TEST_UNIT =
    test ~src:(create ~len:1) ~dst:(create ~len:1)
      [ 0, 0, -1, false;
        0, 0, 0, true;
        0, 0, 1, true;
        0, 0, 2, false;
        -1, 0, 0, false;
        0, -1, 0, false;
        1, 0, -1, false;
        1, 0, 0, true;
        1, 0, 1, false;
        0, 1, -1, false;
        0, 1, 0, true;
        0, 1, 1, false;
        0, 0, Int.min_value, false;
        0, 0, Int.max_value, false;
        1, 1, Int.min_value, false;
        1, 1, Int.max_value, false;
      ]
  ;;

  TEST_UNIT =
    (* Test overlapping src/dst *)
    let of_list l =
      let arr = create ~len:(List.length l) in
      List.iteri l ~f:(fun i x -> unsafe_set_int_assuming_currently_int arr i x);
      arr
    in
    let src = of_list [0;1;2;3;4;5;6;7;8;9] in
    blit ~src ~src_pos:3 ~dst:src ~dst_pos:0 ~len:7;
    let expected = of_list [3;4;5;6;7;8;9;7;8;9] in
    assert (Array.equal src expected ~equal:phys_equal);
    let src = of_list [0;1;2;3;4;5;6;7;8;9] in
    blit ~src ~src_pos:0 ~dst:src ~dst_pos:3 ~len:7;
    let expected = of_list [0;1;2;0;1;2;3;4;5;6] in
    assert (Array.equal src expected ~equal:phys_equal);
end

(* [truncate] *)
TEST = does_fail (fun () -> truncate empty ~len:0)
TEST = does_fail (fun () -> truncate empty ~len:1)
TEST = does_fail (fun () -> truncate empty ~len:-1)
TEST = does_fail (fun () -> truncate (create ~len:1) ~len:0)
TEST = does_fail (fun () -> truncate (create ~len:1) ~len:2)

TEST_UNIT =
  let t = create ~len:1 in
  truncate t ~len:1;
  assert (length t = 1);
;;

TEST_UNIT =
  let t = create ~len:3 in
  truncate t ~len:2;
  assert (length t = 2);
  truncate t ~len:1;
  assert (length t = 1);
;;

