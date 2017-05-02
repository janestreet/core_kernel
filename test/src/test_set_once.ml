open! Core_kernel
open! Import
open! Set_once

module T = struct
  type t = int Set_once.t [@@deriving bin_io, sexp]

  let compare t1 t2 = Option.compare [%compare: int] (get t1) (get t2)
end

let%expect_test "serialization" =
  let t1 = create () in
  let t2 = create () in
  set_exn t2 13;
  print_and_check_stable_type [%here] (module T) [ t1; t2 ];
  [%expect {|
    (bin_shape_digest 8b7c356301db5206ab98e334f4886c11)
    ((sexp ()) (bin_io "\000"))
    ((sexp (13)) (bin_io "\001\r")) |}];
;;

open T

let show t =
  print_s [%message "" ~_:(t : t)];
  invariant ignore t;
;;

let%expect_test "[sexp_of_t]" =
  let t = create () in
  show t;
  [%expect {|
    () |}];
  set_exn t 13;
  show t;
  [%expect {|
    (13) |}]
;;

let%expect_test "[get]" =
  let t = create () in
  let show_get () = print_s [%message "" ~_:(get t : int option)] in
  show_get ();
  [%expect {|
    () |}];
  set_exn t 13;
  show_get ();
  [%expect {|
    (13) |}];
;;

let%expect_test "[get] doesn't allocate" =
  let t = create () in
  let check_get here =
    ignore (require_no_allocation here (fun () -> get t) : int option) in
  check_get [%here];
  [%expect {|
    |}];
  set_exn t 13;
  check_get [%here];
  [%expect {|
    |}];
;;

let%expect_test "[get_exn]" =
  let t = create () in
  show_raise (fun () -> get_exn t);
  [%expect {|
    (raised "Option.value_exn None") |}];
  set_exn t 13;
  print_s [%message "" ~_:(get_exn t : int)];
  [%expect {|
    13 |}];
;;

let%expect_test "[set]" =
  let t = create () in
  print_s [%message "" ~_:(set t 13 : (unit, string) Result.t)];
  [%expect {|
    (Ok ()) |}];
;;

let%expect_test "[set] error" =
  let t = create () in
  set_exn t 13;
  print_s [%message "" ~_:(set t 14 : (unit, string) Result.t)];
  [%expect {|
    (Error "already set") |}];
;;

let%expect_test "[set_exn] error" =
  let t = create () in
  set_exn t 13;
  show_raise (fun () -> set_exn t 14);
  [%expect {|
    (raised set_once.ml.Already_set) |}];
;;
