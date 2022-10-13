open! Base
open! Expect_test_helpers_base
open! Uopt
open! Optional_syntax

let%expect_test _ =
  print_s [%sexp (is_none none : bool)];
  [%expect {| true |}]
;;

let%expect_test _ =
  print_s [%sexp (is_some none : bool)];
  [%expect {| false |}]
;;

let%expect_test _ =
  print_s [%sexp (is_none (some 13) : bool)];
  [%expect {| false |}]
;;

let%expect_test _ =
  print_s [%sexp (is_some (some 13) : bool)];
  [%expect {| true |}]
;;

let%expect_test _ =
  require_does_raise [%here] (fun () -> value_exn none);
  [%expect {| (Failure Uopt.value_exn) |}]
;;

let%expect_test _ =
  print_s [%sexp (value_exn (some 13) : int)];
  [%expect {| 13 |}]
;;

let%expect_test _ =
  print_s [%sexp (unsafe_value (some 13) : int)];
  [%expect {| 13 |}]
;;

let%expect_test "[match%optional none]" =
  require
    [%here]
    (match%optional none with
     | None -> true
     | Some _ -> false)
;;

let%expect_test "[match%optional some]" =
  require
    [%here]
    (match%optional some 13 with
     | None -> false
     | Some x -> x = 13)
;;

let%expect_test "ensure no miscompilation due to unboxing of the float" =
  let[@inline never] f n p =
    let t = if p then Uopt.some (Float.of_int n) else Uopt.none in
    match%optional t with
    | None -> "none"
    | Some x -> Float.to_string x
  in
  print_endline (f 100 true);
  print_endline (f 100 false);
  [%expect {|
    100.
    none |}]
;;

let%expect_test "Some None" =
  require_does_raise [%here] (fun () -> Uopt.some Uopt.none);
  [%expect {|(Failure "Uopt.some Uopt.none")|}]
;;

let[@inline never] construct_some_to_inspect_assembly () =
  Uopt.some (Sys.opaque_identity 0)
;;

let construct_some i = Uopt.some (Sys.opaque_identity i)

let%bench_fun "constructing 1000 Somes" =
  Core.Gc.keep_alive construct_some_to_inspect_assembly;
  fun () ->
    for i = 1 to 1000 do
      let x = construct_some i in
      ignore (Sys.opaque_identity x : int t)
    done
;;

let[@inline never] unpack_uopt_to_inspect_assembly uopt count =
  match%optional.Uopt uopt with
  | None -> ()
  | Some y -> count := !count + y
;;

let unpack_uopt uopt count =
  match%optional.Uopt uopt with
  | None -> ()
  | Some y -> count := !count + y
;;

let%bench_fun "unpacking 1000 Somes and incrementing a counter" =
  let uopt = Uopt.some 0 in
  let count = ref 0 in
  Core.Gc.keep_alive unpack_uopt_to_inspect_assembly;
  fun () ->
    count := 0;
    for _ = 1 to 1000 do
      unpack_uopt (Sys.opaque_identity uopt) count
    done;
    ignore (Sys.opaque_identity !count : int)
;;
