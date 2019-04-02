open! Import
open! Core_kernel
open! Map

let%expect_test "Symmetric_diff_element.map_data" =
  let print x = print_s [%sexp (x : (string, int) Symmetric_diff_element.t)] in
  let f x = x + 1 in
  print (Symmetric_diff_element.map_data ("foo", `Left 1) ~f);
  [%expect {| (foo (Left 2)) |}];
  print (Symmetric_diff_element.map_data ("foo", `Right 5) ~f);
  [%expect {| (foo (Right 6)) |}];
  print (Symmetric_diff_element.map_data ("foo", `Unequal (10, 12)) ~f);
  [%expect {| (foo (Unequal (11 13))) |}]
;;

let%expect_test "Symmetric_diff_element.{left,right}" =
  let values = [ "foo", `Left 1; "bar", `Right 2; "baz", `Unequal (3, 4) ] in
  let go f =
    List.iter values ~f:(fun sd_elt ->
      printf
        !"%{sexp: (string, int) Symmetric_diff_element.t} => %{sexp: int option}\n"
        sd_elt
        (f sd_elt))
  in
  go Symmetric_diff_element.left;
  [%expect
    {|
    (foo (Left 1)) => (1)
    (bar (Right 2)) => ()
    (baz (Unequal (3 4))) => (3) |}];
  go Symmetric_diff_element.right;
  [%expect
    {|
    (foo (Left 1)) => ()
    (bar (Right 2)) => (2)
    (baz (Unequal (3 4))) => (4) |}]
;;
