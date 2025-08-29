open! Core
open! Expect_test_helpers_core

let%expect_test ("[to_list] does not allocate more than one new constructor"
  [@tags "no-js"])
  =
  (* [to_list] should at most allocate a new list node, which takes 3 minor words *)
  let list : int list = Sys.opaque_identity [ 2; 3 ] in
  let _ : int list =
    require_allocation_does_not_exceed (Minor_words 3) (fun () -> 1 :: list)
  in
  [%expect {| |}];
  let list = Sys.opaque_identity Nonempty_list.[ 1; 2; 3 ] in
  let _ : int list =
    require_allocation_does_not_exceed ~cr:CR_soon (Minor_words 3) (fun () ->
      Nonempty_list.to_list list)
  in
  [%expect
    {|
    ("allocation exceeded limit" (allocation_limit (Minor_words 3)))
    |}]
;;
