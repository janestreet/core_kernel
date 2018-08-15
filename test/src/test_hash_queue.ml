open! Core_kernel
open! Import
open! Hash_queue

module M = struct
  type t = int [@@deriving sexp, hash, compare]
end

include Make(M)

let print_queue_from_front t =
  while not (is_empty t) do
    printf "%s " (dequeue_exn t)
  done

let%expect_test "enqueue/dequeue is in the expected order" =
  let t = create () in
  enqueue_exn t 1 "one";
  enqueue_exn t 2 "two";
  enqueue_exn t 3 "three";
  enqueue_exn t 4 "four";
  print_queue_from_front t;
  [%expect {| one two three four |}]

let%expect_test "enqueue respects hash keys" =
  let t = create () in
  ignore (enqueue t 1 "a");
  ignore (enqueue t 1 "b");
  ignore (enqueue t 2 "c");
  print_queue_from_front t;
  [%expect {| a c |}]

let%expect_test "lookup_and_move_to_back" =
  let t = create () in
  enqueue_exn t 1 "one";
  enqueue_exn t 2 "two";
  enqueue_exn t 3 "three";
  ignore (lookup_and_move_to_back_exn t 1);
  print_queue_from_front t;
  [%expect {| two three one |}]


