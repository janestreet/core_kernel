open! Import
open Std_internal

include Fdeque

let enqueue     = enqueue_back
let enqueue_top = enqueue_front
let bot_exn     = peek_back_exn
let bot         = peek_back
let top_exn     = peek_front_exn
let top         = peek_front
let dequeue_exn = dequeue_front_exn
let dequeue     = dequeue_front
let discard_exn = drop_front_exn

let%test_unit "Fqueue round trip via list" =
  Quickcheck.test (List.gen Int.gen)
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun a ->
      let b = of_list a in
      let c = to_list b in
      let d = of_list c in
      [%test_result: int list] ~expect:a c;
      [%test_result: int t] ~expect:b d)
    ~examples:
      [ []
      ; [1]
      ; [1; 2]
      ; [1; 2; 3]
      ]
;;
