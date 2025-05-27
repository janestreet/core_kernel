open Core
open Bus

let create_with_subscribers (type a) (arity : a Callback_arity.t) ~num_subscribers =
  let t =
    create_exn arity ~on_subscription_after_first_write:Allow ~on_callback_raise:ignore
  in
  let subscribers =
    if num_subscribers = 0
    then [||]
    else
      Array.init num_subscribers ~f:(fun _ ->
        subscribe_exn
          (read_only t)
          ~f:
            (match arity with
             | Arity1 -> fun _ -> ()
             | Arity1_local -> fun _ -> ()
             | Arity2 -> fun _ _ -> ()
             | Arity2_local -> fun _ _ -> ()
             | Arity3 -> fun _ _ _ -> ()
             | Arity3_local -> fun _ _ _ -> ()
             | Arity4 -> fun _ _ _ _ -> ()
             | Arity4_local -> fun _ _ _ _ -> ()
             | Arity5 -> fun _ _ _ _ _ -> ()
             | Arity5_local -> fun _ _ _ _ _ -> ()
             | Arity6 -> fun _ _ _ _ _ _ -> ()
             | Arity6_local -> fun _ _ _ _ _ _ -> ()))
  in
  t, subscribers
;;

let create arity ~num_subscribers = fst (create_with_subscribers arity ~num_subscribers)

let%bench_fun "Bus.create_exn (Arity 1)" =
  fun () -> ignore (create Arity1 ~num_subscribers:0 : _ Read_write.t)
;;

let%bench_fun "Bus.create_exn (Arity 4)" =
  fun () -> ignore (create Arity4 ~num_subscribers:0 : _ Read_write.t)
;;

let%bench_fun "Bus.write [int] to 0 subs" =
  let t = create Arity1 ~num_subscribers:0 in
  fun () -> write t 1
;;

let%bench_fun "Bus.write [int] to 1 sub" =
  let t = create Arity1 ~num_subscribers:1 in
  fun () -> write t 1
;;

let%bench_fun "Bus.write [int] to 300 subs" =
  let t = create Arity1 ~num_subscribers:300 in
  fun () -> write t 1
;;

let%bench_fun "Bus.write [float] to 300 subs" =
  let t = create Arity1 ~num_subscribers:300 in
  fun () -> write t 1.
;;

let%bench_fun "Bus.write [int * int] to 300 subs" =
  let t = create Arity1 ~num_subscribers:300 in
  fun () -> write t (1, 2)
;;

let%bench_fun "Bus.write4 to 300 subs" =
  let t = create Arity4 ~num_subscribers:300 in
  fun () -> write4 t 1 2 3 4
;;

let%bench_fun "Bus.write5 to 300 subs" =
  let t = create Arity5 ~num_subscribers:300 in
  fun () -> write5 t 1 2 3 4 5
;;

let%bench_fun "Bus.write_local [int * int] to 0 subs" =
  let t = create Arity1_local ~num_subscribers:0 in
  fun () -> write_local t (1, 2)
;;

let%bench_fun "Bus.write_local [int * int] to 1 sub" =
  let t = create Arity1_local ~num_subscribers:1 in
  fun () -> write_local t (1, 2)
;;

let%bench_fun "Bus.write_local [int * int] to 300 subs" =
  let t = create Arity1_local ~num_subscribers:300 in
  fun () -> write_local t (1, 2)
;;

let%bench_fun "Bus.subscribe_exn 300 times" = fun () -> create Arity1 ~num_subscribers:300

let%bench_fun "Bus.subscribe_exn + Bus.unsubscribe_exn 300 times" =
  fun () ->
  let t, subscribers = create_with_subscribers Arity1 ~num_subscribers:300 in
  Array.iter subscribers ~f:(unsubscribe (read_only t))
;;
