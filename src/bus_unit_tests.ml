open Std_internal
module Gc = Core_gc

let does_raise = Exn.does_raise

let%test_module _ =
  (module (struct
    open Bus

    module Callback_arity = Callback_arity

    type nonrec ('a, 'b) t = ('a, 'b) t
    type ('a, 'b) bus = ('a, 'b) t


    module Read_only = struct
      type 'a t = 'a Read_only.t [@@deriving sexp_of]
      let invariant = Read_only.invariant
    end

    module Read_write = struct
      type 'a t = 'a Read_write.t [@@deriving sexp_of]
      let invariant = Read_write.invariant
    end

    module Subscriber = Subscriber

    module Fold_arity = Fold_arity

    let callback_arity  = callback_arity
    let close           = close
    let create          = create
    let fold_exn        = fold_exn
    let is_closed       = is_closed
    let iter_exn        = iter_exn
    let num_subscribers = num_subscribers
    let read_only       = read_only
    let subscribe_exn   = subscribe_exn
    let unsubscribe     = unsubscribe
    let write           = write

    let create1 created_from ~allow_subscription_after_first_write =
      create
        created_from
        Arity1
        ~allow_subscription_after_first_write
        ~on_callback_raise:Error.raise
    ;;

    let create2 created_from ~allow_subscription_after_first_write =
      create
        created_from
        Arity2
        ~allow_subscription_after_first_write
        ~on_callback_raise:Error.raise
    ;;

    let sexp_of_t = sexp_of_t

    let%test_unit "sexp_of_t" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      let bus_r = read_only bus in
      ignore (subscribe_exn bus_r [%here] ~f:ignore : _ Subscriber.t);
      ignore (bus |> [%sexp_of: (_, _) t] : Sexp.t);
    ;;

    let%test_unit "write to empty bus" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:true in
      write bus ();
    ;;

    let%test_unit "can subscribe and unsubscribe" =
      let call_count = ref 0 in
      let callback _v = incr call_count in
      let bus = create1 [%here] ~allow_subscription_after_first_write:true in
      let bus_r = read_only bus in
      [%test_result: int] (num_subscribers bus) ~expect:0;
      [%test_result: bool] (is_closed bus) ~expect:false;
      let subscription = subscribe_exn bus_r [%here] ~f:callback in
      [%test_result: int] (num_subscribers bus) ~expect:1;
      write bus ();
      unsubscribe bus_r subscription;
      [%test_result: int] (num_subscribers bus) ~expect:0;
      write bus ();
      [%test_result: int] !call_count ~expect:1;
    ;;

    let%test_unit "~allow_subscription_after_first_write:false" =
      let callback _ = () in
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      let bus_r = read_only bus in
      write bus ();
      assert (does_raise (fun () -> subscribe_exn bus_r [%here] ~f:callback));
    ;;

    let%test_unit "~allow_subscription_after_first_write:true" =
      let call_count = ref 0 in
      let callback _ = incr call_count in
      let bus = create1 [%here] ~allow_subscription_after_first_write:true in
      let bus_r = read_only bus in
      write bus ();
      ignore (subscribe_exn bus_r [%here] ~f:callback : _ Subscriber.t);
      [%test_result: int] !call_count ~expect:0;
      write bus ();
      [%test_result: int] !call_count ~expect:1;
    ;;

    let%test_unit "mid-callback subscribe_exn takes effect for the next write" =
      let bus = create2 [%here] ~allow_subscription_after_first_write:true in
      let bus_r = read_only bus in
      let call_count2 = ref 0 in
      let callback2 _ _ = incr call_count2 in
      let call_count1 = ref 0 in
      let callback1 _ _ =
        incr call_count1;
        ignore (subscribe_exn bus_r [%here] ~f:callback2)
      in
      ignore (subscribe_exn bus_r [%here] ~f:callback1);
      write bus () ();
      write bus () ();
      [%test_result: int] !call_count1 ~expect:2;
      [%test_result: int] !call_count2 ~expect:1;
    ;;

    let%test_unit "unsubscribe is idempotent" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      let bus_r = read_only bus in
      let subscriber = Bus.subscribe_exn bus_r [%here] ~f:ignore in
      unsubscribe bus_r subscriber;
      unsubscribe bus_r subscriber;
    ;;

    let%test_unit "mid-callback unsubscribe takes effect for the next write" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      let bus_r = read_only bus in
      let subscriber2 = ref None in
      let call_count1 = ref 0 in
      let callback1 _ =
        incr call_count1;
        unsubscribe bus_r (Option.value_exn !subscriber2)
      in
      let subscriber1 = subscribe_exn bus_r [%here] ~f:callback1 in
      let call_count2 = ref 0 in
      let callback2 _ = incr call_count2; unsubscribe bus_r subscriber1 in
      subscriber2 := Some (subscribe_exn bus_r [%here] ~f:callback2);
      write bus ();
      [%test_result: int] !call_count1 ~expect:1;
      [%test_result: int] !call_count2 ~expect:1;
      write bus ();
      [%test_result: int] !call_count1 ~expect:1;
      [%test_result: int] !call_count2 ~expect:1;
    ;;

    let%test_unit "on_callback_raise called for every error" =
      let call_count = ref 0 in
      let on_callback_raise _ = incr call_count in
      let bus =
        create
          [%here]
          Arity2
          ~allow_subscription_after_first_write: true
          ~on_callback_raise
      in
      let bus_r = read_only bus in
      let callback1 _ _ = failwith "callback1" in
      let callback2 _ _ = failwith "callback2" in
      ignore (subscribe_exn bus_r [%here] ~f:callback1);
      ignore (subscribe_exn bus_r [%here] ~f:callback2);
      write bus () ();
      [%test_result: int] !call_count ~expect:2;
    ;;

    let%test_unit "subscribe_exn ~on_callback_raise" =
      let r1 = ref 0 in
      let r2 = ref 0 in
      let bus =
        create [%here] Arity1 ~allow_subscription_after_first_write:true
          ~on_callback_raise:(fun _ -> incr r1)
      in
      let bus_r = read_only bus in
      ignore (subscribe_exn bus_r [%here]
                ~f:(fun () -> failwith "")
                ~on_callback_raise:(fun _ -> incr r2));
      [%test_result: int] !r1 ~expect:0;
      [%test_result: int] !r2 ~expect:0;
      write bus ();
      [%test_result: int] !r1 ~expect:0;
      [%test_result: int] !r2 ~expect:1;
    ;;

    let%test_unit "subscribe_exn ~on_callback_raise:raise" =
      let r = ref 0 in
      let bus =
        create [%here] Arity1 ~allow_subscription_after_first_write:true
          ~on_callback_raise:(fun _ -> incr r)
      in
      let bus_r = read_only bus in
      ignore (subscribe_exn bus_r [%here]
                ~f:(fun () -> failwith "")
                ~on_callback_raise:Error.raise);
      [%test_result: int] !r ~expect:0;
      write bus ();
      [%test_result: int] !r ~expect:1;
    ;;

    let%test_unit "close is idempotent" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      close bus;
      close bus;
    ;;

    let%test_unit "write raises after close" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      write bus ();
      close bus;
      [%test_result: bool] (is_closed bus) ~expect:true;
      assert (does_raise (fun () -> write bus ()))
    ;;

    let%test_unit "close takes effect after all writes" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      let bus_r = read_only bus in
      let call_count = ref 0 in
      let callback () =
        incr call_count;
        close bus;
      in
      let _ = subscribe_exn bus_r [%here] ~f:callback in
      let _ = subscribe_exn bus_r [%here] ~f:callback in
      write bus ();
      [%test_result: int] !call_count ~expect:2;
    ;;

    let assert_no_allocation bus callback write =
      let bus_r = read_only bus in
      ignore (subscribe_exn bus_r [%here] ~f:callback);
      let starting_minor_words = Gc.minor_words () in
      let starting_major_words = Gc.major_words () in
      write ();
      let ending_minor_words = Gc.minor_words () in
      let ending_major_words = Gc.major_words () in
      [%test_result: int] (ending_minor_words - starting_minor_words) ~expect:0;
      [%test_result: int] (ending_major_words - starting_major_words) ~expect:0;
    ;;

    let%test_unit "write doesn't allocate" =
      let allow_subscription_after_first_write = false in
      let create created_from arity =
        create created_from arity ~allow_subscription_after_first_write
          ~on_callback_raise:Error.raise
      in
      let bus1 = create [%here] Arity1 in
      let bus2 = create [%here] Arity2 in
      let bus3 = create [%here] Arity3 in
      let bus4 = create [%here] Arity4 in
      assert_no_allocation bus1
        (fun () -> ())
        (fun () -> write bus1 ());
      assert_no_allocation bus2
        (fun () () -> ())
        (fun () -> write bus2 () ());
      assert_no_allocation bus3
        (fun () () () -> ())
        (fun () -> write bus3 () () ());
      assert_no_allocation bus4
        (fun () () () () -> ())
        (fun () -> write bus4 () () () ());
    ;;

    let%test_unit "raising in [on_callback_raise] closes the bus" =
      let t =
        create [%here] Arity1 ~allow_subscription_after_first_write:false
          ~on_callback_raise:Error.raise
      in
      iter_exn (read_only t) [%here] ~f:(fun _ -> failwith "");
      assert (does_raise (fun () -> write t ()));
      assert (is_closed t);
    ;;

    let%test_unit "fold threads values through future calls" =
      let bus = create1 [%here] ~allow_subscription_after_first_write:false in
      let ro = read_only bus in
      let last1 = ref 0 in
      let last2 = ref "" in
      fold_exn ro [%here] Arity1 ~init:0 ~f:(fun acc () ->
        let this = acc + 1 in
        last1 := this;
        this);
      fold_exn ro [%here] Arity1 ~init:"" ~f:(fun acc () ->
        let this = acc ^ "." in
        last2 := this;
        this);
      write bus ();
      write bus ();
      write bus ();
      [%test_result: int] !last1 ~expect:3;
      [%test_result: string] !last2 ~expect:"...";
      write bus ();
      write bus ();
      [%test_result: int] !last1 ~expect:5;
      [%test_result: string] !last2 ~expect:".....";
    ;;

  end
  (* This signature constraint is here to remind us to add a unit test whenever
     the interface to [Bus] changes. *)
  : module type of Bus))
