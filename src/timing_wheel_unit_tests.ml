open Std_internal
open! Int.Replace_polymorphic_compare
open Timing_wheel_intf

module Make (Timing_wheel : Timing_wheel)
  : sig
    (** This interface is only here to remind us to add a unit test whenever
        [Timing_wheel] changes. *)
    include module type of Timing_wheel

    open Timing_wheel

    val create_unit
      :  ?level_bits      : Level_bits.t
      -> ?start           : Time.t
      -> ?alarm_precision : Time.Span.t
      -> unit
      -> unit t
  end = struct

  (* module Timing_wheel = Timing_wheel_debug.Make (Time) (Timing_wheel)
   * let () = Timing_wheel.show_messages := false *)

  module Debug_in_this_dir = Debug
  open Timing_wheel
  module Debug = Debug_in_this_dir

  module Interval_num = Interval_num
  module Time = Time

  let does_raise = Exn.does_raise
  let sec = Time.Span.of_sec

  module Level_bits = struct

    open Level_bits

    type nonrec t = t

    let invariant = invariant

    let max_num_bits = max_num_bits

    let%test _ = max_num_bits = Int64.num_bits - 3

    let create_exn = create_exn
    let t_of_sexp = t_of_sexp

    let%test_unit _ =
      List.iter
        [ []
        ; [ 0 ]
        ; [ -1 ]
        ; [ 2; 0; 1 ]
        ; [ max_num_bits + 1 ]
        ; List.init (max_num_bits + 1) ~f:Fn.id
        ]
        ~f:(fun level_bits ->
          assert (does_raise (fun () -> Level_bits.create_exn level_bits));
          assert (does_raise (fun () -> t_of_sexp ([%sexp_of: int list] level_bits))))
    ;;

    let default = default

    let%test_unit _ = invariant default

    let num_bits = num_bits
    let sexp_of_t = sexp_of_t

    let%test_unit _ =
      List.iter
        [ [ 1 ]      , 1
        ; [ 1; 1 ]   , 2
        ; [ 1; 2; 3 ], 6
        ]
        ~f:(fun (bits, expect) ->
          let t = create_exn bits in
          assert (num_bits t = expect);
          let sexp = sexp_of_t t in
          assert (Sexp.equal sexp (sexp_of_t (t_of_sexp sexp))))
    ;;

  end

  module Config = struct

    open Config

    type nonrec t = t [@@deriving sexp]

    let invariant = invariant

    let create = create
    let alarm_precision = alarm_precision

    let%test _ = does_raise (fun () -> create ~alarm_precision:(sec (-1.)) ())
    let%test _ = does_raise (fun () -> create ~alarm_precision:(sec 0.) ())
    let%test _ = Time.Span.equal (sec 1.) (alarm_precision (create ~alarm_precision:(sec 1.) ()))

    let level_bits = level_bits

    let durations = durations

    let%test_unit _ =
      List.iter
        [ [ 1 ]   , [ sec 2. ]
        ; [ 2; 1 ], [ sec 4.; sec 8. ]
        ]
        ~f:(fun (level_bits, expect) ->
          assert (Poly.equal expect
                    (durations (create
                                  ~alarm_precision:(sec 1.)
                                  ~level_bits:(Level_bits.create_exn level_bits)
                                  ()))))
    ;;

  end

  module Priority_queue = struct

    open Priority_queue

    type nonrec 'a t = 'a t [@@deriving sexp_of]

    type 'a priority_queue = 'a t

    let invariant = invariant

    module Key = Key

    module Elt = struct
      open Elt

      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let invariant = invariant
      let key = key
      let value = value
    end

    let create = create

    let create_unit ~level_bits =
      create ~level_bits:(Level_bits.create_exn level_bits) ()
    ;;

    let min_allowed_key = min_allowed_key
    let max_allowed_key = max_allowed_key

    let%test_unit _ =
      List.iter
        [ [ 1 ]      , 1
        ; [ 1; 1 ]   , 5
        ; [ 1; 1; 1 ], 11
        ; [ 2 ]      , 3
        ; [ 3 ]      , 7
        ; [ 3; 1 ]   , 23
        ]
        ~f:(fun (level_bits, expected_max_allowed_key) ->
          let t = create_unit ~level_bits in
          [%test_result: Key.t] (min_allowed_key t) ~expect:Key.zero;
          [%test_result: Key.t] (max_allowed_key t)
            ~expect:(Key.of_int expected_max_allowed_key))
    ;;

    let add        = add
    let change_key = change_key
    let is_empty   = is_empty
    let length     = length
    let mem        = mem
    let remove     = remove

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1 ] in
      assert (is_empty t);
      assert (length t = 0);
      let e1 = add t ~key:Key.zero () in
      let e2 = add t ~key:Key.zero () in
      assert (mem t e1);
      assert (mem t e2);
      assert (not (is_empty t));
      assert (length t = 2);
      remove t e1;
      assert (not (mem t e1));
      assert (mem t e2);
      assert (length t = 1);
      assert (not (is_empty t));
      change_key t e2 ~key:(Key.of_int 1);
      assert (Key.equal (Elt.key t e2) (Key.of_int 1));
      assert (not (mem t e1));
      assert (does_raise (fun () -> change_key t e1 ~key:(Key.of_int 1)));
      assert (mem t e2);
      assert (length t = 1);
      assert (not (is_empty t));
      remove t e2;
      assert (not (mem t e1));
      assert (not (mem t e2));
      assert (length t = 0);
      assert (is_empty t)
    ;;

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1 ] in
      let add ~key = ignore (add t ~key () : _ Elt.t) in
      for key = Key.to_int_exn (min_allowed_key t)
        to      Key.to_int_exn (max_allowed_key t)
      do
        add ~key:(Key.of_int key);
      done;
      let check_adds_fail () =
        List.iter
          [ Key.min_value
          ; Key.pred (min_allowed_key t)
          ; Key.succ (max_allowed_key t)
          ; Key.succ Key.max_representable
          ; Key.max_value
          ]
          ~f:(fun key -> assert (does_raise (fun () -> add ~key)))
      in
      check_adds_fail ();
      increase_min_allowed_key t ~key:Key.one ~handle_removed:ignore;
      check_adds_fail ();
      increase_min_allowed_key t ~key:(max_allowed_key t) ~handle_removed:ignore;
      check_adds_fail ();
      increase_min_allowed_key t ~key:Key.max_representable ~handle_removed:ignore;
      check_adds_fail ()
    ;;

    let clear = clear
    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1; 1 ] in
      clear t;
      let e1 = add t ~key:Key.zero       () in
      let e2 = add t ~key:(Key.of_int 2) () in
      clear t;
      assert (is_empty t);
      assert (not (mem t e1));
      assert (not (mem t e2))
    ;;

    let increase_min_allowed_key_return_removed_keys t ~key =
      let r = ref [] in
      let handle_removed elt = r := Elt.key t elt :: !r in
      increase_min_allowed_key t ~key ~handle_removed;
      !r
    ;;

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1 ] in
      let add ~key = ignore (add t ~key () : _ Elt.t) in
      add ~key:Key.zero;
      add ~key:Key.one;
      assert (does_raise (fun () ->
        increase_min_allowed_key t ~key:Key.( succ max_representable )
          ~handle_removed:ignore));
      increase_min_allowed_key t ~key:Key.max_representable ~handle_removed:ignore;
      assert (is_empty t);
      [%test_result: Key.t] (min_allowed_key t) ~expect:Key.max_representable;
      [%test_result: Key.t] (max_allowed_key t) ~expect:Key.max_representable;
      add ~key:Key.max_representable;
      assert (length t = 1)
    ;;

    let increase_min_allowed_key = increase_min_allowed_key

    let%test_unit _ =
      (* [all_sums n] returns all combinations of nonnegative ints that sum to [n]. *)
      let all_sums n =
        let results = Array.create ~len:(n + 1) [] in
        results.( 0 ) <- [[]];
        for i = 1 to n do
          results.( i ) <-
            List.concat
              (List.init i ~f:(fun j ->
                 let first = j + 1 in
                 List.map results.( i - first ) ~f:(fun rest -> first :: rest)));
        done;
        results.( n )
      in
      let test ~num_bits ~level_bits ~initial_min_allowed_key ~step =
        if false
        then Debug.eprints "test" (`num_bits num_bits, `level_bits level_bits,
                                   `initial_min_allowed_key initial_min_allowed_key,
                                   `step step)
               [%sexp_of: [`num_bits of int] * [`level_bits of int list]
                            * [`initial_min_allowed_key of Key.t]
                            * [`step of Key.Span.t]];
        let t = create_unit ~level_bits in
        try
          increase_min_allowed_key t ~key:initial_min_allowed_key ~handle_removed:ignore;
          [%test_result: Key.t] (min_allowed_key t) ~expect:initial_min_allowed_key;
          assert (Key.( >= ) (max_allowed_key t)
                    (Key.add (min_allowed_key t)
                       (Key.Span.of_int63 Int63.( shift_left one num_bits - one ))));
          let keys =
            List.init (Key.Span.to_int_exn
                         (Key.diff (max_allowed_key t) (min_allowed_key t)))
              ~f:(fun i -> Key.add (min_allowed_key t) (Key.Span.of_int i))
          in
          let n = ref 0 in
          List.iter keys ~f:(fun key ->
            ignore (add t ~key () : _ Elt.t);
            incr n;
            assert (length t = !n));
          let removed = ref [] in
          while length t > 0 do
            let keys_removed =
              increase_min_allowed_key_return_removed_keys t
                ~key:(Key.min Key.max_representable
                        (Key.add (min_allowed_key t) step))
            in
            removed := keys_removed @ !removed;
            List.iter keys_removed ~f:(fun key -> assert (Key.( < ) key (min_allowed_key t)))
          done;
          let keys_removed = List.sort !removed ~cmp:Key.compare in
          assert (Poly.equal keys_removed keys);
        with exn ->
          failwiths "failure" (exn, t) [%sexp_of: exn * _ t]
      in
      let num_bits = 6 in
      let all_sums = all_sums num_bits in
      List.iter
        [ Key.zero
        ; Key.sub Key.max_representable
            (Key.Span.of_int63 (Int63.shift_left Int63.one num_bits))
        ]
        ~f:(fun initial_min_allowed_key ->
          for step = 1 to 1 lsl num_bits do
            List.iter all_sums ~f:(fun level_bits ->
              test ~num_bits ~level_bits ~initial_min_allowed_key
                ~step:(Key.Span.of_int step))
          done)
    ;;

    let min_elt = min_elt
    let min_key = min_key

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
      assert (is_none (min_key t));
      let _elt = add t ~key:Key.zero () in
      [%test_result: Key.t option] (min_key t) ~expect:(Some Key.zero);
      let max_key = 10 in
      for key = 1 to max_key; do
        let key = Key.of_int key in
        assert (is_ok (Result.try_with (fun () -> add t ~key ())));
        [%test_result: Key.t option] (min_key t) ~expect:(Some Key.zero);
      done;
      for key = 1 to max_key + 1; do
        let key = Key.of_int key in
        begin match increase_min_allowed_key_return_removed_keys t ~key with
        | [ key' ] -> [%test_result: Key.t] key' ~expect:(Key.pred key);
        | _ -> assert false
        end;
        [%test_result: Key.t option] (min_key t)
          ~expect:(if Key.( <= ) key (Key.of_int max_key) then Some key else None);
      done
    ;;

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
      let max_key = Key.of_int 10 in
      let elts =
        List.init (Key.to_int_exn max_key + 1)
          ~f:(fun key -> add t ~key:(Key.of_int key) ())
      in
      List.iter elts ~f:(fun elt ->
        let key = Elt.key t elt in
        remove t elt;
        [%test_result: Key.t option]
          (min_key t)
          ~expect:(if Key.( < ) key max_key then Some (Key.succ key) else None))
    ;;

    let iter = iter

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
      let count () =
        let r = ref 0 in
        iter t ~f:(fun _ -> incr r);
        !r
      in
      assert (count () = 0);
      let num_elts = 10 in
      for key = 0 to num_elts - 1; do
        ignore (add t ~key:(Key.of_int key) () : _ Elt.t);
      done;
      assert (count () = num_elts);
      increase_min_allowed_key t ~key:Key.one ~handle_removed:ignore;
      assert (count () = num_elts - 1);
      increase_min_allowed_key t ~key:(Key.of_int num_elts) ~handle_removed:ignore;
      assert (count () = 0)
    ;;

    let%test_unit _ =
      let t = create_unit ~level_bits:[ 1; 1; 1; 1 ] in
      let elts = ref [] in
      for key = 0 to Key.to_int_exn (max_allowed_key t) do
        elts := add t ~key:(Key.of_int key) () :: !elts
      done;
      let elts' = ref [] in
      iter t ~f:(fun elt -> elts' := elt :: !elts');
      let sort elts =
        List.sort elts ~cmp:(fun elt1 elt2 ->
          Key.compare (Elt.key t elt1) (Elt.key t elt2))
      in
      assert (List.equal ~equal:phys_equal (sort !elts) (sort !elts'))
    ;;
  end

  type nonrec 'a t = 'a t [@@deriving sexp_of]

  type nonrec 'a t_now = 'a t_now [@@deriving sexp_of]

  type 'a timing_wheel = 'a t

  module Alarm = struct
    open Alarm

    type nonrec 'a t = 'a t [@@deriving sexp_of]

    let null = null

    let at           = at
    let interval_num = interval_num
    let value        = value
  end

  let invariant = invariant

  let create_unit ?level_bits ?(start = Time.epoch) ?(alarm_precision = sec 1.) () =
    create ~config:(Config.create ?level_bits ~alarm_precision ()) ~start
  ;;

  let%test_unit "start after epoch" =
    let t = create_unit ~start:(Time.add Time.epoch (Time.Span.of_sec 1.)) () in
    invariant ignore t;
  ;;

  let create = create

  let%test_unit _ =
    List.iter
      [ sec (-1.)
      ; sec 0.
      ]
      ~f:(fun alarm_precision ->
        assert (does_raise (fun () -> create_unit ~alarm_precision ())))
  ;;

  let alarm_upper_bound = alarm_upper_bound

  let interval_num       = interval_num
  let interval_num_start = interval_num_start
  let interval_start     = interval_start

  let%test_unit _ =
    let t = create_unit () in
    assert (not (mem t (Alarm.null ())));
    let start = start t in
    List.iter
      [ 0.,   0
      ; 0.1,  0
      ; 0.99, 0
      ; 1.,   1
      ; 1.5,  1
      ; 1.99, 1
      ; 2.,   2
      ]
      ~f:(fun (after, expected_interval) ->
        let time = Time.add start (sec after) in
        [%test_result: Interval_num.t]
          (interval_num t time)
          ~expect:(Interval_num.of_int expected_interval);
        assert (Time.equal
                  (interval_num_start t (interval_num t time))
                  (interval_start t time));
        assert (Time.equal
                  (interval_start t time)
                  (Time.add start (sec (Float.of_int expected_interval)))))
  ;;

  let now = now
  let start = start
  let alarm_precision = alarm_precision

  let%test_unit _ =
    let t = create_unit () in
    assert (Time.Span.equal (alarm_precision t) (sec 1.));
    assert (Time.equal (now t) Time.epoch);
    assert (Time.equal (start t) Time.epoch);
    let to_ = Time.add (now t) (sec 1.) in
    advance_clock t ~to_ ~handle_fired:ignore;
    assert (Time.equal (now t) to_);
    assert (Time.equal (start t) Time.epoch)
  ;;

  let is_empty = is_empty
  let length = length

  let%test_unit _ =
    let t = create_unit () in
    assert (is_empty t);
    assert (length t = 0);
    let alarm = add t ~at:(now t) () in
    assert (not (is_empty t));
    assert (length t = 1);
    remove t alarm;
    assert (is_empty t);
    assert (length t = 0)
  ;;

  let iter = iter

  let%test_unit _ =
    let t = create_unit () in
    iter t ~f:(fun _ -> assert false);
    let alarm1 = add t ~at:(now t) () in
    iter t ~f:(fun alarm -> assert (phys_equal alarm alarm1));
    let alarm2 = add t ~at:(now t) () in
    let r = ref 0 in
    iter t ~f:(fun alarm ->
      assert (phys_equal alarm alarm1 || phys_equal alarm alarm2);
      incr r);
    assert (!r = 2);
    remove t alarm1;
    remove t alarm2;
    iter t ~f:(fun _ -> assert false)
  ;;

  let clear = clear
  (* Already tested above for [Priority_queue]. *)

  let mem = mem
  let remove = remove

  (* Check that access to a removed alarm doesn't segfault. *)
  let%test_unit _ =
    let t =
      create
        ~config:(Config.create ~alarm_precision:(sec 1.) ())
        ~start:Time.epoch
    in
    let alarm = add t ~at:(Time.add (now t) (sec 5.)) (ref 1) in
    assert (mem t alarm);
    remove t alarm;
    assert (not (mem t alarm));
    assert (does_raise (fun _ -> Alarm.interval_num t alarm));
    assert (does_raise (fun _ -> Alarm.at t alarm));
    assert (does_raise (fun _ -> Alarm.value t alarm))
  ;;

  let reschedule                 = reschedule
  let reschedule_at_interval_num = reschedule_at_interval_num

  (* Check that [reschedule] and [reschedule_at_interval_num] leave an alarm in the timing
     wheel but reschedule its scheduled time. *)
  let%test_unit _ =
    List.iter
      [ (fun t alarm ~at -> reschedule                 t alarm ~at)
      ; (fun t alarm ~at -> reschedule_at_interval_num t alarm ~at:(interval_num t at))
      ]
      ~f:(fun reschedule ->
        let epoch_plus n_seconds = Time.add Time.epoch (sec n_seconds) in
        let t =
          create
            ~config:(Config.create ~alarm_precision:(sec 1.) ())
            ~start:(epoch_plus 0.)
        in
        (* add alarm1 before alarm2, test initial conditions *)
        let alarm1 = add t ~at:(epoch_plus 5.)  () in
        let alarm2 = add t ~at:(epoch_plus 10.) () in
        assert (mem t alarm1);
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm1) (epoch_plus 5.);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 0.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 6.));
        (* reschedule alarm1 after alarm2, make sure alarm2 becomes next *)
        reschedule t alarm1 ~at:(epoch_plus 15.);
        assert (mem t alarm1);
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm1) (epoch_plus 15.);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 0.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 11.));
        (* advance time past alarm1's original time, make sure nothing fires *)
        advance_clock t ~to_:(epoch_plus 7.) ~handle_fired:(fun _ -> assert false);
        assert (mem t alarm1);
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm1) (epoch_plus 15.);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 7.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 11.));
        (* reschedule alarm1 before alarm2 again, make sure alarm1 becomes next *)
        reschedule t alarm1 ~at:(epoch_plus 8.);
        assert (mem t alarm1);
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm1) (epoch_plus 8.);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 7.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 9.));
        (* advance time past alarm1, make sure alarm1 fires but alarm2 does not *)
        let fired1 = ref false in
        advance_clock t ~to_:(epoch_plus 9.) ~handle_fired:(fun _ -> fired1 := true);
        assert !fired1;
        assert (not (mem t alarm1));
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 9.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 11.));
        (* cannot reschedule already-fired alarm *)
        assert (does_raise (fun _ -> reschedule t alarm1 ~at:(epoch_plus 20.)));
        assert (not (mem t alarm1));
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 9.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 11.));
        (* cannot reschedule to before current time *)
        assert (does_raise (fun _ -> reschedule t alarm2 ~at:(epoch_plus 8.)));
        assert (not (mem t alarm1));
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 9.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 11.));
        (* cannot reschedule arbitrarily far in the future *)
        assert (does_raise (fun _ ->
          reschedule t alarm2 ~at:(Time.add (alarm_upper_bound t) (sec 1.))));
        assert (not (mem t alarm1));
        assert (mem t alarm2);
        [%test_eq: Time.t] (Alarm.at t alarm2) (epoch_plus 10.);
        [%test_eq: Time.t] (now t) (epoch_plus 9.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) (Some (epoch_plus 11.));
        (* fire alarm2 *)
        let fired2 = ref false in
        advance_clock t ~to_:(epoch_plus 11.) ~handle_fired:(fun _ -> fired2 := true);
        assert !fired2;
        assert (not (mem t alarm1));
        assert (not (mem t alarm2));
        [%test_eq: Time.t] (now t) (epoch_plus 11.);
        [%test_eq: Time.t option] (next_alarm_fires_at t) None)
  ;;

  let add                 = add
  let add_at_interval_num = add_at_interval_num
  let advance_clock       = advance_clock
  let now_interval_num    = now_interval_num

  (* No early alarms *)
  let%test_unit _ =
    let test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by =
      if false
      then Debug.eprints "test" (num_alarms, alarm_precision, alarm_separation, advance_by)
             [%sexp_of: int * Time.Span.t * Time.Span.t * Time.Span.t];
      let t =
        create ~config:(Config.create ~alarm_precision ()) ~start:Time.epoch
      in
      for i = 1 to num_alarms do
        let at = Time.add (now t) (Time.Span.scale alarm_separation (Float.of_int i)) in
        ignore (add t ~at (fun () -> assert (Time.( <= ) at (now t))) : _ Alarm.t);
      done;
      while not (is_empty t) do
        let to_ = Time.add (now t) advance_by in
        advance_clock t ~to_ ~handle_fired:(fun alarm -> Alarm.value t alarm ());
        [%test_result: Interval_num.t] (now_interval_num t) ~expect:(interval_num t to_);
      done;
    in
    List.iter
      [ add
      ; (fun t ~at a -> add_at_interval_num t ~at:(interval_num t at) a)
      ]
      ~f:(fun add ->
        List.iter [ 100 ] ~f:(fun num_alarms ->
          List.iter [ 1.; 0.5; 0.1 ] ~f:(fun s ->
            let alarm_precision = sec s in
            List.iter [ 0.01; 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
              let alarm_separation = sec s in
              List.iter [ 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
                let advance_by = sec s in
                test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by)))))
  ;;

  let%test_unit _ =
    let t =
      create
        ~config:(Config.create
                   ~alarm_precision:(sec 1.)
                   ~level_bits:(Level_bits.create_exn [ 10 ])
                   ())
        ~start:Time.epoch
    in
    let num_alarms () =
      let r = ref 0 in
      iter t ~f:(fun _ -> incr r);
      !r
    in
    let add ~after f = ignore (add t ~at:(Time.add (now t) after) f : _ Alarm.t) in
    let advance_clock by =
      advance_clock t ~to_:(Time.add (now t) by)
        ~handle_fired:(fun alarm -> Alarm.value t alarm ())
    in
    assert (does_raise (fun () -> add ~after:(sec (-1.)) ignore));
    assert (Time.equal (alarm_upper_bound t) (Time.add (now t) (sec 1024.)));
    assert (num_alarms () = 0);
    assert (is_none (next_alarm_fires_at t));
    let fired = ref false in
    add ~after:(sec 30.) (fun () -> fired := true);
    assert (Poly.equal (next_alarm_fires_at t) (Some (Time.add (now t) (sec 31.))));
    assert (num_alarms () = 1);
    advance_clock (sec 30.);
    assert (not !fired);
    advance_clock (sec 1.);
    assert !fired
  ;;

  let next_alarm_fires_at = next_alarm_fires_at

  let next_alarm_fires_at_exn = next_alarm_fires_at_exn

  let%test_unit _ =
    let t = create_unit ~level_bits:(Level_bits.create_exn [ 10 ]) () in
    let add_at at = ignore (add t ~at:(Time.add Time.epoch at) () : _ Alarm.t) in
    let no_next_alarm () = is_none (next_alarm_fires_at t) in
    let next_alarm_fires_at span =
      match next_alarm_fires_at t with
      | None -> false
      | Some at -> Time.( = ) at (Time.add Time.epoch span)
    in
    let advance_clock span =
      advance_clock t ~to_:(Time.add Time.epoch span) ~handle_fired:ignore
    in
    assert (no_next_alarm ());
    add_at (sec 2.);
    assert (next_alarm_fires_at (sec 3.));
    add_at (sec 1.5);
    assert (next_alarm_fires_at (sec 2.));
    add_at (sec 1.0);
    assert (next_alarm_fires_at (sec 2.));
    add_at (sec 0.5);
    assert (next_alarm_fires_at (sec 1.));
    add_at (sec 0.1);
    assert (next_alarm_fires_at (sec 1.));
    advance_clock (sec 0.5);
    assert (next_alarm_fires_at (sec 1.));
    advance_clock (sec 1.);
    assert (next_alarm_fires_at (sec 2.));
    advance_clock (sec 1.5);
    assert (next_alarm_fires_at (sec 2.));
    advance_clock (sec 2.);
    assert (next_alarm_fires_at (sec 3.));
    advance_clock (sec 3.);
    assert (no_next_alarm ())
  ;;

  let fire_past_alarms = fire_past_alarms

  let%test_unit _ = (* all possible subsets of alarms in the first bucket that fire *)
    let start = Time.epoch in
    let at sec = Time.add start (Time.Span.of_sec sec) in
    let at1 = at 1. in
    let at2 = at 2. in
    for num_elts = 0 to 5 do
      let rec loop i ats =
        if i > 0
        then (
          loop (i - 1) (at1 :: ats);
          loop (i - 1) (at2 :: ats))
        else (
          let t =
            create ~start
              ~config:(Config.create ~alarm_precision:(Time.Span.of_sec 60.) ())
          in
          let num_fired = ref 0 in
          List.iter ats ~f:(fun at ->
            let alarm = add t ~at () in
            [%test_result: Interval_num.t] (Alarm.interval_num t alarm)
              ~expect:Interval_num.zero);
          advance_clock t ~to_:at1 ~handle_fired:(fun _ -> assert false);
          fire_past_alarms t ~handle_fired:(fun alarm ->
            if Time.equal (Alarm.at t alarm) at1
            then incr num_fired
            else assert false);
          [%test_result: int] !num_fired ~expect:(List.count ats ~f:(Time.equal at1)));
      in
      loop num_elts []
    done
  ;;

  let%test_unit _ =
    let start = Time.epoch in
    let t : bool ref t =
      create ~config:(Config.create ~alarm_precision:(sec 1.) ()) ~start
    in
    let handle_fired (a : bool ref Alarm.t) : unit =
      let r = Alarm.value t a in
      assert (not !r);
      r := true
    in
    let precision = alarm_precision t in
    let precision_0_2 = Time.Span.scale precision 0.2 in
    let _ = add t ~at:(Time.add start precision) (ref false) in
    let base = next_alarm_fires_at t |> Option.value_exn in
    let step0 = Time.add base  precision_0_2 in
    let step1 = Time.add step0 precision_0_2 in
    let step2 = Time.add step1 precision_0_2 in
    let step3 = Time.add step2 precision in
    (* Check all alarm will be in the same bucket but step3 *)
    let interval_num0  = interval_num t step0 in
    let interval_num1  = interval_num t step1 in
    let interval_num2  = interval_num t step2 in
    let interval_num3  = interval_num t step3 in
    [%test_result: Interval_num.t] interval_num2 ~expect:interval_num0;
    [%test_result: Interval_num.t] interval_num1 ~expect:interval_num0;
    assert (Interval_num.( <> ) interval_num0 interval_num3);
    let step1_fired = ref false in
    let step2_fired = ref false in
    let step3_fired = ref false in
    let _ = add t ~at:step1 step1_fired in
    let _ = add t ~at:step2 step2_fired in
    let _ = add t ~at:step3 step3_fired in
    (* Nothing should be triggered before *)
    advance_clock t ~to_:step0 ~handle_fired;
    fire_past_alarms t ~handle_fired;
    assert (not !step1_fired);
    assert (not !step2_fired);
    assert (not !step3_fired);
    (* Check only step1_fired get trigger at step1 *)
    advance_clock t ~to_:step1 ~handle_fired;
    assert (not !step1_fired);
    fire_past_alarms t ~handle_fired;
    assert (!step1_fired);
    assert (not !step2_fired);
    assert (not !step3_fired);
    (* Check only step2_fired get trigger at step2 *)
    advance_clock t ~to_:step2 ~handle_fired;
    assert (not !step2_fired);
    fire_past_alarms t ~handle_fired;
    assert (!step1_fired);
    assert (!step2_fired);
    assert (not !step3_fired)
  ;;
end
