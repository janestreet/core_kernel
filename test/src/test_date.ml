open! Core_kernel
open! Import
open! Date
open! Date.Private

let%expect_test _ =
  print_and_check_container_sexps
    [%here]
    (module Date)
    [ Date.of_string "1955-11-12"
    ; Date.of_string "1985-10-26"
    ; Date.of_string "2015-10-21"
    ];
  [%expect
    {|
    (Set (1955-11-12 1985-10-26 2015-10-21))
    (Map (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2)))
    (Hash_set (1955-11-12 1985-10-26 2015-10-21))
    (Table (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2))) |}]
;;

let%expect_test "Date.V1" =
  print_and_check_stable_type
    [%here]
    (module Date.Stable.V1)
    [ Date.create_exn ~y:1066 ~m:Oct ~d:16
    ; Date.create_exn ~y:1955 ~m:Nov ~d:5
    ; Date.create_exn ~y:2012 ~m:Apr ~d:19
    ];
  [%expect
    {|
    (bin_shape_digest 47681bb034560d96024e1b2eca0d98ca)
    ((sexp   1066-10-16)
     (bin_io "\254*\004\t\016"))
    ((sexp   1955-11-05)
     (bin_io "\254\163\007\n\005"))
    ((sexp   2012-04-19)
     (bin_io "\254\220\007\003\019")) |}]
;;

let%expect_test "Date.V1.Set" =
  print_and_check_stable_type
    [%here]
    (module Date.Stable.V1.Set)
    [ Date.Set.empty
    ; Date.Set.singleton (Date.create_exn ~y:1066 ~m:Oct ~d:16)
    ; Date.Set.of_list
        [ Date.create_exn ~y:1955 ~m:Nov ~d:5; Date.create_exn ~y:2012 ~m:Apr ~d:19 ]
    ; Date.Set.of_list
        [ Date.create_exn ~y:1066 ~m:Oct ~d:16
        ; Date.create_exn ~y:1955 ~m:Nov ~d:5
        ; Date.create_exn ~y:2012 ~m:Apr ~d:19
        ]
    ];
  [%expect
    {|
    (bin_shape_digest ccde15fc17afce11a067d80e40cb1e8d)
    ((sexp ()) (bin_io "\000"))
    ((sexp (1066-10-16)) (bin_io "\001\254*\004\t\016"))
    ((sexp (1955-11-05 2012-04-19))
     (bin_io "\002\254\163\007\n\005\254\220\007\003\019"))
    ((sexp (1066-10-16 1955-11-05 2012-04-19))
     (bin_io "\003\254*\004\t\016\254\163\007\n\005\254\220\007\003\019")) |}]
;;

let%expect_test "Date.V1.Map" =
  let module T = struct
    type t = string Date.Stable.V1.Map.t [@@deriving bin_io, compare, sexp]
  end
  in
  print_and_check_stable_type
    [%here]
    (module T)
    [ Date.Map.empty
    ; Date.Map.singleton
        (Date.create_exn ~y:1066 ~m:Oct ~d:16)
        "not the Battle of Hastings"
    ; Date.Map.of_alist_exn
        [ Date.create_exn ~y:1955 ~m:Nov ~d:5, "flux capacitor"
        ; Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday"
        ]
    ; Date.Map.of_alist_exn
        [ Date.create_exn ~y:1066 ~m:Oct ~d:16, "not the Battle of Hastings"
        ; Date.create_exn ~y:1955 ~m:Nov ~d:5, "flux capacitor"
        ; Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday"
        ]
    ];
  [%expect
    {|
    (bin_shape_digest a0aa3c6d1173d784fbd03980ac5d0be5)
    ((sexp ()) (bin_io "\000"))
    ((sexp ((1066-10-16 "not the Battle of Hastings")))
     (bin_io "\001\254*\004\t\016\026not the Battle of Hastings"))
    ((sexp (
       (1955-11-05 "flux capacitor")
       (2012-04-19 "a Thursday")))
     (bin_io
      "\002\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday"))
    ((sexp (
       (1066-10-16 "not the Battle of Hastings")
       (1955-11-05 "flux capacitor")
       (2012-04-19 "a Thursday")))
     (bin_io
      "\003\254*\004\t\016\026not the Battle of Hastings\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday")) |}]
;;

let%test_unit "create_exn doesn't allocate" =
  let allocation_before = Gc.major_plus_minor_words () in
  ignore (Date.create_exn ~y:1999 ~m:Dec ~d:31 : Date.t);
  let allocation_after = Gc.major_plus_minor_words () in
  [%test_eq: int] allocation_before allocation_after
;;

let%test_unit "creation and destruction" =
  let test y m d =
    let t = Date.create_exn ~y ~m ~d in
    [%test_result: int] ~expect:y (Date.year t);
    [%test_result: Month.t] ~expect:m (Date.month t);
    [%test_result: int] ~expect:d (Date.day t)
  in
  test 2014 Month.Sep 24;
  test 9999 Month.Dec 31
;;

let%expect_test "add_years" =
  let test string =
    let date = Date.of_string string in
    for years = -4 to 4 do
      let date_plus_years = Date.add_years date years in
      printf !"%{Date} + %2d years = %{Date}\n" date years date_plus_years
    done
  in
  (* non-leap day *)
  test "2013-10-07";
  [%expect
    {|
    2013-10-07 + -4 years = 2009-10-07
    2013-10-07 + -3 years = 2010-10-07
    2013-10-07 + -2 years = 2011-10-07
    2013-10-07 + -1 years = 2012-10-07
    2013-10-07 +  0 years = 2013-10-07
    2013-10-07 +  1 years = 2014-10-07
    2013-10-07 +  2 years = 2015-10-07
    2013-10-07 +  3 years = 2016-10-07
    2013-10-07 +  4 years = 2017-10-07 |}];
  (* leap day maps to Feb 28 on non-leap years (and 400-year century behaves properly) *)
  test "2004-02-29";
  [%expect
    {|
    2004-02-29 + -4 years = 2000-02-29
    2004-02-29 + -3 years = 2001-02-28
    2004-02-29 + -2 years = 2002-02-28
    2004-02-29 + -1 years = 2003-02-28
    2004-02-29 +  0 years = 2004-02-29
    2004-02-29 +  1 years = 2005-02-28
    2004-02-29 +  2 years = 2006-02-28
    2004-02-29 +  3 years = 2007-02-28
    2004-02-29 +  4 years = 2008-02-29 |}];
  (* non-leap year century behaves properly *)
  test "1904-02-29";
  [%expect
    {|
    1904-02-29 + -4 years = 1900-02-28
    1904-02-29 + -3 years = 1901-02-28
    1904-02-29 + -2 years = 1902-02-28
    1904-02-29 + -1 years = 1903-02-28
    1904-02-29 +  0 years = 1904-02-29
    1904-02-29 +  1 years = 1905-02-28
    1904-02-29 +  2 years = 1906-02-28
    1904-02-29 +  3 years = 1907-02-28
    1904-02-29 +  4 years = 1908-02-29 |}]
;;

let%test_module "week_number" =
  (module struct
    let%test_unit _ =
      [%test_result: int] (ordinal_date (create_exn ~y:2014 ~m:Jan ~d:1)) ~expect:1
    ;;

    let%test_unit _ =
      [%test_result: int] (ordinal_date (create_exn ~y:2014 ~m:Dec ~d:31)) ~expect:365
    ;;

    let%test_unit _ =
      [%test_result: int] (ordinal_date (create_exn ~y:2014 ~m:Feb ~d:28)) ~expect:59
    ;;

    let test_week_number y m d ~expect =
      [%test_result: int] (week_number (create_exn ~y ~m ~d)) ~expect
    ;;

    let%test_unit _ = test_week_number 2014 Jan 1 ~expect:1
    let%test_unit _ = test_week_number 2014 Dec 31 ~expect:1
    let%test_unit _ = test_week_number 2010 Jan 1 ~expect:53
    let%test_unit _ = test_week_number 2017 Jan 1 ~expect:52
    let%test_unit _ = test_week_number 2014 Jan 10 ~expect:2
    let%test_unit _ = test_week_number 2012 Jan 1 ~expect:52
    let%test_unit _ = test_week_number 2012 Dec 31 ~expect:1
  end)
;;

let%test_module "diff_weekdays" =
  (module struct
    let c y m d = create_exn ~y ~m ~d

    let%test "2014 Jan 1 is a Wednesday" =
      Day_of_week.( = ) (day_of_week (c 2014 Jan 1)) Day_of_week.Wed
    ;;

    let ( = ) = Int.( = )

    (* future minus Wednesday *)
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 1) = 0
    let%test _ = diff_weekdays (c 2014 Jan 2) (c 2014 Jan 1) = 1
    let%test _ = diff_weekdays (c 2014 Jan 3) (c 2014 Jan 1) = 2
    let%test _ = diff_weekdays (c 2014 Jan 4) (c 2014 Jan 1) = 3
    let%test _ = diff_weekdays (c 2014 Jan 5) (c 2014 Jan 1) = 3
    let%test _ = diff_weekdays (c 2014 Jan 6) (c 2014 Jan 1) = 3
    let%test _ = diff_weekdays (c 2014 Jan 7) (c 2014 Jan 1) = 4
    let%test _ = diff_weekdays (c 2014 Jan 8) (c 2014 Jan 1) = 5
    let%test _ = diff_weekdays (c 2014 Jan 9) (c 2014 Jan 1) = 6
    let%test _ = diff_weekdays (c 2014 Jan 10) (c 2014 Jan 1) = 7
    let%test _ = diff_weekdays (c 2014 Jan 11) (c 2014 Jan 1) = 8
    let%test _ = diff_weekdays (c 2014 Jan 12) (c 2014 Jan 1) = 8
    let%test _ = diff_weekdays (c 2014 Jan 13) (c 2014 Jan 1) = 8
    let%test _ = diff_weekdays (c 2014 Jan 14) (c 2014 Jan 1) = 9
    (* Wednesday minus future *)
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 2) = -1
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 3) = -2
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 4) = -3
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 5) = -3
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 6) = -3
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 7) = -4
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 8) = -5
    let%test _ = diff_weekdays (c 2014 Jan 1) (c 2014 Jan 9) = -6
    (* diff_weekend_days *)
    let%test _ = diff_weekend_days (c 2014 Jan 1) (c 2014 Jan 1) = 0
    let%test _ = diff_weekend_days (c 2014 Jan 2) (c 2014 Jan 1) = 0
    let%test _ = diff_weekend_days (c 2014 Jan 3) (c 2014 Jan 1) = 0
    let%test _ = diff_weekend_days (c 2014 Jan 4) (c 2014 Jan 1) = 0
    let%test _ = diff_weekend_days (c 2014 Jan 5) (c 2014 Jan 1) = 1
    let%test _ = diff_weekend_days (c 2014 Jan 6) (c 2014 Jan 1) = 2
    let%test _ = diff_weekend_days (c 2014 Jan 7) (c 2014 Jan 1) = 2
    let%test _ = diff_weekend_days (c 2014 Jan 8) (c 2014 Jan 1) = 2
    let%test _ = diff_weekend_days (c 2014 Jan 9) (c 2014 Jan 1) = 2
    let%test _ = diff_weekend_days (c 2014 Jan 10) (c 2014 Jan 1) = 2
    let%test _ = diff_weekend_days (c 2014 Jan 11) (c 2014 Jan 1) = 2
    let%test _ = diff_weekend_days (c 2014 Jan 12) (c 2014 Jan 1) = 3
    let%test _ = diff_weekend_days (c 2014 Jan 13) (c 2014 Jan 1) = 4
    let%test _ = diff_weekend_days (c 2014 Jan 14) (c 2014 Jan 1) = 4
  end)
;;

let%test_module "ordinal_date" =
  (module struct
    (* check the ordinal date tables we found on wikipedia... *)
    let check_table year ordinal_date_table =
      let days_of_year =
        dates_between
          ~min:(create_exn ~y:year ~m:Month.Jan ~d:01)
          ~max:(create_exn ~y:year ~m:Month.Dec ~d:31)
      in
      [%test_result: int]
        (List.length days_of_year)
        ~expect:(if is_leap_year ~year then 366 else 365);
      let months =
        List.group days_of_year ~break:(fun d d' -> Month.( <> ) (month d) (month d'))
      in
      let sum =
        List.foldi months ~init:0 ~f:(fun index sum month ->
          [%test_result: int] sum ~expect:ordinal_date_table.(index);
          sum + List.length month)
      in
      [%test_result: int] sum ~expect:(List.length days_of_year)
    ;;

    let%test_unit _ = check_table 2015 non_leap_year_table
    let%test_unit _ = check_table 2000 leap_year_table
  end)
;;

let%test_module "weekdays_between" =
  (module struct
    let c y m d = create_exn ~y ~m ~d

    (* systematic test of consistency between [weekdays_between] and [diff_weekdays] *)
    let dates =
      [ c 2014 Jan 1
      ; c 2014 Jan 2
      ; c 2014 Jan 3
      ; c 2014 Jan 4
      ; c 2014 Jan 5
      ; c 2014 Jan 6
      ; c 2014 Jan 7
      ; c 2014 Feb 15
      ; c 2014 Feb 16
      ; c 2014 Feb 17
      ; c 2014 Feb 18
      ; c 2014 Feb 19
      ; c 2014 Feb 20
      ; c 2014 Feb 21
      ]
    ;;

    let ( = ) = Int.( = )

    let%test_unit _ =
      List.iter dates ~f:(fun date1 ->
        List.iter dates ~f:(fun date2 ->
          if date1 <= date2
          then
            assert (
              List.length (weekdays_between ~min:date1 ~max:(add_days date2 (-1)))
              = diff_weekdays date2 date1)))
    ;;
  end)
;;

let%test_module "first_strictly_after" =
  (module struct
    let mon1 = create_exn ~y:2013 ~m:Month.Apr ~d:1
    let tue1 = create_exn ~y:2013 ~m:Month.Apr ~d:2
    let wed1 = create_exn ~y:2013 ~m:Month.Apr ~d:3
    let thu1 = create_exn ~y:2013 ~m:Month.Apr ~d:4
    let fri1 = create_exn ~y:2013 ~m:Month.Apr ~d:5
    let sat1 = create_exn ~y:2013 ~m:Month.Apr ~d:6
    let sun1 = create_exn ~y:2013 ~m:Month.Apr ~d:7
    let mon2 = create_exn ~y:2013 ~m:Month.Apr ~d:8
    let tue2 = create_exn ~y:2013 ~m:Month.Apr ~d:9

    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Mon) mon2
    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Tue) tue2
    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Wed) wed1
    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Thu) thu1
    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Fri) fri1
    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Sat) sat1
    let%test _ = equal (first_strictly_after tue1 ~on:Day_of_week.Sun) sun1
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Mon) mon2
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Tue) tue1
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Wed) wed1
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Thu) thu1
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Fri) fri1
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Sat) sat1
    let%test _ = equal (first_strictly_after mon1 ~on:Day_of_week.Sun) sun1
  end)
;;

let%test_unit _ =
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of:sexp_of_t ~f:(fun t ->
    t = of_string "1900-01-01")
;;

let%test_unit _ =
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of:sexp_of_t ~f:(fun t ->
    t = of_string "2100-01-01")
;;

let%test_unit _ =
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of:sexp_of_t ~f:(fun t ->
    of_string "1900-01-01" < t && t < of_string "2100-01-01")
;;

let%test_unit _ =
  Quickcheck.test_distinct_values
    quickcheck_generator
    ~sexp_of:sexp_of_t
    ~compare
    ~trials:1_000
    ~distinct_values:500
;;
