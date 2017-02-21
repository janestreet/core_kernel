open! Core_kernel
open  Expect_test_helpers_kernel

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Date) [
    Date.of_string "1955-11-12";
    Date.of_string "1985-10-26";
    Date.of_string "2015-10-21";
  ];
  [%expect {|
    (Set (1955-11-12 1985-10-26 2015-10-21))
    (Map (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2)))
    (Hash_set (1955-11-12 1985-10-26 2015-10-21))
    (Table (
      (1955-11-12 0)
      (1985-10-26 1)
      (2015-10-21 2))) |}];
;;

let%expect_test "Date.V1" =
  print_and_check_stable_type [%here] (module Date.Stable.V1) [
    Date.create_exn ~y:1066 ~m:Oct ~d:16;
    Date.create_exn ~y:1955 ~m:Nov ~d: 5;
    Date.create_exn ~y:2012 ~m:Apr ~d:19;
  ];
  [%expect {|
    (bin_shape_digest 47681bb034560d96024e1b2eca0d98ca)
    ((sexp   1066-10-16)
     (bin_io "\254*\004\t\016"))
    ((sexp   1955-11-05)
     (bin_io "\254\163\007\n\005"))
    ((sexp   2012-04-19)
     (bin_io "\254\220\007\003\019")) |}];
;;

let%expect_test "Date.V1.Set" =
  print_and_check_stable_type [%here] (module Date.Stable.V1.Set) [
    Date.Set.empty;
    Date.Set.singleton (Date.create_exn ~y:1066 ~m:Oct ~d:16);
    Date.Set.of_list [
      Date.create_exn ~y:1955 ~m:Nov ~d: 5;
      Date.create_exn ~y:2012 ~m:Apr ~d:19;
    ];
    Date.Set.of_list [
      Date.create_exn ~y:1066 ~m:Oct ~d:16;
      Date.create_exn ~y:1955 ~m:Nov ~d: 5;
      Date.create_exn ~y:2012 ~m:Apr ~d:19;
    ];
  ];
  [%expect {|
    (bin_shape_digest ccde15fc17afce11a067d80e40cb1e8d)
    ((sexp ()) (bin_io "\000"))
    ((sexp (1066-10-16)) (bin_io "\001\254*\004\t\016"))
    ((sexp (1955-11-05 2012-04-19))
     (bin_io "\002\254\163\007\n\005\254\220\007\003\019"))
    ((sexp (1066-10-16 1955-11-05 2012-04-19))
     (bin_io "\003\254*\004\t\016\254\163\007\n\005\254\220\007\003\019")) |}];
;;

let%expect_test "Date.V1.Map" =
  let module T = struct
    type t = string Date.Stable.V1.Map.t
    [@@deriving bin_io, compare, sexp]
  end in
  print_and_check_stable_type [%here] (module T) [
    Date.Map.empty;
    Date.Map.singleton (Date.create_exn ~y:1066 ~m:Oct ~d:16)
      "not the Battle of Hastings";
    Date.Map.of_alist_exn [
      Date.create_exn ~y:1955 ~m:Nov ~d: 5, "flux capacitor";
      Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday";
    ];
    Date.Map.of_alist_exn [
      Date.create_exn ~y:1066 ~m:Oct ~d:16, "not the Battle of Hastings";
      Date.create_exn ~y:1955 ~m:Nov ~d: 5, "flux capacitor";
      Date.create_exn ~y:2012 ~m:Apr ~d:19, "a Thursday";
    ];
  ];
  [%expect {|
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
      "\003\254*\004\t\016\026not the Battle of Hastings\254\163\007\n\005\014flux capacitor\254\220\007\003\019\na Thursday")) |}];
;;
