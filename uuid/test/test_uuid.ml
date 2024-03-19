open! Core
open! Uuid
open! Uuid.Private
open! Expect_test_helpers_core

let%expect_test _ =
  Quickcheck.test_distinct_values
    ~sexp_of:Unstable.sexp_of_t
    quickcheck_generator
    ~trials:1_024
    ~distinct_values:512
    ~compare;
  [%expect {| |}]
;;

let%expect_test _ =
  Quickcheck.test ~sexp_of:sexp_of_t quickcheck_generator ~f:invariant;
  [%expect {| |}]
;;

let%expect_test "All hex digits are correctly encoded in lowercase" =
  let conversions = List.init 16 ~f:(fun int -> int, bottom_4_bits_to_hex_char int) in
  print_s [%sexp (conversions : (int * Char.t) list)];
  [%expect
    {|
    ((0  0)
     (1  1)
     (2  2)
     (3  3)
     (4  4)
     (5  5)
     (6  6)
     (7  7)
     (8  8)
     (9  9)
     (10 a)
     (11 b)
     (12 c)
     (13 d)
     (14 e)
     (15 f))
    |}]
;;

let%test_module "create_rand does a reasonable creation" =
  (module struct
    let all_bytes_are_hex_or_dash t =
      String.for_all t ~f:(function
        | '-' | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    ;;

    let%expect_test "almost v4" =
      let state = Random.State.make [||] in
      let create_rand_and_verify () =
        let t = create_random state in
        is_valid_exn t;
        [%test_pred: String.t] all_bytes_are_hex_or_dash (t |> to_string);
        print_endline (t |> to_string)
      in
      for _ = 0 to 10 do
        create_rand_and_verify ()
      done;
      [%expect
        {|
        c14ac950-b432-46ed-7d19-622539d60705
        a7b6c250-74a1-4865-d844-0f363cd03b01
        fd08e817-23af-4c3e-468a-2f62976e42fc
        9d013e20-a916-4ac6-e32b-a5c086d493b8
        5d03ec13-5f28-4ad7-4874-96abccf22ece
        31e69bdf-77e8-4ba0-9537-204f9027a9eb
        0acbe3d4-1c12-4123-2b37-a73bcaffc8b3
        1c70ce60-708a-46da-c92e-8edaf429ffef
        69a984c2-8e41-4004-65d3-b52f4d263bd9
        d6f253c4-f8c8-4390-b3a7-5b201fc215e9
        11051874-0aca-4276-eaa4-0476434d3f12
        |}]
    ;;
  end)
;;
