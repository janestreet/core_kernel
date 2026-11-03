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

module%test [@name "create_rand does a reasonable creation"] _ = struct
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
end

module%test [@name "of_string validation (SWAR dash count)"] _ = struct
  let accepts (s : string) =
    match of_string s with
    | (_ : t) -> true
    | exception _ -> false
  ;;

  (* The pre-change validation rule, used as an oracle. ([open! Uuid] shadows [=] with
     Uuid's t-comparison, so use [Int.equal] for the int checks.) *)
  let reference (s : string) =
    Int.equal (String.length s) 36
    && Int.equal (String.count s ~f:(Char.equal '-')) 4
    && Char.equal s.[8] '-'
    && Char.equal s.[13] '-'
    && Char.equal s.[18] '-'
    && Char.equal s.[23] '-'
  ;;

  let valid = "c14ac950-b432-46ed-7d19-622539d60705"

  let%expect_test "explicit accept/reject" =
    require (accepts valid);
    (* a ',' (0x2C) in a hex slot is accepted, matching the original [String.count] check
       -- regression test for the zero-byte-trick borrow bug (see the [dash_mask] comment
       in uuid.ml) *)
    require (accepts "c14ac950-,432-46ed-7d19-622539d60705");
    (* wrong length *)
    require (not (accepts (valid ^ "0")));
    require (not (accepts (String.drop_suffix valid 1)));
    (* stray dash in a hex slot -> count <> 4, still length 36 *)
    require
      (not (accepts (String.mapi valid ~f:(fun i c -> if Int.equal i 0 then '-' else c))));
    (* missing dash at a required position (13) -> count <> 4 and position check fails *)
    require
      (not
         (accepts (String.mapi valid ~f:(fun i c -> if Int.equal i 13 then '0' else c))));
    [%expect {| |}]
  ;;

  let%expect_test "verify validity under single character edits" =
    let check s =
      if not (Bool.equal (accepts s) (reference s))
      then raise_s [%message "SWAR validation disagrees with reference" (s : string)]
    in
    require (accepts valid);
    (* Mutate one byte at a time and restore it, rather than allocating a fresh string per
       edit. [Bytes.unsafe_to_string] is safe here because [check] only reads the string
       (via [accepts]/[reference]) and does not retain it. *)
    let buf = Bytes.of_string valid in
    List.iter [ '-'; ','; '.'; '0'; 'f'; '\000'; '\255' ] ~f:(fun c ->
      for i = 0 to Bytes.length buf - 1 do
        let orig = Bytes.get buf i in
        Bytes.set buf i c;
        Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf |> check;
        Bytes.set buf i orig
      done);
    List.iter [ 30; 33; 35; 36 ] ~f:(fun len -> String.prefix valid len |> check);
    check (valid ^ "0");
    check (valid ^ "0000");
    [%expect {| |}]
  ;;
end
