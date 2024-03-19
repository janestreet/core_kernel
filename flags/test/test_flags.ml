open! Core
open! Expect_test_helpers_core
open! Flags

let a = Int63.of_int 0x1
let b = Int63.of_int 0x2
let c = Int63.of_int 0xC
let d = create ~bit:62

let%expect_test _ =
  List.iter [ -1; 63 ] ~f:(fun bit ->
    show_raise ~hide_positions:true (fun () -> ignore (create ~bit : Int63.t)));
  [%expect
    {|
    (raised (
      "Flags.create got invalid ~bit (must be between 0 and 62)"
      -1
      lib/flags/src/flags.ml:LINE:COL))
    (raised (
      "Flags.create got invalid ~bit (must be between 0 and 62)"
      63
      lib/flags/src/flags.ml:LINE:COL))
    |}]
;;

let%expect_test _ =
  let print_hex bit = print_s [%sexp (create ~bit |> Int63.Hex.to_string_hum : string)] in
  print_hex 0;
  [%expect {| 0x1 |}];
  print_hex 1;
  [%expect {| 0x2 |}];
  print_hex 62;
  [%expect {| -0x4000_0000_0000_0000 |}]
;;

module Flags = struct
  let allow_intersecting = false
  let should_print_error = true
  let known = [ a, "a"; b, "b"; c, "c" ]
  let remove_zero_flags = false
end

module M = Make (Flags)
include M

let%expect_test _ =
  print_and_check_comparable_sexps [%here] (module M) [ a; b; c ];
  [%expect
    {|
    (Set (
      (a)
      (b)
      (c)))
    (Map (
      ((a) 0)
      ((b) 1)
      ((c) 2)))
    |}]
;;

(* [sexp_of_t] *)
let print_sexp_of t = print_s [%sexp (t : t)]

let%expect_test _ =
  print_sexp_of empty;
  print_sexp_of a;
  print_sexp_of b;
  print_sexp_of c;
  print_sexp_of d;
  [%expect
    {|
    ()
    (a)
    (b)
    (c)
    (() (unrecognized_bits 0x4000000000000000))
    |}]
;;

let%expect_test _ =
  print_sexp_of (a + b);
  [%expect {| (a b) |}]
;;

let%expect_test _ =
  print_sexp_of (Int63.of_int 0x10);
  [%expect {| (() (unrecognized_bits 0x10)) |}]
;;

(* [t_of_sexp] *)
let check_t_of_sexp here expect string =
  require here (equal expect (t_of_sexp (Sexp.of_string string)))
;;

let%expect_test _ = check_t_of_sexp [%here] empty "()"
let%expect_test _ = check_t_of_sexp [%here] a "(a)"
let%expect_test _ = check_t_of_sexp [%here] c "(c)"
let%expect_test _ = check_t_of_sexp [%here] (b + c) "(b c)"
let%expect_test _ = check_t_of_sexp [%here] (b + c) "(c b)"

let%expect_test _ =
  List.iter [ "a"; "(())"; "(a ())"; "(d)" ] ~f:(fun string ->
    show_raise (fun () ->
      print_s [%message "" ~input:string];
      ignore (t_of_sexp (Sexp.of_string string) : t)));
  [%expect
    {|
    (input a)
    (raised ("Of_sexp_error: list needed" (sexp a)))
    (input "(())")
    (raised (Of_sexp_error "string_of_sexp: atom needed" (invalid_sexp ())))
    (input "(a ())")
    (raised (Of_sexp_error "string_of_sexp: atom needed" (invalid_sexp ())))
    (input "(d)")
    (raised (
      Of_sexp_error "Flags.t_of_sexp got unknown name: d" (invalid_sexp (d))))
    |}]
;;

(* +, - *)
let%expect_test _ = require [%here] (equal (a + a) a)
let%expect_test _ = require [%here] (equal (a + b) (b + a))
let%expect_test _ = require [%here] (equal (a - a) empty)
let%expect_test _ = require [%here] (equal (a + b - a) b)

(* [intersect] *)
let%expect_test _ = require [%here] (equal (intersect a a) a)
let%expect_test _ = require [%here] (equal (intersect a b) empty)
let%expect_test _ = require [%here] (equal (intersect (a + b) a) a)

(* [complement] *)
let%expect_test _ = require [%here] (equal (intersect (complement a) b) b)

(* [do_intersect] *)
let%expect_test _ = require [%here] (do_intersect a a)
let%expect_test _ = require [%here] (not (do_intersect a b))
let%expect_test _ = require [%here] (do_intersect (a + b) a)
let%expect_test _ = require [%here] (do_intersect (a + b) b)
let%expect_test _ = require [%here] (not (do_intersect (a + b) c))

(* [are_disjoint] *)
let%expect_test _ = require [%here] (are_disjoint a empty)
let%expect_test _ = require [%here] (not (are_disjoint a a))
let%expect_test _ = require [%here] (are_disjoint a b)
let%expect_test _ = require [%here] (are_disjoint b a)
let%expect_test _ = require [%here] (not (are_disjoint (a + b) a))
let%expect_test _ = require [%here] (are_disjoint (a + b) c)

(* compare *)
let%expect_test _ = require [%here] (Int.( = ) (Int.compare 0 1) (-1))
let print_compare t1 t2 = print_s [%sexp (compare t1 t2 : int)]

let%expect_test _ =
  print_compare a empty;
  [%expect {| 1 |}];
  print_compare c empty;
  [%expect {| 1 |}];
  print_compare a a;
  [%expect {| 0 |}];
  print_compare c c;
  [%expect {| 0 |}];
  print_compare empty empty;
  [%expect {| 0 |}];
  print_compare empty a;
  [%expect {| -1 |}];
  print_compare empty c;
  [%expect {| -1 |}];
  print_compare (a + c) a;
  [%expect {| 1 |}];
  print_compare (a + c) c;
  [%expect {| 1 |}];
  print_compare (b + b) b;
  [%expect {| 0 |}];
  print_compare b (b + c);
  [%expect {| -1 |}]
;;

(* Create all combinations of the given flags. The output is ordered if the input is also
   ordered. *)
let combinations flags =
  let rec combinations prefix flags =
    match flags with
    | [] -> [ prefix ]
    | h :: t ->
      let x = combinations prefix t in
      let y = combinations (h :: prefix) t in
      List.concat [ x; y ]
  in
  combinations [] (List.rev flags) |> List.map ~f:(List.fold ~init:M.empty ~f:M.( + ))
;;

let%expect_test "[compare] is a total order consistent with [is_subset]" =
  print_compare a b;
  [%expect {| -1 |}];
  print_compare b (b + d);
  [%expect {| -1 |}];
  print_compare a (b + d);
  [%expect {| -1 |}];
  let test_ordering ordered =
    List.is_sorted_strictly ordered ~compare:(fun f1 f2 ->
      require [%here] (not (M.is_subset f2 ~of_:f1));
      M.compare f1 f2)
  in
  let known = List.map Flags.known ~f:fst in
  (* known flags *)
  let ordered = combinations known in
  (* show the computed ordering *)
  print_s [%message (ordered : M.t list)];
  [%expect
    {|
    (ordered (
      ()
      (a)
      (b)
      (a b)
      (c)
      (a c)
      (b c)
      (a b c)))
    |}];
  require [%here] (test_ordering ordered);
  let complements = List.map ordered ~f:M.complement in
  print_s [%message (complements : M.t list)];
  [%expect
    {|
    (complements (
      (a b c)
      (b c)
      (a c)
      (c)
      (a b)
      (b)
      (a)
      ()))
    |}];
  (* complemented flags (forms a reversed ordering) *)
  let ordered = complements |> List.rev in
  require [%here] (test_ordering ordered);
  (* With "unknown" flag [d] *)
  let ordered = combinations (known @ [ d ]) in
  require [%here] (test_ordering ordered)
;;

(* Check that conflicting flags leads to an error. *)
let%test _ =
  Result.is_error
    (Result.try_with (fun () ->
       let module _ =
         Make (struct
           let allow_intersecting = false
           let should_print_error = false
           let known = [ Int63.of_int 0x1, ""; Int63.of_int 0x1, "" ]
           let remove_zero_flags = false
         end)
       in
       ()))
;;

let%expect_test "complement" =
  let flags = M.(a + c) in
  let complement = M.complement flags in
  print_s [%message (flags : M.t) (complement : M.t)];
  [%expect {| ((flags (a c)) (complement (b))) |}]
;;

let%expect_test "ensure sexp representation of flags having only recognized bits \
                 round-trips"
  =
  let t_list = [ M.empty; a; M.(a + b) ] in
  List.iter t_list ~f:(fun t ->
    let sexp = [%sexp (t : t)] in
    let t_recovered = t_of_sexp sexp in
    Expect_test_helpers_core.require_equal [%here] (module M) t t_recovered);
  [%expect {| |}]
;;

let%expect_test "ensure sexp representation of flag with unrecognized bits round-trips" =
  let t = Int63.max_value in
  let sexp = [%sexp (t : t)] in
  print_s sexp;
  [%expect {| ((a b c) (unrecognized_bits 0x3ffffffffffffff0)) |}];
  let t_recovered = t_of_sexp sexp in
  Expect_test_helpers_core.require_equal [%here] (module M) t t_recovered;
  print_s [%message (t : t) (t_recovered : t) (sexp : Sexp.t)];
  [%expect
    {|
    ((t           ((a b c) (unrecognized_bits 0x3ffffffffffffff0)))
     (t_recovered ((a b c) (unrecognized_bits 0x3ffffffffffffff0)))
     (sexp        ((a b c) (unrecognized_bits 0x3ffffffffffffff0))))
    |}]
;;

let gen_int63 = Int63.gen_incl Int63.min_value Int63.max_value

let gen_flag_and_name =
  Quickcheck.Generator.tuple2 gen_int63 (String.gen_with_length 10 Char.gen_alpha)
;;

let gen_scenario =
  let%bind.Quickcheck len = Int.gen_incl 0 63 in
  let%map.Quickcheck known = List.gen_with_length len gen_flag_and_name in
  let known =
    List.dedup_and_sort known ~compare:(fun (_, name) (_, name1) ->
      String.compare name name1)
  in
  let module M =
    Make (struct
      let allow_intersecting = true
      let should_print_error = true
      let known = known
      let remove_zero_flags = true
    end)
  in
  (module M : S)
;;

let gen_sexp_round_trip_scenario =
  Quickcheck.Generator.tuple2 gen_scenario (Int.gen_incl 0 Int.max_value)
;;

let%expect_test "quickcheck test: ensure sexps round-trip for general scenarios" =
  Quickcheck.test ~trials:1000 gen_sexp_round_trip_scenario ~f:(fun ((module M : S), i) ->
    let t = M.of_int i in
    let sexp = [%sexp (t : M.t)] in
    let t_recovered = M.t_of_sexp sexp in
    Expect_test_helpers_core.require_equal [%here] (module M) t t_recovered);
  [%expect {| |}]
;;

let%expect_test "quickcheck_test: existing hex representations can still be converted \
                 back."
  =
  let to_unsigned_hex_string_old num = sprintf "0x%Lx" (Int63.to_int64 num) in
  let of_unsigned_hex_string s = Int64.Hex.of_string s |> Int63.of_int64_trunc in
  Quickcheck.test ~trials:1000 gen_int63 ~f:(fun int63 ->
    let old_format_hex_num = to_unsigned_hex_string_old int63 in
    let int63_recovered = of_unsigned_hex_string old_format_hex_num in
    Expect_test_helpers_core.require_equal [%here] (module Int63) int63 int63_recovered);
  [%expect {| |}]
;;

let%expect_test "old hex repr of 2^62 still deserializes correctly" =
  let num = 1 lsl 62 |> Int63.of_int in
  let to_unsigned_hex_string_old num = sprintf "0x%Lx" (Int63.to_int64 num) in
  let to_unsigned_hex_string_new num =
    Int64.(max_value land Int63.to_int64 num) |> Int64.Hex.to_string
  in
  let of_unsigned_hex_string s = Int64.Hex.of_string s |> Int63.of_int64_trunc in
  let old_hex_str = to_unsigned_hex_string_old num in
  let new_hex_str = to_unsigned_hex_string_new num in
  Expect_test_helpers_core.require_equal
    [%here]
    (module Int63)
    (of_unsigned_hex_string old_hex_str)
    (of_unsigned_hex_string new_hex_str);
  [%expect {| |}]
;;
