open! Core_kernel.Std
open! Flags

let a = Int63.of_int 0x1
let b = Int63.of_int 0x2
let c = Int63.of_int 0xC

let%test_unit _ =
  List.iter [ -1; 63 ] ~f:(fun bit ->
    assert (Exn.does_raise (fun () -> create ~bit)))
;;

let%test_unit _ =
  assert (create ~bit:0 = Int63.of_int 0x1);
  assert (create ~bit:1 = Int63.of_int 0x2);
  (* this constant is a string rather than an int so that it builds on 32bit *)
  assert (create ~bit:62 = Int63.of_string "0x4000_0000_0000_0000")
;;

module M = Make (struct
    let allow_intersecting = false
    let should_print_error = true
    let known =
      [ a, "a";
        b, "b";
        c, "c";
      ]
    ;;
    let remove_zero_flags = false
  end)

include M

include Comparable.Check_sexp_conversion (struct
    include M
    let examples = [ a; b; c ]
  end)

(* [sexp_of_t] *)
let%test _ = Sexp.equal (sexp_of_t empty)   Sexp.(List [])
let%test _ = Sexp.equal (sexp_of_t a)       Sexp.(List [ Atom "a" ])
let%test _ = Sexp.equal (sexp_of_t c)       Sexp.(List [ Atom "c" ])
let%test _ = Sexp.equal (sexp_of_t (a + b)) Sexp.(List [ Atom "a"; Atom "b" ])
let%test_unit _ = ignore (sexp_of_t (Int63.of_int 0x10) : Sexp.t)

(* [t_of_sexp] *)
let%test _ = equal empty (t_of_sexp (Sexp.of_string "()"))
let%test _ = equal a (t_of_sexp (Sexp.of_string "(a)"))
let%test _ = equal c (t_of_sexp (Sexp.of_string "(c)"))
let%test _ = equal (b + c) (t_of_sexp (Sexp.of_string "(b c)"))
let%test _ = equal (b + c) (t_of_sexp (Sexp.of_string "(c b)"))
let%test_unit _ =
  List.iter [ "a"; "(())"; "(a ())"; "(d)" ] ~f:(fun sexp ->
    let sexp = Sexp.of_string sexp in
    match Result.try_with (fun () -> t_of_sexp sexp) with
    | Error _ -> ()
    | Ok t -> failwiths "invalid sexp converted" (sexp, t) [%sexp_of: Sexp.t * t])

(* +, - *)
let%test _ = equal (a + a) a
let%test _ = equal (a + b) (b + a)
let%test _ = equal (a - a) empty
let%test _ = equal ((a + b) - a) b

(* [intersect] *)
let%test _ = equal (intersect a a) a
let%test _ = equal (intersect a b) empty
let%test _ = equal (intersect (a + b) a) a

(* [complement] *)
let%test _ = equal (intersect (complement a) b) b

(* [do_intersect] *)
let%test _ = do_intersect a a
let%test _ = not (do_intersect a b)
let%test _ = do_intersect (a + b) a
let%test _ = do_intersect (a + b) b
let%test _ = not (do_intersect (a + b) c)

(* [are_disjoint] *)
let%test _ = are_disjoint a empty
let%test _ = not (are_disjoint a a)
let%test _ = are_disjoint a b
let%test _ = are_disjoint b a
let%test _ = not (are_disjoint (a + b) a)
let%test _ = are_disjoint (a + b) c

(* compare *)
let%test _ = Int.(=) (Int.compare 0 1) (-1)
let%test _ = Int.(=) (compare a empty) 1
let%test _ = Int.(=) (compare c empty) 1
let%test _ = Int.(=) (compare a a) 0
let%test _ = Int.(=) (compare c c) 0
let%test _ = Int.(=) (compare empty empty) 0
let%test _ = Int.(=) (compare empty a) (-1)
let%test _ = Int.(=) (compare empty c) (-1)
let%test _ = Int.(=) (compare (a + c) a) 1
let%test _ = Int.(=) (compare (a + c) c) 1
let%test _ = Int.(=) (compare (b + b) b) 0
let%test _ = Int.(=) (compare b (b + c)) (-1)
let%test _ = Int.(=) (compare b (b + c)) (-1)
