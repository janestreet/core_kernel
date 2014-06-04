open Std_internal

include Flags_intf

let create ~bit:n =
  if n < 0 || n > 62 then
    failwiths "Flags.create got invalid ~bit (must be between 0 and 62)"
      n <:sexp_of< int >>;
  Int63.shift_left Int63.one n
;;

module Make (M : Make_arg) = struct
  type t = Int63.t with typerep

  let of_int     = Int63.of_int
  let to_int_exn = Int63.to_int_exn

  let empty = Int63.zero

  let is_empty t = t = empty

  let (+) a b = Int63.bit_or a b
  let (-) a b = Int63.bit_and a (Int63.bit_not b)

  let intersect  = Int63.bit_and
  let complement = Int63.bit_not

  let do_intersect t1 t2 = Int63.(<>) (Int63.bit_and t1 t2) Int63.zero
  let are_disjoint t1 t2 = Int63.(=)  (Int63.bit_and t1 t2) Int63.zero

  let error message a sexp_of_a =
    let e = Error.create message a sexp_of_a in
    if M.should_print_error then
      eprintf "%s\n%!" (Sexp.to_string_hum (Error.sexp_of_t e));
    Error.raise e;
  ;;

  let known =
    if M.remove_zero_flags then
      List.filter ~f:(fun (n, _) -> not (Int63.equal n Int63.zero)) M.known
    else M.known
  ;;

  let () =
    if not M.allow_intersecting then begin
      let rec check l ac =
        match l with
        | [] -> ac
        | (flag, name) :: l ->
          let bad = List.filter l ~f:(fun (flag', _) -> do_intersect flag flag') in
          let ac = if List.is_empty bad then ac else (flag, name, bad) :: ac in
          check l ac
      in
      let bad = check known [] in
      if not (List.is_empty bad) then
        error "Flags.Make got intersecting flags" bad
          (<:sexp_of< (Int63.t * string * (Int63.t * string) list) list >>);
    end;
  ;;

  let () =
    let bad = List.filter known ~f:(fun (flag, _) -> flag = Int63.zero) in
    if not (List.is_empty bad) then
      error "Flag.Make got flags with no bits set" bad
        (<:sexp_of< (Int63.t * string) list >>)
  ;;

  type sexp_format = string list with sexp

  let sexp_of_t =
    (* We reverse [known] so that the fold below accumulates from right to left, giving a
       final list with elements in the same order as [known]. *)
    let known = List.rev known in
    fun t ->
      let leftover, flag_names =
        List.fold known ~init:(t, []) ~f:(fun (t, flag_names) (flag, flag_name) ->
          if Int63.bit_and t flag = flag
          then (t - flag, flag_name :: flag_names)
          else (t, flag_names))
      in
      if leftover = empty
      then <:sexp_of< sexp_format >> flag_names
      else
        <:sexp_of< string list * [ `unrecognized_bits of string ] >>
          (flag_names, `unrecognized_bits (sprintf "0x%Lx" (Int63.to_int64 leftover)))
  ;;

  let known_by_name =
    String.Table.of_alist_exn (List.map known ~f:(fun (mask, name) -> name, mask))
  ;;

  let t_of_sexp sexp =
    List.fold (sexp |> <:of_sexp< sexp_format >>) ~init:empty ~f:(fun t name ->
      match Hashtbl.find known_by_name name with
      | Some mask -> t + mask
      | None -> of_sexp_error (sprintf "Flags.t_of_sexp got unknown name: %s" name) sexp)
  ;;

  let compare t u =
    if t = u then 0
    else if t - u = empty then -1
    else if u - t = empty then 1
    else compare t u                    (* arbitrary but consistent with subset *)

  include Comparable.Make (struct type nonrec t = t with sexp, compare end)
end

(* Check that conflicting flags leads to an error. *)
TEST =
  Result.is_error
    (Result.try_with (fun () ->
      let module M =
            Make (struct
              let allow_intersecting = false
              let should_print_error = false
              let known = [ Int63.of_int 0x1, "";
                            Int63.of_int 0x1, "";
                          ]
              let remove_zero_flags = false
            end)
      in
      ()))
;;

TEST_MODULE = struct
  let a = Int63.of_int 0x1
  let b = Int63.of_int 0x2
  let c = Int63.of_int 0xC

  TEST_UNIT =
    List.iter [ -1; 63 ] ~f:(fun bit ->
      assert (does_raise (fun () -> create ~bit)))
  ;;

  TEST_UNIT =
    assert (create ~bit:0 = Int63.of_int 0x1);
    assert (create ~bit:1 = Int63.of_int 0x2);
    (* this constant is a string rather than an int so that it builds on 32bit *)
    assert (create ~bit:62 = Int63.of_string "0x4000_0000_0000_0000");
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
  TEST = Sexp.equal (sexp_of_t empty)   Sexp.(List [])
  TEST = Sexp.equal (sexp_of_t a)       Sexp.(List [ Atom "a" ])
  TEST = Sexp.equal (sexp_of_t c)       Sexp.(List [ Atom "c" ])
  TEST = Sexp.equal (sexp_of_t (a + b)) Sexp.(List [ Atom "a"; Atom "b" ])
  TEST_UNIT = ignore (sexp_of_t (Int63.of_int 0x10) : Sexp.t)

  (* [t_of_sexp] *)
  TEST = equal empty (t_of_sexp (Sexp.of_string "()"))
  TEST = equal a (t_of_sexp (Sexp.of_string "(a)"))
  TEST = equal c (t_of_sexp (Sexp.of_string "(c)"))
  TEST = equal (b + c) (t_of_sexp (Sexp.of_string "(b c)"))
  TEST = equal (b + c) (t_of_sexp (Sexp.of_string "(c b)"))
  TEST_UNIT =
    List.iter [ "a"; "(())"; "(a ())"; "(d)" ] ~f:(fun sexp ->
      let sexp = Sexp.of_string sexp in
      match Result.try_with (fun () -> t_of_sexp sexp) with
      | Error _ -> ()
      | Ok t -> failwiths "invalid sexp converted" (sexp, t) <:sexp_of< Sexp.t * t >>)

  (* +, - *)
  TEST = equal (a + a) a
  TEST = equal (a + b) (b + a)
  TEST = equal (a - a) empty
  TEST = equal ((a + b) - a) b

  (* [intersect] *)
  TEST = equal (intersect a a) a
  TEST = equal (intersect a b) empty
  TEST = equal (intersect (a + b) a) a

  (* [complement] *)
  TEST = equal (intersect (complement a) b) b

  (* [do_intersect] *)
  TEST = do_intersect a a
  TEST = not (do_intersect a b)
  TEST = do_intersect (a + b) a
  TEST = do_intersect (a + b) b
  TEST = not (do_intersect (a + b) c)

  (* [are_disjoint] *)
  TEST = are_disjoint a empty
  TEST = not (are_disjoint a a)
  TEST = are_disjoint a b
  TEST = are_disjoint b a
  TEST = not (are_disjoint (a + b) a)
  TEST = are_disjoint (a + b) c

  (* compare *)
  TEST = Int.(=) (Int.compare 0 1) (-1)
  TEST = Int.(=) (compare a empty) 1
  TEST = Int.(=) (compare c empty) 1
  TEST = Int.(=) (compare a a) 0
  TEST = Int.(=) (compare c c) 0
  TEST = Int.(=) (compare empty empty) 0
  TEST = Int.(=) (compare empty a) (-1)
  TEST = Int.(=) (compare empty c) (-1)
  TEST = Int.(=) (compare (a + c) a) 1
  TEST = Int.(=) (compare (a + c) c) 1
  TEST = Int.(=) (compare (b + b) b) 0
  TEST = Int.(=) (compare b (b + c)) (-1)
  TEST = Int.(=) (compare b (b + c)) (-1)
end
