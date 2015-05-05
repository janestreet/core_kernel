open Typerep_lib.Std

(* Tests to make sure the implementation of values hasn't changed and that witnesses are
   provided correctly.  *)

(* Tests are bound to the value returned by Immediate.of_typerep, as the test cases are
   designed along the value returned and so that they must be revised if changes are
   made. *)
TEST_MODULE = struct
  type t = Always | Sometimes | Never

  (* Where appropriate, unit is used as a representative of something that will always be
     immediate and int ref is used as a representative of something that will always be
     boxed, no matter the actual representation. *)
  let imm_t = typerep_of_unit
  let box_t = typerep_of_ref typerep_of_int
  let is_imm v = Obj.is_int (Obj.repr v)

  (* When [expected] and the actual result is [Sometimes], at least one immediate and one
     non-immediate value must be supplied. It requires at least one value in any case.  *)
  let check expected typerep list =
    if (List.length list = 0)
    then failwith "Must provide at least one test case.";
    let actual =
      Type_immediacy.dest (Type_immediacy.of_typerep typerep),
      Type_immediacy.Always.   of_typerep typerep,
      Type_immediacy.Sometimes.of_typerep typerep,
      Type_immediacy.Never.    of_typerep typerep
    in
    let has_imm = List.exists is_imm list in
    let has_boxed = List.exists (fun x -> not (is_imm x)) list in
    match has_imm, has_boxed, expected, actual with
    | true , false, Always   , (Type_immediacy.Always    _, Some _, None  , None  )
      -> true
    | true , true , Sometimes, (Type_immediacy.Sometimes _, None  , Some _, None  )
      -> true
    | false, true , Never    , (Type_immediacy.Never     _, None  , None  , Some _)
      -> true
    | _, _, _, _
      -> false
  ;;

  module T : sig
    val check_a : 'a Typerep.t -> 'a list -> bool
    val check_s : 'a Typerep.t -> 'a list -> bool
    val check_n : 'a Typerep.t -> 'a list -> bool
  end = struct
    let check_a typerep list  =
      check Always typerep list

    let check_s typerep list =
      check Sometimes typerep list

    let check_n typerep list =
      check Never typerep list
  end
  include T

  TEST =
    let module M = struct
      type t = int with typerep
    end in
    check_a M.typerep_of_t [0; 1]

  TEST =
    let module M = struct
      type t = int32 with typerep
    end in
    check_n M.typerep_of_t [Int32.zero; Int32.one; Int32.minus_one; Int32.of_int 32580]

  TEST =
    let module M = struct
      type t = int64 with typerep
    end in
    check_n M.typerep_of_t [Int64.zero]

  TEST =
    let module M = struct
      type t = nativeint with typerep
    end in
    check_n M.typerep_of_t [Nativeint.zero]

  TEST =
    let module M = struct
      type t = char with typerep
    end in
    check_a M.typerep_of_t ['a']

  TEST =
    let module M = struct
      type t = float with typerep
    end in
    check_n M.typerep_of_t
      [1.1; 0.0; -3.3; nan; infinity; neg_infinity; max_float; min_float]

  TEST =
    let module M = struct
      type t = string with typerep
    end in
    check_n M.typerep_of_t [""; "Hello world!"]

  TEST =
    let module M = struct
      type t = bool with typerep
    end in
    check_a M.typerep_of_t [true; false]

  TEST =
    let module M = struct
      type t = unit with typerep
    end in
    check_a M.typerep_of_t [()]

  TEST =
    let module M = struct
      (* The generated typrep in the case of parametric types have this signature:
         val M.typerep_of_t : 'a Typerep.t -> 'a t Typerep.t *)
      type 'a t = 'a option with typerep
    end in
    check_s (M.typerep_of_t imm_t) [None; Some ()]
    && check_s (M.typerep_of_t box_t) [None; Some (ref 1)]

  TEST =
    let module M = struct
      type 'a t = 'a list with typerep
    end in
    check_s (M.typerep_of_t imm_t) [ []; [()] ]
    && check_s (M.typerep_of_t box_t) [ []; [ref 1]]

  let static_empty_array = [||]

  TEST =
    let module M = struct
      type 'a t = 'a array with typerep
    end in
    check_n (M.typerep_of_t imm_t) [ static_empty_array ; [| |]; [| (); () |]]

  TEST =
    let module M = struct
      type 'a t = 'a ref with typerep
    end in
    check_n (M.typerep_of_t imm_t) [ref ()]

  TEST =
    let module M = struct
      type 'a t = 'a lazy_t with typerep
    end in
    check_s (M.typerep_of_t typerep_of_int) [lazy 1; lazy (1+2)]
    && check_n (M.typerep_of_t typerep_of_float) [lazy 0.0; lazy (1.0 +. -3.3)]

  TEST =
    let module M = struct
      type t = {foo:unit} with typerep
    end in
    check_n (M.typerep_of_t) [{M.foo = ()}]

  TEST =
    let module M = struct
      type ('a, 'b) t = ('a * 'b) with typerep
    end in
    check_n (M.typerep_of_t imm_t imm_t) [ (), () ]

  TEST =
    let module M = struct
      type ('a, 'b, 'c) t = ('a * 'b * 'c) with typerep
    end in
    check_n (M.typerep_of_t imm_t imm_t imm_t) [ (), (), () ]

  TEST =
    let module M = struct
      type ('a, 'b, 'c, 'd) t = ('a * 'b * 'c * 'd) with typerep
    end in
    check_n (M.typerep_of_t imm_t imm_t imm_t imm_t) [ (), (), (), () ]

  TEST =
    let module M = struct
      type ('a, 'b, 'c, 'd, 'e) t = ('a * 'b * 'c * 'd * 'e) with typerep
    end in
    check_n (M.typerep_of_t imm_t imm_t imm_t imm_t imm_t) [ (), (), (), (), () ]

  TEST =
    let module M = struct
      type t =
      | Foo
      | Bar of unit with typerep
    end in
    check_s M.typerep_of_t [M.Foo; M.Bar ()]

  TEST =
    let module M = struct
      type t =
      | Foo
      | Bar
      | Baz with typerep
    end in
    check_a M.typerep_of_t [M.Foo; M.Bar; M.Baz]

  TEST =
    let module M = struct
      type t =
      | Foo of unit
      | Bar of unit
      | Baz of unit with typerep
    end in
    check_n M.typerep_of_t [M.Foo (); M.Bar (); M.Baz ()]

  TEST =
    let module M = struct
      type t = Foo of unit with typerep
    end in
    check_n M.typerep_of_t [M.Foo ()]

  TEST =
    let module M = struct
      type t =
      [ `Foo
      | `Bar of unit ] with typerep
    end in
    check_s M.typerep_of_t [`Foo; `Bar ()]

  TEST =
    let module M = struct
      type t =
      [ `Foo
      | `Bar
      | `Baz ] with typerep
    end in
    check_a M.typerep_of_t [`Foo; `Bar; `Baz]

  TEST =
    let module M = struct
      type t =
      [ `Foo of unit
      | `Bar of unit
      | `Baz of unit ] with typerep
    end in
    check_n M.typerep_of_t [`Foo (); `Bar (); `Baz ()]

  TEST =
    let module M = struct
      type t = [ `Foo of unit ] with typerep
    end in
    check_n M.typerep_of_t [`Foo ()]

  TEST =
    let module M = struct
      type 'a t = Nil | Cons of 'a * 'a t with typerep
    end in
    check_s (M.typerep_of_t typerep_of_int) [M.Nil; M.Cons (1, M.Cons (2, M.Nil))]

  TEST =
    let module M = struct
      type 'a t = A | B of 'a t lazy_t | C of 'a * 'a t with typerep
    end in
    check_s (M.typerep_of_t typerep_of_int) [M.A; M.B (lazy M.A); M.C (1,M.A)]

  TEST =
    let module M = struct
      type 'a t = A | B of ('a t * 'a t) | C of 'a with typerep
    end in
    check_s (M.typerep_of_t typerep_of_int) [M.A; M.B (M.A,M.C 1)]

  (* Test the [For_all_parameters] generic witness functors. *)
  TEST =
    let module M = struct
      type 'a t = A | B | C with typerep
    end in
    let module X = Type_immediacy.Always.For_all_parameters_S1(M) in
    true

  TEST =
    let module M = struct
      type 'a t = 'a option lazy_t with typerep
    end in
    let module X = Type_immediacy.Sometimes.For_all_parameters_S1(M) in
    true

  TEST =
    let module M = struct
      type 'a t = 'a array lazy_t with typerep
    end in
    let module X = Type_immediacy.Never.For_all_parameters_S1(M) in
    true

  TEST =
    let module M = struct
      type 'a t = 'a lazy_t with typerep
    end in
    try
      let module X = Type_immediacy.Sometimes.For_all_parameters_S1(M) in
      false
    with _ ->
      try
        let module X = Type_immediacy.Never.For_all_parameters_S1(M) in
        false
      with _ ->
        try
          let module X = Type_immediacy.Always.For_all_parameters_S1(M) in
          false
        with _ -> true
end
