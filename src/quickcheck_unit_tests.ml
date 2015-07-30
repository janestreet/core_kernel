INCLUDE "config.mlh"

open Std_internal

module Test (S : sig val default_seed : Quickcheck_intf.seed end) = struct

  let int_middle_bits =
IFDEF ARCH_SIXTYFOUR THEN
    Int64.to_int_exn 0x0000_ffff_ffff_0000L
ELSE
    0x00ff_ff00
ENDIF

  module JC = Quickcheck.Configure (struct
      include Quickcheck
      include S
      let default_trial_count = default_trial_count * 10
      let default_trial_count_for_test_no_duplicates =
        `Scale_of_default_trial_count 1.
    end)

  open JC

  module G = Generator
  module O = Observer

  TEST_MODULE "examples" = struct
    let example = "some silly string that is unlikely to be generated randomly"

    let example_occurs ~examples =
      with_return (fun r ->
        test G.string ~examples ~f:(fun str ->
          if String.equal str example then r.return true);
        false)

    TEST_UNIT = <:test_result< bool >> (example_occurs ~examples:[])        ~expect:false
    TEST_UNIT = <:test_result< bool >> (example_occurs ~examples:[example]) ~expect:true
  end

  TEST_MODULE "failure" = struct
    let cannot_generate gen =
      assert (Exn.does_raise (fun () ->
        test_can_generate gen ~f:(fun _ -> true)))

    TEST_UNIT = cannot_generate G.failure
    TEST_UNIT = cannot_generate G.(filter unit ~f:(fun _ -> false))
    TEST_UNIT = cannot_generate G.(filter int  ~f:(fun _ -> false))

    TEST_UNIT =
      test_can_generate
        G.(list ~length:(`Exactly 100) (filter int ~f:(fun x -> (x mod 2) = 0)))
        ~f:(fun _ -> true)
  end

  TEST_MODULE "unit" = struct
    let sexp_of = Unit.sexp_of_t
    let by = `Compare Unit.compare
    let gen = G.unit
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun () -> true)
  end

  TEST_MODULE "bool" = struct
    let sexp_of = Bool.sexp_of_t
    let by = `Compare Bool.compare
    let gen = G.bool
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> x = true)
    TEST_UNIT = can_generate (fun x -> x = false)
  end

  TEST_MODULE "int" = struct
    let sexp_of = Int.Hex.sexp_of_t
    let by = `Compare Int.compare
    let gen = G.int
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> x = Int.max_value)
    TEST_UNIT = can_generate (fun x -> x = Int.min_value)
    TEST_UNIT = can_generate (fun x -> x = 0)
    TEST_UNIT = can_generate (fun x -> x = 1)
    TEST_UNIT = can_generate (fun x -> x = -1)
    TEST_UNIT = can_generate (fun x -> x > 256)
    TEST_UNIT = can_generate (fun x -> x < -256)
    let middle_bits x = x land int_middle_bits
    TEST_UNIT = can_generate (fun x ->
      middle_bits x <> middle_bits   0 &&
      middle_bits x <> middle_bits (-1))
  end

  TEST_MODULE "float" = struct
    let bits_compare x y = Int64.compare (Int64.bits_of_float x) (Int64.bits_of_float y)
    let bits_equal x y = (bits_compare x y) = 0
    let sexp_of = Float.sexp_of_t
    let by = `Compare bits_compare
    let gen = G.float
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let has_class x c =
      match Float.classify x, (c : Float.Class.t) with
      | Infinite,  Infinite
      | Nan,       Nan
      | Normal,    Normal
      | Subnormal, Subnormal
      | Zero,      Zero
        -> true
      | _ -> false

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> has_class x Infinite)
    TEST_UNIT = can_generate (fun x -> has_class x Nan)
    TEST_UNIT = can_generate (fun x -> has_class x Normal)
    TEST_UNIT = can_generate (fun x -> has_class x Subnormal)
    TEST_UNIT = can_generate (fun x -> has_class x Zero)
    TEST_UNIT = can_generate (fun x -> Float.(<) x 0.)
    TEST_UNIT = can_generate (fun x -> Float.(>) x 0.)
    TEST_UNIT = can_generate (fun x -> Float.(=) x 0. && bits_equal x 0.)
    TEST_UNIT = can_generate (fun x -> Float.(=) x 0. && not (bits_equal x 0.))
    TEST_UNIT = can_generate (fun x -> Float.(=) x Float.neg_infinity)

    TEST_UNIT =
      test_can_generate G.float_without_nan ~f:(fun f ->
        Float.equal f Float.infinity)
    TEST_UNIT =
      test_can_generate G.float_without_nan ~f:(fun f ->
        Float.equal f Float.neg_infinity)
    TEST_UNIT =
      test_can_generate G.float_without_nan ~f:(fun f ->
        Float.is_finite f)
    TEST_UNIT =
      test G.float_without_nan ~f:(fun f ->
        assert (not (Float.is_nan f)))

    TEST_UNIT =
      test_can_generate G.float_finite (* ~trials:1_000 *) ~f:(fun f ->
        Float.equal f Float.max_finite_value)
    TEST_UNIT =
      test_can_generate G.float_finite (* ~trials:1_000 *) ~f:(fun f ->
        Float.equal f Float.(- max_finite_value))
    TEST_UNIT =
      test G.float_finite ~f:(fun f ->
        assert (not (Float.is_nan f)))
    TEST_UNIT =
      test G.float_finite ~f:(fun f ->
        assert (not (Float.is_inf f)))

  end

  TEST_MODULE "string" = struct
    let sexp_of = String.sexp_of_t
    let by = `Compare String.compare
    let gen = G.string
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun x -> String.length x = 0)
    TEST_UNIT = can_generate (fun x -> String.length x = 1)
    TEST_UNIT = can_generate (fun x -> String.length x = 2)
    TEST_UNIT = can_generate (fun x -> String.length x > 2)
    TEST_UNIT = can_generate (fun x -> String.uppercase x <> x)
    TEST_UNIT = can_generate (fun x -> String.lowercase x <> x)
    TEST_UNIT = can_generate (fun x ->
      match Int.of_string x with
      | _ -> true
      | exception _ -> false)
  end

  TEST_MODULE "char" = struct
    let sexp_of = Char.sexp_of_t
    let by = `Compare Char.compare
    let gen = G.char
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate Char.is_digit
    TEST_UNIT = can_generate Char.is_lowercase
    TEST_UNIT = can_generate Char.is_uppercase
    TEST_UNIT = can_generate Char.is_print
    TEST_UNIT = can_generate Char.is_whitespace
    TEST_UNIT = can_generate (fun c ->
      not (Char.is_digit c)
      && not (Char.is_lowercase c)
      && not (Char.is_uppercase c)
      && not (Char.is_print c)
      && not (Char.is_whitespace c))
  end

  TEST_MODULE "tuple2" = struct
    let sexp_of = <:sexp_of< Int.Hex.t * Int.Hex.t >>
    let by = `Compare <:compare< int * int >>
    let gen = G.(tuple2 int int)
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (fun (x,y) -> x = y)
    TEST_UNIT = can_generate (fun (x,y) -> x < y)
    TEST_UNIT = can_generate (fun (x,y) -> x > y)
  end

  TEST_MODULE "option" = struct
    let sexp_of = <:sexp_of< Int.Hex.t option >>
    let by = `Compare <:compare< int option >>
    let gen = G.(option int)
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate Option.is_none
    TEST_UNIT = can_generate Option.is_some
  end

  TEST_MODULE "function" = struct
    let sexp_of = G.fn_sexp
    let by = `Compare <:compare< Sexp.t >>
    let gen = G.(fn_with_sexp O.int int ~sexp_of_rng:<:sexp_of< int >>)
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates G.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    TEST_UNIT = can_generate (fun (f, _) -> f 0 = f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f 0 < f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f 0 > f (-1))

    TEST_UNIT = can_generate (fun (f, _) -> f 1 = f 0)
    TEST_UNIT = can_generate (fun (f, _) -> f 1 < f 0)
    TEST_UNIT = can_generate (fun (f, _) -> f 1 > f 0)

    TEST_UNIT = can_generate (fun (f, _) -> f 2 = f 1)
    TEST_UNIT = can_generate (fun (f, _) -> f 2 < f 1)
    TEST_UNIT = can_generate (fun (f, _) -> f 2 > f 1)

    TEST_UNIT = can_generate (fun (f, _) -> f (-1) <> f 0 && f 0 <> f 1 && f 1 <> f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f (-1) =  f 0 && f 0 =  f 1 && f 1 =  f (-1))
    TEST_UNIT = can_generate (fun (f, _) -> f int_middle_bits <> f 0)

  end

  TEST_MODULE "higher-order function" = struct
    let sexp_of = G.fn_sexp
    let by = `Compare <:compare< Sexp.t >>
    let gen =
      G.fn_with_sexp
        (O.fn G.int O.int ~sexp_of_dom:<:sexp_of< int >>)
        G.int ~sexp_of_rng:<:sexp_of< int >>
    let can_generate ?trials f = test_can_generate gen ~sexp_of ~f ?trials

    TEST_UNIT = test_no_duplicates G.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    TEST_UNIT = can_generate (fun (f, _) -> f Int.succ = f Int.pred)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.succ > f Int.pred)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.succ < f Int.pred)

    TEST_UNIT = can_generate (fun (f, _) -> f Int.neg <> f Int.abs)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.neg <> f Fn.id)
    TEST_UNIT = can_generate (fun (f, _) -> f Int.abs <> f Fn.id)

    TEST_UNIT = can_generate (fun (f, _) ->
      let x = f Fn.id   in
      let y = f Int.neg in
      let z = f Int.abs in
      x <> y && y <> z && z <> x)

  end

  TEST_MODULE "list" = struct
    let sexp_of = <:sexp_of< Int.Hex.t list >>
    let by = `Compare <:compare< int list >>
    let gen = G.(list int)
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let is_sorted_with_dup list ~compare =
      List.is_sorted list ~compare
      && not (List.is_sorted_strictly list ~compare)

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate List.is_empty
    TEST_UNIT = can_generate (function [_] -> true | _ -> false)
    TEST_UNIT = can_generate (function [x;y] -> x=y | _ -> false)
    TEST_UNIT = can_generate (function [x;y] -> x<y | _ -> false)
    TEST_UNIT = can_generate (function [x;y] -> x>y | _ -> false)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && List.is_sorted_strictly list ~compare:Int.compare)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && is_sorted_with_dup list ~compare:Int.compare)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && List.is_sorted_strictly (List.rev list) ~compare:Int.compare)
    TEST_UNIT = can_generate (fun list ->
      List.length list > 2
      && is_sorted_with_dup (List.rev list) ~compare:Int.compare)
  end

  TEST_MODULE "sexp" = struct
    let sexp_of = <:sexp_of< Sexp.t >>
    let by = `Compare <:compare< Sexp.t >>
    let gen =
      G.recursive (fun sexp_gen ->
        G.map ~f:(function `A s -> Sexp.Atom s | `B l -> Sexp.List l)
          (G.variant2 G.string (G.list sexp_gen)))
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates gen ~sexp_of ~by
    TEST_UNIT = can_generate (function Sexp.Atom _       -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List _       -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.Atom ""      -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List []      -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List [_]     -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List [_;_]   -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.List [_;_;_] -> true | _ -> false)
    TEST_UNIT = can_generate (function Sexp.Atom _ -> false | Sexp.List list ->
      let is_atom = function Sexp.Atom _ -> true | Sexp.List _ -> false in
      List.length list >= 2 && List.for_all list ~f:is_atom)
  end

  TEST_MODULE "function on recursive data" = struct
    let sexp_of = G.fn_sexp
    let by = `Compare <:compare< Sexp.t >>
    let gen = G.(fn_with_sexp O.(list bool) G.char ~sexp_of_rng:<:sexp_of< char >>)
    let can_generate f = test_can_generate gen ~sexp_of ~f

    TEST_UNIT = test_no_duplicates G.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    TEST_UNIT = can_generate (fun (f, _) -> f [] = f [true])
    TEST_UNIT = can_generate (fun (f, _) -> f [] = f [false])
    TEST_UNIT = can_generate (fun (f, _) -> f [true] = f [false])

    TEST_UNIT = can_generate (fun (f, _) -> f [] <> f [true])
    TEST_UNIT = can_generate (fun (f, _) -> f [] <> f [false])
    TEST_UNIT = can_generate (fun (f, _) -> f [true] <> f [false])

    TEST_UNIT = can_generate (fun (f, _) -> f [true;true] <> f [true;false])
    TEST_UNIT = can_generate (fun (f, _) -> f [true;true;true] <> f [true;true;false])

  end

end

TEST_MODULE = Test (Quickcheck)
TEST_MODULE = Test (struct let default_seed = `Deterministic "foo" end)
TEST_MODULE = Test (struct let default_seed = `Deterministic "bar" end)
TEST_MODULE = Test (struct let default_seed = `Deterministic "baz" end)
TEST_MODULE = Test (struct let default_seed = `Deterministic "quux" end)
TEST_MODULE = Test (struct let default_seed = `Deterministic "zanzibar" end)
TEST_MODULE = Test (struct let default_seed = `Deterministic "lorem ipsum" end)
