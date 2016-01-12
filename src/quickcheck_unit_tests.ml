#import "config.mlh"

open Std_internal

module Test (S : sig val default_seed : Quickcheck_intf.seed end) : sig end = struct

  let int_middle_bits =
#if JSC_ARCH_SIXTYFOUR
    Int64.to_int_exn 0x0000_ffff_ffff_0000L
#else
    0x00ff_ff00
#endif

  module JC = Quickcheck.Configure (struct
      include Quickcheck
      include S
      let default_trial_count = default_trial_count * 10
      let default_trial_count_for_test_no_duplicates =
        `Scale_of_default_trial_count 1.
    end)

  open JC

  module G = Quickcheck.Generator
  module O = Quickcheck.Observer

  let%test_module "examples" = (module struct
    let example = "some silly string that is unlikely to be generated randomly"

    let example_occurs ~examples =
      with_return (fun r ->
        test String.gen ~examples ~f:(fun str ->
          if String.equal str example then r.return true);
        false)

    let%test_unit _ = [%test_result: bool] (example_occurs ~examples:[])        ~expect:false
    let%test_unit _ = [%test_result: bool] (example_occurs ~examples:[example]) ~expect:true
  end)

  let%test_module "failure" = (module struct
    let cannot_generate gen =
      assert (Exn.does_raise (fun () ->
        test_can_generate gen ~f:(fun _ -> true)))

    let%test_unit _ =
      cannot_generate G.failure
    let%test_unit _ =
      cannot_generate G.(filter Unit.gen ~f:(fun _ -> false))
    let%test_unit _ =
      cannot_generate G.(filter Int.gen  ~f:(fun _ -> false))

    let%test_unit _ =
      test_can_generate
        (List.gen'
           ~length:(`Exactly 100)
           G.(filter Int.gen ~f:(fun x -> (x mod 2) = 0)))
        ~f:(fun _ -> true)
  end)

  let%test_module "unit" = (module struct
    let sexp_of = Unit.sexp_of_t
    let by = `Compare Unit.compare
    let gen = Unit.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (fun () -> true)
  end)

  let%test_module "bool" = (module struct
    let sexp_of = Bool.sexp_of_t
    let by = `Compare Bool.compare
    let gen = Bool.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (fun x -> x = true)
    let%test_unit _ = can_generate (fun x -> x = false)
  end)

  let%test_module "int" = (module struct
    let sexp_of = Int.Hex.sexp_of_t
    let by = `Compare Int.compare
    let gen = Int.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (fun x -> x = Int.max_value)
    let%test_unit _ = can_generate (fun x -> x = Int.min_value)
    let%test_unit _ = can_generate (fun x -> x = 0)
    let%test_unit _ = can_generate (fun x -> x = 1)
    let%test_unit _ = can_generate (fun x -> x = -1)
    let%test_unit _ = can_generate (fun x -> x > 256)
    let%test_unit _ = can_generate (fun x -> x < -256)
    let middle_bits x = x land int_middle_bits
    let%test_unit _ = can_generate (fun x ->
      middle_bits x <> middle_bits   0 &&
      middle_bits x <> middle_bits (-1))
  end)

  let%test_module "float" = (module struct
    let bits_compare x y = Int64.compare (Int64.bits_of_float x) (Int64.bits_of_float y)
    let bits_equal x y = (bits_compare x y) = 0
    let sexp_of = Float.sexp_of_t
    let by = `Compare bits_compare
    let gen = Float.gen
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

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (fun x -> has_class x Infinite)
    let%test_unit _ = can_generate (fun x -> has_class x Nan)
    let%test_unit _ = can_generate (fun x -> has_class x Normal)
    let%test_unit _ = can_generate (fun x -> has_class x Subnormal)
    let%test_unit _ = can_generate (fun x -> has_class x Zero)
    let%test_unit _ = can_generate (fun x -> Float.(<) x 0.)
    let%test_unit _ = can_generate (fun x -> Float.(>) x 0.)
    let%test_unit _ = can_generate (fun x -> Float.(=) x 0. && bits_equal x 0.)
    let%test_unit _ = can_generate (fun x -> Float.(=) x 0. && not (bits_equal x 0.))
    let%test_unit _ = can_generate (fun x -> Float.(=) x Float.neg_infinity)

    let%test_unit _ =
      test_can_generate Float.gen_without_nan ~f:(fun f ->
        Float.equal f Float.infinity)
    let%test_unit _ =
      test_can_generate Float.gen_without_nan ~f:(fun f ->
        Float.equal f Float.neg_infinity)
    let%test_unit _ =
      test_can_generate Float.gen_without_nan ~f:(fun f ->
        Float.is_finite f)
    let%test_unit _ =
      test Float.gen_without_nan ~f:(fun f ->
        assert (not (Float.is_nan f)))

    let%test_unit _ =
      test_can_generate Float.gen_finite ~trials:10_000 ~f:(fun f ->
        Float.equal f Float.max_finite_value)
    let%test_unit _ =
      test_can_generate Float.gen_finite ~trials:10_000 ~f:(fun f ->
        Float.equal f Float.(- max_finite_value))
    let%test_unit _ =
      test Float.gen_finite ~f:(fun f ->
        assert (not (Float.is_nan f)))
    let%test_unit _ =
      test Float.gen_finite ~f:(fun f ->
        assert (not (Float.is_inf f)))

  end)

  let%test_module "string" = (module struct
    let sexp_of = String.sexp_of_t
    let by = `Compare String.compare
    let gen = String.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (fun x -> String.length x = 0)
    let%test_unit _ = can_generate (fun x -> String.length x = 1)
    let%test_unit _ = can_generate (fun x -> String.length x = 2)
    let%test_unit _ = can_generate (fun x -> String.length x > 2)
    let%test_unit _ = can_generate (fun x -> String.uppercase x <> x)
    let%test_unit _ = can_generate (fun x -> String.lowercase x <> x)
    let%test_unit _ = can_generate (fun x ->
      match Int.of_string x with
      | _ -> true
      | exception _ -> false)
  end)

  let%test_module "char" = (module struct
    let sexp_of = Char.sexp_of_t
    let by = `Compare Char.compare
    let gen = Char.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate Char.is_digit
    let%test_unit _ = can_generate Char.is_lowercase
    let%test_unit _ = can_generate Char.is_uppercase
    let%test_unit _ = can_generate Char.is_print
    let%test_unit _ = can_generate Char.is_whitespace
    let%test_unit _ = can_generate (fun c ->
      not (Char.is_digit c)
      && not (Char.is_lowercase c)
      && not (Char.is_uppercase c)
      && not (Char.is_print c)
      && not (Char.is_whitespace c))
  end)

  let%test_module "tuple2" = (module struct
    let sexp_of = [%sexp_of: Int.Hex.t * Int.Hex.t]
    let by = `Compare [%compare: int * int]
    let gen = G.tuple2 Int.gen Int.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (fun (x,y) -> x = y)
    let%test_unit _ = can_generate (fun (x,y) -> x < y)
    let%test_unit _ = can_generate (fun (x,y) -> x > y)
  end)

  let%test_module "option" = (module struct
    let sexp_of = [%sexp_of: Int.Hex.t option]
    let by = `Compare [%compare: int option]
    let gen = Option.gen Int.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate Option.is_none
    let%test_unit _ = can_generate Option.is_some
  end)

  let%test_module "function" = (module struct
    let sexp_of = G.fn_sexp
    let by = `Compare [%compare: Sexp.t]
    let gen =
      G.fn_with_sexp Int.obs Int.gen
        ~sexp_of_rng:[%sexp_of: int]
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates G.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    let%test_unit _ = can_generate (fun (f, _) -> f 0 = f (-1))
    let%test_unit _ = can_generate (fun (f, _) -> f 0 < f (-1))
    let%test_unit _ = can_generate (fun (f, _) -> f 0 > f (-1))

    let%test_unit _ = can_generate (fun (f, _) -> f 1 = f 0)
    let%test_unit _ = can_generate (fun (f, _) -> f 1 < f 0)
    let%test_unit _ = can_generate (fun (f, _) -> f 1 > f 0)

    let%test_unit _ = can_generate (fun (f, _) -> f 2 = f 1)
    let%test_unit _ = can_generate (fun (f, _) -> f 2 < f 1)
    let%test_unit _ = can_generate (fun (f, _) -> f 2 > f 1)

    let%test_unit _ = can_generate (fun (f, _) -> f (-1) <> f 0 && f 0 <> f 1 && f 1 <> f (-1))
    let%test_unit _ = can_generate (fun (f, _) -> f (-1) =  f 0 && f 0 =  f 1 && f 1 =  f (-1))
    let%test_unit _ = can_generate (fun (f, _) -> f int_middle_bits <> f 0)

  end)

  let%test_module "higher-order function" = (module struct
    let sexp_of = G.fn_sexp
    let by = `Compare [%compare: Sexp.t]
    let gen =
      G.fn_with_sexp
        (O.fn Int.gen Int.obs
           ~sexp_of_dom:[%sexp_of: int])
        Int.gen
        ~sexp_of_rng:[%sexp_of: int]
    let can_generate ?trials f = test_can_generate gen ~sexp_of ~f ?trials

    let%test_unit _ = test_no_duplicates G.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    let%test_unit _ = can_generate (fun (f, _) -> f Int.succ = f Int.pred)
    let%test_unit _ = can_generate (fun (f, _) -> f Int.succ > f Int.pred)
    let%test_unit _ = can_generate (fun (f, _) -> f Int.succ < f Int.pred)

    let%test_unit _ = can_generate (fun (f, _) -> f Int.neg <> f Int.abs)
    let%test_unit _ = can_generate (fun (f, _) -> f Int.neg <> f Fn.id)
    let%test_unit _ = can_generate (fun (f, _) -> f Int.abs <> f Fn.id)

    let%test_unit _ = can_generate (fun (f, _) ->
      let x = f Fn.id   in
      let y = f Int.neg in
      let z = f Int.abs in
      x <> y && y <> z && z <> x)

  end)

  let%test_module "list" = (module struct
    let sexp_of = [%sexp_of: Int.Hex.t list]
    let by = `Compare [%compare: int list]
    let gen = List.gen Int.gen
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let is_sorted_with_dup list ~compare =
      List.is_sorted list ~compare
      && not (List.is_sorted_strictly list ~compare)

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate List.is_empty
    let%test_unit _ = can_generate (function [_] -> true | _ -> false)
    let%test_unit _ = can_generate (function [x;y] -> x=y | _ -> false)
    let%test_unit _ = can_generate (function [x;y] -> x<y | _ -> false)
    let%test_unit _ = can_generate (function [x;y] -> x>y | _ -> false)
    let%test_unit _ = can_generate (fun list ->
      List.length list > 2
      && List.is_sorted_strictly list ~compare:Int.compare)
    let%test_unit _ = can_generate (fun list ->
      List.length list > 2
      && is_sorted_with_dup list ~compare:Int.compare)
    let%test_unit _ = can_generate (fun list ->
      List.length list > 2
      && List.is_sorted_strictly (List.rev list) ~compare:Int.compare)
    let%test_unit _ = can_generate (fun list ->
      List.length list > 2
      && is_sorted_with_dup (List.rev list) ~compare:Int.compare)
  end)

  let%test_module "sexp" = (module struct
    let sexp_of = [%sexp_of: Sexp.t]
    let by = `Compare [%compare: Sexp.t]
    let gen =
      G.recursive (fun sexp_gen ->
        G.map ~f:(function `A s -> Sexp.Atom s | `B l -> Sexp.List l)
          (G.variant2
             String.gen
             (List.gen sexp_gen)))
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates gen ~sexp_of ~by
    let%test_unit _ = can_generate (function Sexp.Atom _       -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.List _       -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.Atom ""      -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.List []      -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.List [_]     -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.List [_;_]   -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.List [_;_;_] -> true | _ -> false)
    let%test_unit _ = can_generate (function Sexp.Atom _ -> false | Sexp.List list ->
      let is_atom = function Sexp.Atom _ -> true | Sexp.List _ -> false in
      List.length list >= 2 && List.for_all list ~f:is_atom)
  end)

  let%test_module "function on recursive data" = (module struct
    let sexp_of = G.fn_sexp
    let by = `Compare [%compare: Sexp.t]
    let gen =
      G.fn_with_sexp
        (List.obs Bool.obs)
        Char.gen
        ~sexp_of_rng:[%sexp_of: char]
    let can_generate f = test_can_generate gen ~sexp_of ~f

    let%test_unit _ = test_no_duplicates G.(gen >>| sexp_of) ~sexp_of:Fn.id ~by

    let%test_unit _ = can_generate (fun (f, _) -> f [] = f [true])
    let%test_unit _ = can_generate (fun (f, _) -> f [] = f [false])
    let%test_unit _ = can_generate (fun (f, _) -> f [true] = f [false])

    let%test_unit _ = can_generate (fun (f, _) -> f [] <> f [true])
    let%test_unit _ = can_generate (fun (f, _) -> f [] <> f [false])
    let%test_unit _ = can_generate (fun (f, _) -> f [true] <> f [false])

    let%test_unit _ = can_generate (fun (f, _) -> f [true;true] <> f [true;false])
    let%test_unit _ = can_generate (fun (f, _) -> f [true;true;true] <> f [true;true;false])

  end)

  let%test_module "deep recursion" = (module struct

    let test length =
      test ~trials:1 (List.gen' Char.gen ~length:(`Exactly length)) ~f:(fun input ->
        [%test_result: int] (List.length input) ~expect:length)

    let%test_unit "used to cause a stack overflow" = test 100_000

  end)

end

let%test_module _ = (module Test (Quickcheck))
let%test_module _ = (module Test (struct let default_seed = `Deterministic "foo" end))
let%test_module _ = (module Test (struct let default_seed = `Deterministic "bar" end))
let%test_module _ = (module Test (struct let default_seed = `Deterministic "baz" end))
let%test_module _ = (module Test (struct let default_seed = `Deterministic "quux" end))
let%test_module _ = (module Test (struct let default_seed = `Deterministic "zanzibar" end))
let%test_module _ = (module Test (struct let default_seed = `Deterministic "lorem ipsum" end))
