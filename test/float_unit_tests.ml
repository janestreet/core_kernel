open Core_kernel.Std

let%test_unit "round_nearest_half_to_even quickcheck" =
  Quickcheck.test
    ~trials:200
    (Int.gen_between
       ~lower_bound:(Maybe_bound.Incl (-100_000_000))
       ~upper_bound:(Maybe_bound.Incl   100_000_000))
    ~f:(fun i ->
      let x = float i /. 10. in
      let y = Float.round_nearest_half_to_even x in
      let f = Float.round_nearest x in
      let is_tie = Int.(%) i 10 = 5 in
      assert (
        is_tie && Float.mod_float y 2. = 0. && Float.abs (y -. x) = 0.5
        || (not is_tie) && y = f
      );
      let x'  = Float.one_ulp `Up   x in
      let x'' = Float.one_ulp `Down x in
      assert (Float.round_nearest_half_to_even x'  = Float.round_nearest x');
      assert (Float.round_nearest_half_to_even x'' = Float.round_nearest x''))
;;

