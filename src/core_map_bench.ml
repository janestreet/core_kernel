open Std

open Core_map_intf

BENCH_MODULE "Map.to_sequence" = struct
  let gen_test ~size ~from ~to_ =
    let map =
      List.init size ~f:(fun i -> string_of_int i, i) |> String.Map.of_alist_exn
    in
    fun () ->
      let seq =
        Map.to_sequence map
          ~keys_greater_or_equal_to:from
          ~keys_less_or_equal_to:to_
      in
      Sequence.iter seq ~f:ignore

  BENCH_FUN "small-less" = gen_test ~size:      100 ~from:"45"     ~to_:"55"
  BENCH_FUN "small-more" = gen_test ~size:      100 ~from: "5"     ~to_:"95"
  BENCH_FUN "big-less"   = gen_test ~size:1_000_000 ~from:"200000" ~to_:"200050"
  BENCH_FUN "big-more"   = gen_test ~size:1_000_000 ~from:"20"     ~to_:"900000"
end
