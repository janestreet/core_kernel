open Std

module Q = Quickcheck
module G = Q.Generator

let lst ~len ~elt ~num =
  let elt_gen = G.of_list (List.range 0 elt) in
  let length = `Exactly len in
  let gen = List.gen' elt_gen ~length in
  Quickcheck.iter gen ~trials:num ~f:ignore

let%bench "list len:  2 elt:   400 num:  1" = lst ~len:  2 ~elt:   400 ~num:  1
let%bench "list len:200 elt:   400 num:  1" = lst ~len:200 ~elt:   400 ~num:  1
let%bench "list len:  2 elt:40_000 num:  1" = lst ~len:  2 ~elt:40_000 ~num:  1
let%bench "list len:200 elt:40_000 num:  1" = lst ~len:200 ~elt:40_000 ~num:  1
let%bench "list len:  2 elt:   400 num:100" = lst ~len:  2 ~elt:   400 ~num:100
let%bench "list len:200 elt:   400 num:100" = lst ~len:200 ~elt:   400 ~num:100
let%bench "list len:  2 elt:40_000 num:100" = lst ~len:  2 ~elt:40_000 ~num:100
let%bench "list len:200 elt:40_000 num:100" = lst ~len:200 ~elt:40_000 ~num:100
