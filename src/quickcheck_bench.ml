open Std

module Q = Quickcheck
module G = Q.Generator

let lst kind ~len ~elt ~num =
  let elt_gen = G.of_list (List.range 0 elt) in
  let length = `Exactly len in
  let gen =
    match kind with
    | `norm -> List.gen' elt_gen ~length
    | `uniq -> List.gen' elt_gen ~length ~unique:true
    | `sort -> List.gen' elt_gen ~length ~sorted:`Arbitrarily
  in
  Quickcheck.iter gen ~trials:num ~f:ignore

let%bench "list norm len:  2 elt:   400 num:  1" = lst `norm ~len:  2 ~elt:   400 ~num:  1
let%bench "list uniq len:  2 elt:   400 num:  1" = lst `uniq ~len:  2 ~elt:   400 ~num:  1
let%bench "list sort len:  2 elt:   400 num:  1" = lst `sort ~len:  2 ~elt:   400 ~num:  1
let%bench "list norm len:200 elt:   400 num:  1" = lst `norm ~len:200 ~elt:   400 ~num:  1
let%bench "list uniq len:200 elt:   400 num:  1" = lst `uniq ~len:200 ~elt:   400 ~num:  1
let%bench "list sort len:200 elt:   400 num:  1" = lst `sort ~len:200 ~elt:   400 ~num:  1
let%bench "list norm len:  2 elt:40_000 num:  1" = lst `norm ~len:  2 ~elt:40_000 ~num:  1
let%bench "list uniq len:  2 elt:40_000 num:  1" = lst `uniq ~len:  2 ~elt:40_000 ~num:  1
let%bench "list sort len:  2 elt:40_000 num:  1" = lst `sort ~len:  2 ~elt:40_000 ~num:  1
let%bench "list norm len:200 elt:40_000 num:  1" = lst `norm ~len:200 ~elt:40_000 ~num:  1
let%bench "list uniq len:200 elt:40_000 num:  1" = lst `uniq ~len:200 ~elt:40_000 ~num:  1
let%bench "list sort len:200 elt:40_000 num:  1" = lst `sort ~len:200 ~elt:40_000 ~num:  1
let%bench "list norm len:  2 elt:   400 num:100" = lst `norm ~len:  2 ~elt:   400 ~num:100
let%bench "list uniq len:  2 elt:   400 num:100" = lst `uniq ~len:  2 ~elt:   400 ~num:100
let%bench "list sort len:  2 elt:   400 num:100" = lst `sort ~len:  2 ~elt:   400 ~num:100
let%bench "list norm len:200 elt:   400 num:100" = lst `norm ~len:200 ~elt:   400 ~num:100
let%bench "list uniq len:200 elt:   400 num:100" = lst `uniq ~len:200 ~elt:   400 ~num:100
let%bench "list sort len:200 elt:   400 num:100" = lst `sort ~len:200 ~elt:   400 ~num:100
let%bench "list norm len:  2 elt:40_000 num:100" = lst `norm ~len:  2 ~elt:40_000 ~num:100
let%bench "list uniq len:  2 elt:40_000 num:100" = lst `uniq ~len:  2 ~elt:40_000 ~num:100
let%bench "list sort len:  2 elt:40_000 num:100" = lst `sort ~len:  2 ~elt:40_000 ~num:100
let%bench "list norm len:200 elt:40_000 num:100" = lst `norm ~len:200 ~elt:40_000 ~num:100
let%bench "list uniq len:200 elt:40_000 num:100" = lst `uniq ~len:200 ~elt:40_000 ~num:100
let%bench "list sort len:200 elt:40_000 num:100" = lst `sort ~len:200 ~elt:40_000 ~num:100
