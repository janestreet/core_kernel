open Core.Std
open Core_bench.Std

let mem_test =
  let table =
    List.init 100_000 ~f:(fun i -> (i, i))
    |> Int.Table.of_alist_exn
  in
  Bench.Test.create ~name:"Hashtbl.mem" (fun () ->
    ignore (Hashtbl.mem table (Random.int 1_000_000)))

let find_exn_test =
  let table =
    List.init 100_000 ~f:(fun i -> (i, i))
    |> Int.Table.of_alist_exn
  in
  Bench.Test.create ~name:"Hashtbl.find_exn" (fun () ->
    ignore (Hashtbl.find_exn table (Random.int 100_000)))

let tests =
  [ mem_test
  ; find_exn_test
  ]

let () = Command.run (Bench.make_command tests)
