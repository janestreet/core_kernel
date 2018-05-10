open! Core_kernel
open! Import
open! Memo

let%test_module "lru" =
  (module struct
    let count = ref 0  (* number of times f underlying function is run *)
    let f = general ~cache_size_bound:3 (fun i -> incr count; i)

    let%test _ = f 0 = 0
    let%test _ = !count = 1

    let%test _ = f 1 = 1
    let%test _ = !count = 2

    let%test _ = f 0 = 0
    let%test _ = !count = 2

    let%test _ = f 3 = 3                       (* cache full *)
    let%test _ = !count = 3

    let%test _ = f 4 = 4                       (* evict 1 *)
    let%test _ = !count = 4

    let%test _ = f 0 = 0
    let%test _ = !count = 4

    let%test _ = f 1 = 1                       (* recompute 1 *)
    let%test _ = !count = 5
  end)
