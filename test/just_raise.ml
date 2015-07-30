open! Core_kernel.Std

exception E of int [@@deriving sexp]

let () = raise (E 42)
