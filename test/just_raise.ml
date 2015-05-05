open! Core_kernel.Std

exception E of int with sexp

let () = raise (E 42)
