open Fieldslib
open Sexplib.Conv

include Invariant_intf

let failwiths = Error.failwiths

let invariant here t sexp_of_t f : unit =
  try
    f ()
  with exn ->
    failwiths "invariant failed" (here, exn, t)
      [%sexp_of: Source_code_position0.t * exn * t]
;;

let check_field t f field =
  try
    f (Field.get field t)
  with exn ->
    failwiths "problem with field" (Field.name field, exn) [%sexp_of: string * exn]
;;
