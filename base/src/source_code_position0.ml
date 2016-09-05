open Sexplib.Std
open Hash.Builtin

type t = Lexing.position =
  { pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }
[@@deriving compare, hash, sexp]

let sexp_of_t = Ppx_assert_lib.Runtime.sexp_of_loc
let to_string = Ppx_assert_lib.Runtime.string_of_loc
