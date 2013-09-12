open Bin_prot.Std
open Sexplib.Std

module Sexp = Sexplib.Sexp

type t = Lexing.position =
  { pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }
with bin_io, compare, sexp

type t_hum = t with bin_io, compare

let to_string t =
  String.concat ""
    [ t.pos_fname
    ; ":"; string_of_int t.pos_lnum
    ; ":"; string_of_int (t.pos_cnum - t.pos_bol)
    ]
;;

let sexp_of_t_hum t = Sexp.Atom (to_string t)

let to_sexp_hum = sexp_of_t_hum
