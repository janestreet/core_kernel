open Bin_prot.Std
open Sexplib.Std

module Sexp = Sexplib.Sexp

module Stable = struct
  module V1 = struct
    type t = Lexing.position =
      { pos_fname : string;
        pos_lnum : int;
        pos_bol : int;
        pos_cnum : int;
      }
    with bin_io, compare, sexp
  end
end

include Stable.V1

type t_hum = t

let sexp_of_t_hum = Pa_test_lib.Runtime.sexp_of_loc
let to_string     = Pa_test_lib.Runtime.string_of_loc
