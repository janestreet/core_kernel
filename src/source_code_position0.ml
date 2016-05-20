open Bin_prot.Std
open Sexplib.Std

module Sexp = Sexplib.Sexp

module Stable = struct
  module V1 = struct
    (* named T' rather than T so as not to collide with Source_code_position.T *)
    module T' = struct
      type t = Lexing.position =
        { pos_fname : string;
          pos_lnum : int;
          pos_bol : int;
          pos_cnum : int;
        }
      [@@deriving bin_io, compare, sexp]
    end
    include T'
    include Comparator.Stable.V1.Make (T')
  end
end

include Stable.V1

let sexp_of_t = Ppx_assert_lib.Runtime.sexp_of_loc
let to_string = Ppx_assert_lib.Runtime.string_of_loc
