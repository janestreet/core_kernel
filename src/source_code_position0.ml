open! Import

open Bin_prot.Std
open Sexplib.Std
open Hash.Builtin

module Sexp = Sexplib.Sexp
module Comparator = Core_comparator

module Stable = struct
  module V1 = struct
    (* named T' rather than T so as not to collide with Source_code_position.T *)
    module T' = struct
      type t = Base0.Source_code_position0.t =
        { pos_fname : string;
          pos_lnum : int;
          pos_bol : int;
          pos_cnum : int;
        } constraint Base0.Source_code_position0.t = Lexing.position
      [@@deriving bin_io, compare, hash, sexp]
    end
    include T'
    include Core_comparator.Stable.V1.Make (T')
  end
end

include Stable.V1

let sexp_of_t = Ppx_assert_lib.Runtime.sexp_of_loc
let to_string = Ppx_assert_lib.Runtime.string_of_loc
