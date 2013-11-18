open Std_internal

(* Disallows whitespace around the edges in of_string and t_of_sexp, but doesn't check
   when reading from bin_io. *)
include Identifiable with type t = private string

module Stable : sig
  module V1 : sig
    type nonrec t = t with sexp, bin_io, compare
  end
end
