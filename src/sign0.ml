(* This is broken off to avoid circular dependency between Sign and Comparable. *)
open Typerep_lib.Std

module Stable = struct
  module V1 = struct
    type t = Neg | Zero | Pos [@@deriving sexp, bin_io, compare, typerep, enumerate]
  end
end

module T = struct
  include Stable.V1
  include Sexpable.To_stringable(Stable.V1)

  let to_int = function
    | Neg  -> -1
    | Zero ->  0
    | Pos  ->  1

  let hash = to_int

  let module_name = "Core.Std.Sign"
end

include T

let of_int n =
  if n < 0
  then Neg
  else if n = 0
  then Zero
  else Pos

