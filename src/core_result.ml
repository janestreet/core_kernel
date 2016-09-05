open! Import

module Stable = struct
  module V1 = struct
    type ('a, 'b) t = ('a, 'b) Result.t =
      | Ok of 'a
      | Error of 'b
    [@@deriving bin_io, compare, hash, sexp]

    let map x ~f1 ~f2 =
      match x with
      | Error err -> Error (f2 err)
      | Ok x      -> Ok    (f1 x)
    ;;
  end

  module V1_stable_unit_test = struct
    open Sexplib.Std
    open Bin_prot.Std
    open Hash.Builtin

    type t = (string, int) V1.t
    [@@deriving bin_io, compare, hash, sexp]

    let equal = (=)

    let tests =
      [ V1.Ok "foo", "(Ok foo)",  "\000\003foo"
      ; V1.Error 7,  "(Error 7)", "\001\007"
      ]
  end
end

include Stable.V1

include (Result : module type of Result with type ('a, 'b) t := ('a, 'b) t)
