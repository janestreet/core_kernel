open Sexplib
open Hash.Builtin

exception Nan_or_inf [@@deriving sexp]

type t = float [@@deriving compare, hash]

let verify t =
  match Pervasives.classify_float t with
  | FP_normal
  | FP_subnormal
  | FP_zero      -> ()
  | FP_infinite
  | FP_nan       -> raise Nan_or_inf

include Binable.Of_binable (Float) (struct
    type nonrec t = t
    let of_binable t = verify t; t
    let to_binable t = verify t; t
  end)

let sexp_of_t = Float.sexp_of_t

let t_of_sexp = function
  | Sexp.Atom _ as sexp ->
    let t = Float.t_of_sexp sexp in
    begin
      try
        verify t
      with e -> Conv.of_sexp_error (Exn.to_string e) sexp
    end;
    t
  | s ->
    Conv.of_sexp_error "Decimal.t_of_sexp: Expected Atom, found List" s
;;
