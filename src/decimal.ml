open Sexplib

exception Decimal_nan_or_inf [@@deriving sexp]

type t = float [@@deriving compare]

let verify t =
  match Pervasives.classify_float t with
  | FP_normal
  | FP_subnormal
  | FP_zero      -> ()
  | FP_infinite
  | FP_nan       -> raise Decimal_nan_or_inf

include Binable.Of_binable (Float) (struct
    type nonrec t = t
    let of_binable t = verify t; t
    let to_binable t = verify t; t
  end)

let sexp_of_t t = Sexp.Atom (Core_printf.sprintf "%.12G" t)

let t_of_sexp = function
  | Sexp.Atom s ->
    let t = Float.of_string s in
    begin
      try
        verify t
      with e -> Conv.of_sexp_error (Exn.to_string e) (Sexp.Atom s)
    end;
    t
  | s ->
    Conv.of_sexp_error "Decimal.t_of_sexp: Expected Atom, found List" s
;;
