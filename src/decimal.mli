(** The [decimal] type alias provides more readable serializations to s-expressions, at
    the cost of lower precision.  For example:

    {[
    # sexp_of_decimal 3.000000000001;;
    - : Sexp.t = 3
    # sexp_of_float 3.000000000001;;
    - : Sexp.t = 3.0000000000010000889
    ]}

    Also, the decimal sexp-converter will fail when provided with [nan] or [infinity].

    {[
    # float_of_sexp (Sexp.Atom "nan");;
    - : float = nan
    # decimal_of_sexp (Sexp.Atom "nan");;
    Exception:
    (Sexplib.Conv.Of_sexp_error (Failure common.ml.Decimal_nan_or_inf) nan).
    ]}
*)

type t = float [@@deriving bin_io, sexp, compare]
