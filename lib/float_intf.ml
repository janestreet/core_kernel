(** Floating-point representation and utilities. *)

module Binable = Binable0

module type S = sig
  type t with typerep
  type outer = t
  with bin_io, sexp, typerep


  include Floatable.S with type t := t
  (** [max] and [min] will return nan if either argument is nan.

      The [validate_*] functions always fail if class is [Nan] or [Infinite]. *)
  include Identifiable.S with type t := t
  include Comparable.With_zero with type t := t
  (** The results of robust comparisons on [nan] should be considered undefined. *)
  include Robustly_comparable.S with type t := t

  (** [validate_ordinary] fails if class is [Nan] or [Infinite]. *)
  val validate_ordinary : t Validate.check

  val nan : t

  val infinity : t
  val neg_infinity : t

  val max_value : t                   (* Float.infinity *)
  val min_value : t                   (* Float.neg_infinity *)
  val zero : t
  val epsilon : t   (* WARNING: This is not [Float.epsilon_float].  See Robust_compare. *)

  (** The difference between 1.0 and the smallest exactly representable floating-point
      number greater than 1.0. *)
  val epsilon_float : t

  val max_finite_value : t

  (** [min_positive_subnormal_value = 2 ** -1074]
      [min_positive_normal_value    = 2 ** -1022] *)
  val min_positive_subnormal_value : t
  val min_positive_normal_value    : t

  (** An order-preserving bijection between all floats except for nans, and all int64s
      with absolute value smaller than or equal to [2**63 - 2**52].
      Note both 0. and -0. map to 0L. *)
  val to_int64_preserve_order : t -> int64 option
  val to_int64_preserve_order_exn : t -> int64
  (** returns [nan] if the absolute value of the argument is too large *)
  val of_int64_preserve_order : int64 -> t

  (** The next or previous representable float.  ULP stands for "unit of least precision",
      and is the spacing between floating point numbers.  Both [one_ulp `Up infinity] and
      [one_ulp `Down neg_infinity] return a nan. *)
  val one_ulp : [`Up | `Down] -> t -> t

  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64

  (** [round] rounds a float to an integer float.  [iround{,_exn}] rounds a float to an
      int.  Both round according to a direction [dir], with default [dir] being [`Nearest].

      {v
        | `Down    | rounds toward Float.neg_infinity                             |
        | `Up      | rounds toward Float.infinity                                 |
        | `Nearest | rounds to the nearest int ("round half-integers up")         |
        | `Zero    | rounds toward zero                                           |
      v}

      [iround_exn] raises when trying to handle nan or trying to handle a float outside the
      range [float min_int, float max_int).


      Here are some examples for [round] for each direction:

      {v
        | `Down    | [-2.,-1.)   to -2. | [-1.,0.)   to -1. | [0.,1.) to 0., [1.,2.) to 1. |
        | `Up      | (-2.,-1.]   to -1. | (-1.,0.]   to -0. | (0.,1.] to 1., (1.,2.] to 2. |
        | `Zero    | (-2.,-1.]   to -1. | (-1.,1.)   to 0.  | [1.,2.) to 1.                |
        | `Nearest | [-1.5,-0.5) to -1. | [-0.5,0.5) to 0.  | [0.5,1.5) to 1.              |
      v}

      For convenience, versions of these functions with the [dir] argument hard-coded are
      provided.  If you are writing performance-critical code you should use the
      versions with the hard-coded arguments (e.g. [iround_down_exn]).  The [_exn] ones
      are the fastest.

      The following properties hold:

      - [of_int (iround_*_exn i) = i] for any float [i] that is an integer with
        [min_int <= i <= max_int].

      - [round_* i = i] for any float [i] that is an integer.

      - [iround_*_exn (of_int i) = i] for any int [i] with [-2**52 <= i <= 2**52].
  *)
  val round      : ?dir:[`Zero|`Nearest|`Up|`Down] -> t -> t
  val iround     : ?dir:[`Zero|`Nearest|`Up|`Down] -> t -> int option
  val iround_exn : ?dir:[`Zero|`Nearest|`Up|`Down] -> t -> int

  val round_towards_zero : t -> t
  val round_down         : t -> t
  val round_up           : t -> t
  val round_nearest      : t -> t

  val iround_towards_zero : t -> int option
  val iround_down         : t -> int option
  val iround_up           : t -> int option
  val iround_nearest      : t -> int option

  val iround_towards_zero_exn : t -> int
  val iround_down_exn         : t -> int
  val iround_up_exn           : t -> int
  val iround_nearest_exn      : t -> int

  (** If [f <= iround_lbound || f >= iround_ubound], then [iround*] functions will refuse
      to round [f], returning [None] or raising as appropriate. *)
  val iround_lbound : t
  val iround_ubound : t


  val is_nan : t -> bool

  (** includes positive and negative Float.infinity *)
  val is_inf : t -> bool

  (** min and max that return the other value if one of the values is a [nan]. Returns
      [nan] if both arguments are [nan]. *)
  val min_inan : t -> t -> t
  val max_inan : t -> t -> t

  val (+) : t -> t -> t
  val (-) : t -> t -> t
  val ( * ) : t -> t -> t
  val (/) : t -> t -> t

  (** Returns the fractional part and the whole (i.e. integer) part.  For example, [modf
      (-3.14)] returns [{ fractional = -0.14; integral = -3.; }]! *)
  module Parts : sig
    type t
    val fractional : t -> outer
    val integral : t -> outer
  end
  val modf : t -> Parts.t

  (** [mod_float x y] returns a result with the same sign as [x].  It returns [nan] if [y]
      is [0].  It is basically
      [let mod_float x y = x -. float(truncate(x/.y)) *. y]
      not
      [let mod_float x y = x -. floor(x/.y) *. y]
      and therefore resembles [mod] on integers more than [%].
  *)
  val mod_float : t -> t -> t

  (* mostly for modules that inherit from t, since the infix operators are more
     convenient *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val scale : t -> t -> t
  val abs : t -> t

  (** A sub-module designed to be opened to make working with floats more convenient.  *)
  module O : sig
    val ( +  ) : t -> t -> t
    val ( -  ) : t -> t -> t
    val ( *  ) : t -> t -> t
    val ( /  ) : t -> t -> t
    val ( ~- ) : t -> t
    include Polymorphic_compare_intf.Infix with type t := t
    include Robustly_comparable.S          with type t := t

    val abs      : t -> t
    val neg      : t -> t
    val zero     : t
    val of_int   : int -> t
    val of_float : float -> t
  end

  (** Like [to_string], but guaranteed to be round-trippable.

      It usually yields as few significant digits as possible.  That is, it won't print
      [3.14] as [3.1400000000000001243].  The only exception is that occasionally it will
      output 17 significant digits when the number can be represented with just 16 (but
      not 15 or less) of them. *)
  val to_string_round_trippable : float -> string

  (** Pretty print float, for example [to_string_hum ~decimals:3 1234.1999 = "1_234.200"]
      [to_string_hum ~decimals:3 ~strip_zero:true 1234.1999 = "1_234.2" ].  No delimiters
      are inserted to the right of the decimal. *)
  val to_string_hum
    :  ?delimiter:char  (* defaults to '_' *)
    -> ?decimals:int    (* defaults to 3 *)
    -> ?strip_zero:bool (* defaults to false *)
    -> float
    -> string

  (** Produce a lossy compact string representation of the float.  The float is scaled by
      an appropriate power of 1000 and rendered with one digit after the decimal point,
      except that the decimal point is written as '.', 'k', 'm', 'g', 't', or 'p' to
      indicate the scale factor.  (However, if the digit after the "decimal" point is 0,
      it is suppressed.)  The smallest scale factor that allows the number to be rendered
      with at most 3 digits to the left of the decimal is used.  If the number is too
      large for this format (i.e., the absolute value is at least 999.95e15), scientific
      notation is used instead. E.g.:

      {[
        to_padded_compact_string     (-0.01) =  "-0  "
        to_padded_compact_string       1.89  =   "1.9"
        to_padded_compact_string 999_949.99  = "999k9"
        to_padded_compact_string 999_950.    =   "1m "
      ]}

      In the case where the digit after the "decimal", or the "decimal" itself are
      omitted, the numbers are padded on the right with spaces to ensure the last two
      columns of the string always correspond to the decimal and the digit afterward
      (except in the case of scientific notation, where the exponent is the right-most
      element in the string and could take up to four characters).

      {[
        to_padded_compact_string    1. =    "1  ";
        to_padded_compact_string  1.e6 =    "1m ";
        to_padded_compact_string 1.e16 = "1.e+16";
        to_padded_compact_string max_finite_value = "1.8e+308";
      ]}

      Numbers in the range -.05 < x < .05 are rendered as "0  " or "-0  ".

      Other cases:

      {[
        to_padded_compact_string nan          =  "nan  "
        to_padded_compact_string infinity     =  "inf  "
        to_padded_compact_string neg_infinity = "-inf  "
      ]}

      Exact ties are resolved to even in the decimal:

      {|
        to_padded_compact_string      3.25 =  "3.2"
        to_padded_compact_string      3.75 =  "3.8"
        to_padded_compact_string 33_250.   = "33k2"
        to_padded_compact_string 33_350.   = "33k4"
      |}

  *)
  val to_padded_compact_string : float -> string

  (** [ldexp x n] returns [x *. 2 ** n] *)
  val ldexp : t -> int -> t

  (** [frexp f] returns the pair of the significant and the exponent of f. When f is zero,
      the significant x and the exponent n of f are equal to zero. When f is non-zero,
      they are defined by [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)
  val frexp : t -> t * int

  module Class : sig
    type t =
    | Infinite
    | Nan
    | Normal
    | Subnormal
    | Zero
    with bin_io, sexp

    include Stringable.S with type t := t
  end

  (* return the Class.t.  Excluding nan the floating-point "number line" looks like:
       t                Class.t    example
     ^ neg_infinity     Infinite   neg_infinity
     | neg normals      Normal     -3.14
     | neg subnormals   Subnormal  -.2. ** -1023.
     | (-/+) zero       Zero       0.
     | pos subnormals   Subnormal  2. ** -1023.
     | pos normals      Normal     3.14
     v infinity         Infinite   infinity
  *)
  val classify : t -> Class.t

  (** [is_finite t] returns [true] iff [classify t] is in [Normal; Subnormal; Zero;]. *)
  val is_finite : t -> bool

  module Sign : sig
    type t = Neg | Zero | Pos with sexp
  end

  val sign : t -> Sign.t

  (** S-expressions contain at most 8 significant digits. *)
  module Terse : sig
    type t = outer with bin_io, sexp
    include Stringable.S with type t := t
  end

end
