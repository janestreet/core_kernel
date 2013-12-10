open Interfaces

module type S = sig
  type t with bin_io, sexp, typerep

  include Floatable            with type t := t
  include Intable              with type t := t
  include Identifiable         with type t := t
  include Comparable.With_zero with type t := t

  (** [delimiter] is underscore by default *)
  val to_string_hum : ?delimiter:char -> t -> string

  (** The number of bits available in this integer type.  Note that the integer
    representations are signed *)
  val num_bits : int

  (** {9 Infix operators and constants } *)

  val zero : t
  val one : t
  val minus_one : t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  (** Negation *)
  val neg : t -> t
  val ( ~- ) : t -> t

  (** A sub-module designed to be opened to make working with ints more convenient.  *)
  module O : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( ~- ) : t -> t
    include Polymorphic_compare_intf.Infix with type t := t

    val abs    : t -> t
    val neg    : t -> t
    val zero   : t
    val of_int_exn : int -> t
  end

  (** {9 Successor and predecessor functions } *)

  val succ : t -> t
  val pred : t -> t

  (** Returns the absolute value of the argument.  May be negative if the input is
      [min_value] *)
  val abs : t -> t

  (** Integer remainder, with the semantics of [mod] in [Pervasives] or [rem] in
      [Int32/64], i.e.  if [y] is not zero, the result of [rem x y] satisfies the
      following properties: [x = (x / y) * y + rem x y] and [abs (rem x y) <= abs y - 1].
      If [y = 0], [rem x y] raises [Division_by_zero].  Notice that [rem x y] is
      nonpositive if and only if [x < 0]. *)
  val rem : t -> t -> t

  (** The largest representable integer *)
  val max_value : t
  (** The smallest representable integer *)
  val min_value : t

  (** {9 Bit-wise logical operations } *)

  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> t

  (** {9 Bit-shifting operations }

      The results are unspecified for negative shifts and shifts [>= num_bits] *)

  (** shifts left, filling in with zeroes *)
  val shift_left : t -> int -> t
  (** shifts right, preserving the sign of the input. *)
  val shift_right : t -> int -> t
  (** shifts right, filling in with zeroes, which will not preserve the sign of the
      input *)
  val shift_right_logical : t -> int -> t

  (** {9 Increment and decrement functions for integer references } *)

  val decr : t ref -> unit
  val incr : t ref -> unit

  (** {9 Conversion functions to related integer types} *)

  val of_int32_exn : int32 -> t
  val to_int32_exn : t -> int32
  val of_int64_exn : int64 -> t
  val to_int64 : t -> int64
  val of_nativeint_exn : nativeint -> t
  val to_nativeint_exn : t -> nativeint

end

TEST_MODULE = struct
  (* this functor's type-correctness ensures that every value in [S.O] is also in [S]. *)
  module Check_O_contained_in_S (M : S) : sig end = (M : module type of M.O)
end
