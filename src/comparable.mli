open Comparable_intf

module type Infix               = Infix
module type Map_and_set_binable = Map_and_set_binable
module type S                   = S
module type S_binable           = S_binable
module type S_common            = S_common
module type Validate            = Validate
module type With_zero           = With_zero

(** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in the
    list [cmps]. *)
val lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int

(** Inherit comparability from a component. *)
module Inherit
  (C : sig type t [@@deriving compare] end)
  (T : sig
    type t [@@deriving sexp]
    val component : t -> C.t
  end) : S with type t := T.t

(** Usage example:

    {[
      module Foo = struct
        module T = struct
          type t = ... [@@deriving compare, sexp]
        end
        include T
        include Comparable.Make (T)
      end
    ]}

    Then include [Comparable.S] in the signature (see comparable_intf.mli for an
    example).

    To add an [Infix] submodule:

    {[
      module C = Comparable.Make (T)
      include C
      module Infix = (C : Comparable.Infix with type t := t)
    ]}

    Common pattern: Define a module [O] with a restricted signature.  It aims to be
    (locally) opened to bring useful operators into scope without shadowing unexpected
    variable names.  E.g. in the [Date] module:

    {[
      module O = struct
        include (C : Comparable.Infix with type t := t)
        let to_string t = ..
      end
    ]}

    Opening [Date] would shadow [now], but opening [Date.O] doesn't:

    {[
      let now = .. in
      let someday = .. in
      Date.O.(now > someday)
    ]}
*)
module Make (T : sig
  type t [@@deriving compare, sexp]
end) : S with type t := T.t

module Make_using_comparator (T : sig
    type t [@@deriving sexp]
    include Comparator.S with type t := t
  end) : S
  with type t := T.t
  with type comparator_witness := T.comparator_witness

module Make_binable (T : sig
  type t [@@deriving bin_io, compare, sexp]
end) : S_binable with type t := T.t

module Make_binable_using_comparator (T : sig
    type t [@@deriving bin_io, sexp]
    include Comparator.S with type t := t
  end) : S_binable
  with type t := T.t
  with type comparator_witness := T.comparator_witness

module Map_and_set_binable (T : sig type t [@@deriving bin_io, compare, sexp] end)
  : Map_and_set_binable with type t := T.t

module Poly (T : sig type t [@@deriving sexp] end) : S with type t := T.t

module Validate (T : sig type t [@@deriving compare, sexp] end) : Validate with type t := T.t

module With_zero
         (T : sig
            type t [@@deriving compare, sexp]
            val zero : t
            include Validate with type t := t
          end) : With_zero with type t := T.t

module Validate_with_zero
         (T : sig
            type t [@@deriving compare, sexp]
            val zero : t
          end)
  : sig
    include Validate  with type t := T.t
    include With_zero with type t := T.t
  end

(** [Check_sexp_conversion] checks that conversion of a map or set to a sexp uses the same
    sexp conversion as the underlying element. *)
module Check_sexp_conversion (M : sig
  type t [@@deriving sexp_of]
  include S with type t := t
  val examples : t list
end) : sig end
