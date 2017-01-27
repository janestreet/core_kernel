open! Import

module type S = sig
  (** The immediate value used to implement the immediate option.

      [S] should be used with [immediate] replaced by a concrete type so the compiler can
      optimize uses of [t] as an immediate value.  For example, use [S with type immediate
      := int] or use the predefined instantiations below. *)
  type immediate

  (** The immediate value carried by the immediate option.

      Given the presence of {!unchecked_value}, the [value] type should not have
      operations that depend on the value's validity for memory safety.  In particular,
      [unchecked_value] is not called [unsafe_value] as it would be if it could return a
      value that later resulted in a segmentation fault.  For pointer-like values, use
      {!Zero.Ext.Nullable}, for example. *)
  type value

  (** Represents [value option] using [immediate] in order to avoid allocating a [Some]
      tag.  Exports the representation so the compiler can optimize [t] as an immediate
      type, assuming [immediate] is known to be immediate.  The interface does not enforce
      that [immediate] is immediate because some types, like [Int63.t], are only immediate
      on 64-bit platforms. *)
  type t = private immediate [@@deriving compare, hash, sexp_of, typerep]

  (** Constructors analogous to [None] and [Some]. *)
  val none : t
  val some : value -> t

  val is_none : t -> bool
  val is_some : t -> bool

  (** [value (some x) ~default = x] and [value none ~default = default]. *)
  val value : t -> default : value -> value

  (** [value_exn (some x) = x].  [value_exn none] raises.  Unlike [Option.value_exn],
      there is no [?message] argument, so that calls to [value_exn] that do not raise
      also do not have to allocate. *)
  val value_exn : t -> value

  (** [unchecked_value (some x) = x].  [unchecked_value none] returns an unspecified
      value.  [unchecked_value t] is intended as an optimization of [value_exn t] when
      [is_some t] is known to be true. *)
  val unchecked_value : t -> value

  val to_option : t -> value option
  val of_option : value option -> t

  (** Provides bindings used by the [match%optional] ppx transformer.  Example:

      {[
        let open Immediate.Int.Option.Optional_syntax in
        match%optional Immediate.Int.Option.some 3 with
        | None   -> ()
        | Some x -> printf "%d" x
      ]}

  *)
  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none         : t -> bool
      val unchecked_value : t -> value
    end
  end
end

module type Int = S
  with type immediate := int
   and type t = private int

(** [Int63.t] is only immediate on 64-bit platforms. *)
module type Int63 = S
  with type immediate := Base.Int63.t
   and type t = private Base.Int63.t

module type Immediate_option = sig
  module type S     = S
  module type Int   = Int
  module type Int63 = Int63
end
