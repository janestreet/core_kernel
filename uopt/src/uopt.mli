(** [Uopt.t] is an unboxed option: an [option]-like type that incurs no allocation,
    without requiring a reserved value in the underlying type.

    The downsides compared to [option] are that:
    - [Uopt.t] cannot be nested, i.e. used as ['a Uopt.t Uopt.t], because trying to
      create [Uopt.some Uopt.none] is not supported and would raise.
    - it is unsafe to have values of type [float Uopt.t array], or any type which has the
      same memory representation, since the representation of the array would vary
      depending on whether [none] or [some] is used to create the array.
      Using [float Uopt.t Uniform_array.t] is fine.
    - the implementation has unsafe code which has resulted in miscompilation in the past.

    As a result, we advise against using this in systems that are not high performance.

    When using Uopt, we recommend:
    - not exposing Uopt in APIs for casual users, so you don't force other people to learn
      about this unnecessarily.
    - not giving values of type [Uopt.t] (whether the type is abstract or not) to other
      APIs, so they are free to use Uopt internally (and also for memory safety in the
      cause of [float Uopt.t]).
    - not returning values of type [Uopt.t] from your libraries when the type is abstract,
      so callers are free to use [Uopt.t] on abstract types. Returning explicit [Uopt.t]
      can be fine, although turning a type that's not Uopt into a type that is could break
      code.

    Since ['a Uopt.t] is abstract, manipulation of an ['a Uopt.t array] does runtime
    checks to see if this is a float array. This can be mostly avoided with
    [Uniform_array.t], although array creation will still do such checks, and you may want
    to use the [set_with_caml_modify] kind of function to skip the immediacy checks. *)


open! Base

type +'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val none : _ t
val some : 'a -> 'a t
val some_local : ('a[@local]) -> ('a t[@local])
val is_none : (_ t[@local]) -> bool
val is_some : (_ t[@local]) -> bool
val value_exn : 'a t -> 'a

(** It is safe to call [unsafe_value t] iff [is_some t]. *)
val unsafe_value : 'a t -> 'a

val to_option : 'a t -> 'a option
val of_option : 'a option -> 'a t

module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none : _ t -> bool
    val unsafe_value : 'a t -> 'a
  end
end

module Local : sig
  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : (_ t[@local]) -> bool
      val unsafe_value : ('a t[@local]) -> ('a[@local])
    end
  end
end
