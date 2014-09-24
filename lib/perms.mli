(** These types are intended to be used as phantom types encoding the permissions on a
    given type.  Here's a hypothetical interface to an on-off switch which uses them:

    {|
      module Switch : sig
        type -'permissions t

        val create : unit -> [< _ Perms.Upper_bound.t] t
        val read  : [> Perms.Read.t] t  -> [`On | `Off]
        val write : [> Perms.Write.t] t -> [`On | `Off] -> unit
      end
    |}

    Note that the permissions parameter must be contravariant -- you are allowed to forget
    that you have any particular permissions, but not give yourself new permissions.

    You can now create different "views" of a switch. For example, in:

    {|
      let read_write_s1 : Perms.Read_write.t Switch.t = Switch.create ()
      let read_only_s1 = (s1 :> Perms.Read_only.t t)
    |}

    [read_write_s1] and [read_only_s1] are physically equal, but calling
    [Switch.write read_only_s1] is a type error, while [Switch.write read_write_s1] is
    allowed.

    Also note that this is a type error:

    {|
      let s1 = Switch.create ()
      let read_write_s1 = (s1 :> Perms.Read_write.t t)
      let immutable_s1  = (s1 :> Perms.Immutable.t  t)
    |}

    which is good, since it would be incorrect if it were allowed.  This is enforced by:

    1. Having the permissions parameter be contravariant, which causes the compiler to
    require that the result of a [create ()] call has a concrete type (due to the value
    restriction).

    2. Ensuring that there is no type that has both [Perms.Read_write.t] and
    [Perms.Immutable.t] as subtypes.  This is why the variants are [`Who_can_write
    of Me.t] and [`Who_can_write of Nobody.t] rather than [`I_can_write] and
    [`Nobody_can_write].
*)

(** Every type in this module besides the following two represent permission sets; these
    two represent who is allowed to write in the [Write.t] and [Immutable.t] types. *)
type nobody with bin_io, compare, sexp
type me     with bin_io, compare, sexp

module Read : sig
  type t = [ `Read ]
  with bin_io, compare, sexp
end

module Write : sig
  type t = [ `Who_can_write of me ]
  with bin_io, compare, sexp
end

module Read_only : sig
  type t = [ Read.t ]
  with bin_io, compare, sexp
end

module Immutable : sig
  type t = [ Read.t | `Who_can_write of nobody ]
  with bin_io, compare, sexp
end

module Read_write : sig
  type t = [ Read.t | Write.t ]
  with bin_io, compare, sexp
end

module Upper_bound : sig
  type 'a t = [ Read.t | `Who_can_write of 'a ]
  with bin_io, compare, sexp
end

module Export : sig
  type read_perm        = Read.          t with bin_io, compare, sexp
  type write_perm       = Write.         t with bin_io, compare, sexp
  type read_only_perms  = Read_only.     t with bin_io, compare, sexp
  type immutable_perms  = Immutable.     t with bin_io, compare, sexp
  type read_write_perms = Read_write.    t with bin_io, compare, sexp
  type 'a perms         = 'a Upper_bound.t with bin_io, compare, sexp
end

module Stable : sig
  module V1 : sig
    type nonrec nobody = nobody with bin_io, compare, sexp
    type nonrec me     = me     with bin_io, compare, sexp

    module Read : sig
      type t = Read.t
      with bin_io, compare, sexp
    end

    module Write : sig
      type t = Write.t
      with bin_io, compare, sexp
    end

    module Read_only : sig
      type t = Read_only.t
      with bin_io, compare, sexp
    end

    module Immutable : sig
      type t = Immutable.t
      with bin_io, compare, sexp
    end

    module Read_write : sig
      type t = Read_write.t
      with bin_io, compare, sexp
    end

    module Upper_bound : sig
      type 'a t = 'a Upper_bound.t
      with bin_io, compare, sexp
    end
  end

  module Export : module type of Export
end
