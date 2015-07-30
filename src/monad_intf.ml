module type Basic = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  (** The following identities ought to hold (for some value of =):

      - [return x >>= f = f x]
      - [t >>= fun x -> return x = t]
      - [(t >>= f) >>= g = t >>= fun x -> (f x >>= g)]

     Note: [>>=] is the infix notation for [bind])
  *)

  (** The [map] argument to [Monad.Make] says how to implement the monad's [map] function.
      [`Define_using_bind] means to define [map t ~f = bind t (fun a -> return (f a))].
      [`Custom] overrides the default implementation, presumably with something more
      efficient.

      Some other functions returned by [Monad.Make] are defined in terms of [map], so
      passing in a more efficient [map] will improve their efficiency as well. *)
  val map : [ `Define_using_bind
            | `Custom of ('a t -> f:('a -> 'b) -> 'b t)
            ]
end

module type Infix = sig
  type 'a t

  (** [t >>= f] returns a computation that sequences the computations represented by two
      monad elements.  The resulting computation first does [t] to yield a value [v], and
      then runs the computation returned by [f v]. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  (** [t >>| f] is [t >>= (fun a -> return (f a))]. *)
  val (>>|) : 'a t -> ('a -> 'b) -> 'b t

end

module type S = sig
  (** A monad is an abstraction of the concept of sequencing of computations.  A value of
      type 'a monad represents a computation that returns a value of type 'a. *)
  include Infix

  module Monad_infix : Infix with type 'a t := 'a t

  (** [bind t f] = [t >>= f] *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [return v] returns the (trivial) computation that returns v. *)
  val return : 'a -> 'a t

  (** [map t ~f] is t >>| f. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** [join t] is [t >>= (fun t' -> t')]. *)
  val join : 'a t t -> 'a t

  (** [ignore_m t] is [map t ~f:(fun _ -> ())].  [ignore_m] used to be called [ignore],
      but we decided that was a bad name, because it shadowed the widely used
      [Pervasives.ignore].  Some monads still do [let ignore = ignore_m] for historical
      reasons. *)
  val ignore_m : 'a t -> unit t

  val all : 'a t list -> 'a list t
  val all_ignore : unit t list -> unit t
end

(**
   Multi parameter monad.
   The second parameter get unified across all the computation. This is used
   to encode monads working on a multi parameter data structure like
   ([('a,'b result)]).
*)
module type Basic2 = sig
  type ('a, 'e) t
  val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val map : [ `Define_using_bind
            | `Custom of (('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t)
            ]
  val return : 'a -> ('a, _) t
end

(** Same as Infix, except the monad type has two arguments. The second is always just
    passed through. *)
module type Infix2 = sig
  type ('a, 'e) t
  val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val (>>|) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
end

(** The same as S except the monad type has two arguments. The second is always just
    passed through. *)
module type S2 = sig
  include Infix2

  module Monad_infix : Infix2 with type ('a, 'e) t := ('a, 'e) t

  val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

  val return : 'a -> ('a, _) t

  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t

  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t

  val ignore_m : (_, 'e) t -> (unit, 'e) t

  val all : ('a, 'e) t list -> ('a list, 'e) t

  val all_ignore : (unit, 'e) t list -> (unit, 'e) t
end

module S_to_S2 (X : S) : (S2 with type ('a, 'e) t = 'a X.t) = struct
  type ('a, 'e) t = 'a X.t
  include (X : S with type 'a t := 'a X.t)
end

module type Monad = sig

  module type Basic  = Basic
  module type Basic2 = Basic2
  module type Infix  = Infix
  module type Infix2 = Infix2
  module type S      = S
  module type S2     = S2

  module Make  (X : Basic ) : S  with type  'a      t :=  'a      X.t
  module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

  module Ident : S with type 'a t = 'a
end
