(** A [('key, 'value, cmp) Map.t] where every value of type ['key] is present.

    This is intended to be used on ['key] types where there is a full enumeration of the
    type.  In the common use case, ['key] will be a simple variant type with [with
    compare, enumerate].  For example:

    {[
      module Arrow_key = struct
        module T = struct
          type t =
            | Up
            | Down
            | Left
            | Right
          with sexp, bin_io, compare, enumerate
        end
        include T
        module Total_map = Total_map.Make (T)
      end
    ]}

    In such a case, a [t] is semantically equivalent to a pure function from ['key] to
    ['value].  The differences are that it is serializable and that mapping or changing a
    [t] will produce a [t] using the same amount of space as the original.

    However, in theory you could also modify the comparison function and enumeration, so
    long as the enumeration contains at least one representative of each equivalence class
    determined by the comparison function.
*)

open Std_internal

type ('key, 'a, 'cmp) t = private ('key, 'a, 'cmp) Map.t

val to_map : ('key, 'a, 'cmp) t -> ('key, 'a, 'cmp) Map.t

(** Many of the functions below have types reflecting the fact that the maps are total
    (e.g., [find] does not return an option).  The fact that they won't raise exceptions
    relies on the enumeration passed to [Make] being complete. *)

val map  : ('key, 'a, 'cmp) t -> f:(                 'a -> 'b) -> ('key, 'b, 'cmp) t
val mapi : ('key, 'a, 'cmp) t -> f:(key:'key -> data:'a -> 'b) -> ('key, 'b, 'cmp) t
val map2
  :  ('key, 'a, 'cmp) t
  -> ('key, 'b, 'cmp) t
  -> f:('a -> 'b -> 'c)
  -> ('key, 'c, 'cmp) t

val iter : ('key, 'a, _) t -> f:(key:'key -> data:'a -> unit) -> unit
val iter2
  :  ('key, 'a, 'cmp) t
  -> ('key, 'b, 'cmp) t
  -> f:(key:'key -> 'a -> 'b -> unit)
  -> unit

val set : ('key, 'a, 'cmp) t -> 'key -> 'a -> ('key, 'a, 'cmp) t

val to_alist : ('key, 'a, _) t -> ('key * 'a) list

val find : ('key, 'a, _) t -> 'key -> 'a

val change : ('key, 'a, 'cmp) t -> 'key -> ('a -> 'a) -> ('key, 'a, 'cmp) t

(** The only reason that the Applicative interface isn't included here is that we don't
    have an [Applicative.S3]. *)

module type Key = sig
  type t with sexp, bin_io, compare, enumerate
end

module type S = sig
  module Key : Key

  type comparator_witness

  type nonrec 'a t = (Key.t, 'a, comparator_witness) t with sexp, bin_io, compare

  include Applicative with type 'a t := 'a t

  val create : (Key.t -> 'a) -> 'a t
end

module Make (Key : Key) : S with module Key = Key

