open! Import


(** Non-re-entrant memoization. *)


(** Returns a memoized version of any function with a single argument. The
    default caching policy is to remember everything for the lifetime
    of the returned closure, but one may specify an upper bound on
    cache size. Whenever a cache entry must be forgotten in order to
    obey this bound, we pick the least-recently-used one.

    Raises an exception if [cache_size_bound] is negative or zero.

    Note: the input function must take _only_ one argument; to memoize a function with
    multiple arguments, pack them up in a tuple. See ../test/src/memo_argument.mlt for
    some examples. *)
val general
  :  ?hashable:'a Hashtbl.Hashable.t
  -> ?cache_size_bound:int
  -> ('a -> 'b)
  -> ('a -> 'b)

(** efficient special case for argument type [unit] *)
val unit : (unit -> 'a) -> (unit -> 'a)

(** Use a comparable instead of hashable type *)
val of_comparable
  :  (module Comparable.S_plain with type t = 'a)
  -> ('a -> 'b)
  -> ('a -> 'b)
