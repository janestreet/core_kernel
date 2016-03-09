type t = bool [@@deriving bin_io, sexp, typerep]

open Interfaces
include Comparable     with type t := t
include Hashable       with type t := t
include Stringable     with type t := t
include Quickcheckable with type t := t

(** - [to_int true = 1]
    - [to_int false = 0]
*)
val to_int : t -> int
