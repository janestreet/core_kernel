type t = bool with bin_io, sexp, typerep

open Interfaces
include Comparable with type t := t
include Hashable   with type t := t
include Stringable with type t := t
