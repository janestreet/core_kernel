open Std_internal

type t

val create : host:string -> port:int -> t

val host : t -> string
val port : t -> int
val tuple : t -> string * int

include Identifiable with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]
  end
end

val type_id : t Type_equal.Id.t
