open! Import
open  Std_internal

include Time_ns_intf.Ofday
  with type span := Span_ns.t

module Stable : sig
  module V1 : Stable_int63able with type t = t
end
