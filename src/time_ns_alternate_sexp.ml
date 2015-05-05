(* A [Time_ns] that uses its alternate sexp representation. *)
include (Time_ns : (module type of struct include Time_ns end
                     with module Span := Time_ns.Span))

include (Alternate_sexp : sig type t with sexp end with type t := t)

module Span = struct
  include Time_ns.Span

  include (Alternate_sexp : sig type t with sexp end with type t := t)
end

