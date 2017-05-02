(** This module extends the Base [Char] module *)

type t = char [@@deriving typerep]

include module type of struct include Base.Char end
  with type t := t

include Identifiable.S
  with type t := t
   and type comparator_witness := comparator_witness

include Quickcheckable.S with type t := t

val gen_digit      : t Quickcheck.Generator.t
val gen_lowercase  : t Quickcheck.Generator.t
val gen_uppercase  : t Quickcheck.Generator.t
val gen_alpha      : t Quickcheck.Generator.t
val gen_alphanum   : t Quickcheck.Generator.t
val gen_print      : t Quickcheck.Generator.t
val gen_whitespace : t Quickcheck.Generator.t
