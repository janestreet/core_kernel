(** A ['a Set_once.t] is like an ['a option ref] that can only be set once.  A
    [Set_once.t] starts out as [None], the first [set] transitions it to [Some], and
    subsequent [set]s fail. *)

open! Import

type 'a t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t (** Passes when unset. *)

val create : unit -> _ t

val set     : 'a t -> Source_code_position.t -> 'a -> unit Or_error.t
val set_exn : 'a t -> Source_code_position.t -> 'a -> unit

val get     : 'a t -> 'a option
val get_exn : 'a t -> Source_code_position.t -> 'a

val is_none : _ t -> bool
val is_some : _ t -> bool

val iter : 'a t -> f:('a -> unit) -> unit


(** [Optional_syntax] defines functions used by [match%optional].  Usage looks like:

    {[
      let open Set_once.Optional_syntax in
      match%optional some_set_once_value with
      | None   -> ?
      | Some a -> ?
    ]} *)
module Optional_syntax : sig
  module Optional_syntax : sig
    val is_none         : _  t -> bool
    val unchecked_value : 'a t -> 'a
  end
end

module Unstable : sig
  type nonrec 'a t = 'a t [@@deriving bin_io, sexp]
end

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, sexp]
  end
end
