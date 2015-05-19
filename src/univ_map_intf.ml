module Key = Type_equal.Id

module type S = sig
  type t with sexp_of

  type 'a data

  include Invariant.S with type t := t

  val empty : t
  val is_empty : t -> bool

  val set : t -> 'a Key.t -> 'a data -> t
  val mem : t -> 'a Key.t -> bool

  val find     : t -> 'a Key.t -> 'a data option
  val find_exn : t -> 'a Key.t -> 'a data

  val add     : t -> 'a Key.t -> 'a data -> [ `Ok of t | `Duplicate ]
  val add_exn : t -> 'a Key.t -> 'a data -> t

  val change     : t -> 'a Key.t -> ('a data option -> 'a data option) -> t
  val change_exn : t -> 'a Key.t -> ('a data        -> 'a data       ) -> t

  module Packed : sig
    type t = T : 'a Key.t * 'a data -> t
  end

  val to_alist : t -> Packed.t list
end

module type S1 = sig
  (** The ['s] parameter is shared across all values stored in the map. *)
  type 's t with sexp_of

  type ('s, 'a) data

  val invariant : _ t -> unit

  val empty : _ t
  val is_empty : _ t -> bool

  val set : 's t -> 'a Key.t -> ('s, 'a) data -> 's t
  val mem : _ t -> _ Key.t -> bool

  val find     : 's t -> 'a Key.t -> ('s, 'a) data option
  val find_exn : 's t -> 'a Key.t -> ('s, 'a) data

  val add     : 's t -> 'a Key.t -> ('s, 'a) data -> [ `Ok of 's t | `Duplicate ]
  val add_exn : 's t -> 'a Key.t -> ('s, 'a) data ->          's t

  val change     : 's t -> 'a Key.t -> (('s, 'a) data option -> ('s, 'a) data option) -> 's t
  val change_exn : 's t -> 'a Key.t -> (('s, 'a) data        -> ('s, 'a) data       ) -> 's t

  module Packed : sig
    type 's t = T : 'a Key.t * ('s, 'a) data -> 's t
  end

  val to_alist : 's t -> 's Packed.t list
end
