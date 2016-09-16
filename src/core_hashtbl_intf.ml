open! Import

module Binable = Binable0

module Hashtbl      = Base.Hashtbl
module Hashtbl_intf = Base.Hashtbl_intf

module type Key_common = Hashtbl_intf.Key_common
module type Key_plain  = Hashtbl_intf.Key_plain

module      Hashable = Hashtbl_intf.Hashable
module type Hashable = Hashtbl_intf.Hashable

module type Key = sig
  type t [@@deriving sexp]
  include Key_common with type t := t
end

module type Key_binable = sig
  type t [@@deriving bin_io, sexp]
  include Key_common with type t := t
end

module type Creators  = Hashtbl_intf.Creators
module type Accessors = Hashtbl_intf.Accessors

type ('key, 'data, 'z) create_options_without_hashable =
  ('key, 'data, 'z) Hashtbl_intf.create_options_without_hashable

type ('key, 'data, 'z) create_options_with_hashable =
  ('key, 'data, 'z) Hashtbl_intf.create_options_with_hashable

module type S_plain = sig
  type key
  type ('a, 'b) hashtbl
  type 'b t = (key, 'b) hashtbl [@@deriving sexp_of]
  type ('a, 'b) t_ = 'b t
  type 'a key_ = key

  val hashable : key Hashable.t

  include Invariant.S1 with type 'b t := 'b t

  include Creators
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a key_
    with type ('key, 'data, 'z) create_options := ('key, 'data, 'z) create_options_without_hashable

  include Accessors
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a key_

  module Provide_of_sexp (Key : sig type t [@@deriving of_sexp] end with type t := key)
    : sig type _ t [@@deriving of_sexp] end with type 'a t := 'a t
  module Provide_bin_io (Key : sig type t [@@deriving bin_io] end with type t := key)
    : sig type 'a t [@@deriving bin_io] end with type 'a t := 'a t
end

module type S = sig
  include S_plain
  include sig
    type _ t [@@deriving of_sexp]
  end with type 'a t := 'a t
end

module type S_binable = sig
  include S
  include Binable.S1 with type 'v t := 'v t
end

module type Hashtbl = sig
  include Hashtbl_intf.S_without_submodules

  (** Shadowing the previous accessors with ones that take [Hashable.t] *)
  include Hashtbl_intf.S_using_hashable
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a key := 'a key
    with type 'a merge_into_action := 'a merge_into_action

  module Poly : sig
    type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving bin_io]
    include Hashtbl_intf.S_poly with type ('a, 'b) t := ('a, 'b) t
  end

  module type Key_plain   = Key_plain
  module type Key         = Key
  module type Key_binable = Key_binable

  module type S_plain   = S_plain   with type ('a, 'b) hashtbl = ('a, 'b) t
  module type S         = S         with type ('a, 'b) hashtbl = ('a, 'b) t
  module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t

  module Make_plain   (Key : Key_plain)   : S_plain   with type key = Key.t
  module Make         (Key : Key        ) : S         with type key = Key.t
  module Make_binable (Key : Key_binable) : S_binable with type key = Key.t
end
