open! Import
open Hash_set_intf
module Hashtbl = Core_hashtbl

module Creators = Base.Hash_set.Creators
module Poly = Base.Hash_set.Poly
let sexp_of_t = Base.Hash_set.sexp_of_t
let hashable = Base.Hash_set.hashable
include Base.Hash_set.Using_hashable

module type S_plain   = S_plain   with type 'a hash_set := 'a t
module type S         = S         with type 'a hash_set := 'a t
module type S_binable = S_binable with type 'a hash_set := 'a t

module type Elt_plain   = Hashtbl.Key_plain
module type Elt         = Hashtbl.Key
module type Elt_binable = Hashtbl.Key_binable

module Make_plain (Elt : Elt_plain) = struct
  type elt = Elt.t
  type nonrec t = elt t
  type 'a elt_ = elt

  include Creators (struct
      type 'a t = Elt.t
      let hashable = Hashtbl.Hashable.of_key (module Elt)
    end)

  let sexp_of_t t = Poly.sexp_of_t Elt.sexp_of_t t

  module Provide_of_sexp(X : sig type t [@@deriving of_sexp] end with type t := elt) =
  struct
    let t_of_sexp sexp = t_of_sexp X.t_of_sexp sexp
  end

  module Provide_bin_io(X : sig type t [@@deriving bin_io] end with type t := elt) =
    Bin_prot.Utils.Make_iterable_binable (struct
      module Elt = struct include Elt include X end
      type nonrec t = t
      type el = Elt.t [@@deriving bin_io]
      let _ = bin_el
      let caller_identity = Bin_prot.Shape.Uuid.of_string "ad381672-4992-11e6-9e36-b76dc8cd466f"
      let module_name = Some "Core_kernel.Hash_set"
      let length = length
      let iter = iter
      let init ~len ~next =
        let t = create ~size:len () in
        for _i = 0 to len - 1 do
          let v = next () in
          add t v;
        done;
        t
    end)
end

module Make (Elt : Elt) = struct
  include Make_plain (Elt)
  include Provide_of_sexp (Elt)
end

module Make_binable (Elt : Elt_binable) = struct
  include Make (Elt)
  include Provide_bin_io (Elt)
end
