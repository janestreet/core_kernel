open! Import

include Deriving_hash_intf

module Of_deriving_hash
    (Repr : S)
    (M : sig
       type t
       val to_repr : t -> Repr.t
     end) = struct
  type t = M.t
  let hash_fold_t state t = Repr.hash_fold_t state (M.to_repr t)
  let hash = [%hash: t]
end
