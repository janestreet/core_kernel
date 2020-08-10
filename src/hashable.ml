(** Functors and interfaces used to make modules hashable. *)

open! Import
include Hashable_intf
module Binable = Binable0

module Make_plain (T : sig
    type t [@@deriving hash]

    include Hashtbl.Key_plain with type t := t
  end) : S_plain with type t := T.t = struct
  include T
  module Table = Hashtbl.Make_plain (T)
  module Hash_set = Hash_set.Make_plain (T)
  module Hash_queue = Hash_queue.Make (T)

  let hashable = Table.hashable
end

module Make_plain_and_derive_hash_fold_t (T : Hashtbl.Key_plain) :
  S_plain with type t := T.t = Make_plain (struct
    include T

    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Make (T : sig
    type t [@@deriving hash]

    include Hashtbl.Key with type t := t
  end) : S with type t := T.t = struct
  include T
  module Table = Hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  module Hash_queue = Hash_queue.Make (T)

  let hashable = Table.hashable
end

module Make_and_derive_hash_fold_t (T : Hashtbl.Key) : S with type t := T.t = Make (struct
    include T

    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Make_binable (T : sig
    type t [@@deriving hash]

    include Hashtbl.Key_binable with type t := t
  end) : S_binable with type t := T.t = struct
  module Table = Hashtbl.Make_binable (T)
  module Hash_set = Hash_set.Make_binable (T)
  module Hash_queue = Hash_queue.Make (T)
  include T

  let hashable = Table.hashable
end

module Make_binable_and_derive_hash_fold_t (T : Hashtbl.Key_binable) :
  S_binable with type t := T.t = Make_binable (struct
    include T

    let hash_fold_t state t = hash_fold_int state (hash t)
  end)

module Stable = struct
  module V1 = struct
    module type S = sig
      type key

      module Table : sig
        type 'a t = (key, 'a) Hashtbl.t [@@deriving sexp, bin_io]
      end

      module Hash_set : sig
        type t = key Hash_set.t [@@deriving sexp, bin_io]
      end
    end

    module Make (Key : Hashtbl.Key_binable) : S with type key := Key.t = struct
      module Table = Hashtbl.Make_binable (Key)
      module Hash_set = Hash_set.Make_binable (Key)
    end
  end
end
