module Binable = Binable0
module Hashtbl = Core_hashtbl

module type S = sig
  type t
  val hash : t -> int
  val compare : t -> t -> int
  val hashable : t Hashtbl.Hashable.t
  module Table      : Hashtbl   .S with type key   = t
  module Hash_set   : Hash_set  .S with type elt   = t
  module Hash_queue : Hash_queue.S with type Key.t = t
end

module Make (T : Hashtbl.Key) : S with type t := T.t = struct
  include T
  module Table      = Hashtbl   .Make (T)
  module Hash_set   = Hash_set  .Make (T)
  module Hash_queue = Hash_queue.Make (T)
  let hashable = Table.hashable
end

module type S_binable = sig
  type t
  val hash : t -> int
  val hashable : t Hashtbl.Hashable.t
  module Table      : Hashtbl.   S_binable with type key = t
  module Hash_set   : Hash_set.  S_binable with type elt = t
  module Hash_queue : Hash_queue.S         with type Key.t = t
end

module Make_binable (T : Hashtbl.Key_binable) : S_binable with type t := T.t = struct
  module Table      = Hashtbl   .Make_binable (T)
  module Hash_set   = Hash_set  .Make_binable (T)
  module Hash_queue = Hash_queue.Make         (T)

  include T
  let hashable = Table.hashable
end
