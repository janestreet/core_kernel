
(** A polymorphic hashtbl that uses [Pool] to avoid allocation.

    This uses the standard linked-chain hashtable algorithm, albeit with links performed
    through a pool and hence avoiding caml_modify (for table manipulation), even when
    hashing object keys/values. *)

include Core_hashtbl_intf.Hashtbl
