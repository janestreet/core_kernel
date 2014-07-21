(** A polymorphic hashtbl that uses [Pool] to avoid allocation.

    This uses the standard linked-chain hashtable algorithm, albeit with links performed
    through a pool and hence avoiding caml_modify (for table manipulation), even when
    hashing object keys/values.

    This implementation is worth exploring for your application if profiling demonstrates
    that garbage collection and the [caml_modify] write barrier are a significant part of
    your execution time. *)

include Core_hashtbl_intf.Hashtbl
