(** An attempt at providing a polymorphic hashtbl that uses the pooling work in Zero to
    avoid allocation, and utilize the optimizations we've learned when dealing with
    immediates to avoid the unnecessary caml_modify calls.

    Algorithmically, this uses the relatively standard linked-chain hashtable algorithm,
    albeit with links performed through the Zero Obj_array pool and hence avoiding
    caml_modify (for table manipulation), even when hashing object keys/values.

    Note that for critical loop int->int tables, the specialized hybrid one will likely
    still outperform. *)


include Core_hashtbl_intf.Hashtbl
