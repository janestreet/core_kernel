(** A hash-heap is a combination of a heap and a hashtable that supports
    constant time lookup, and log(n) time removal and replacement of
    elements in addition to the normal heap operations. *)

include Hash_heap_intf.Hash_heap
