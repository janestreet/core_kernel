(** [Linked_queue] is a wrapper around OCaml's standard [Queue] module that follows Core
    idioms and adds some functions.  See [Queue_intf] for documentation of standard queue
    functions.  Also see [Queue], which has different performance characteristics. *)

open! Import

include Queue_intf.S

(** [transfer ~src ~dst] adds all of the elements of [src] to the end of [dst], then
    clears [src].  It is equivalent to the sequence:

    {[
      iter ~src ~f:(enqueue dst);
      clear src
    ]}

    but runs in constant time. *)
val transfer : src:'a t -> dst:'a t -> unit

(** [partial_iter t ~f] iterates through t until f returns `Stop *)
val partial_iter : 'a t -> f:('a -> [`Continue | `Stop]) -> unit
